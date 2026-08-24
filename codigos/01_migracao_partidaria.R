# pacotes -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(purrr)
library(data.table)
library(tidyr)
library(tibble)
library(ca)
library(magrittr)
library(glue)

source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------
dir_candidatos <- file.path(dir_brutos, "candidatos")

# anos da análise ---------------------------------------------------------

# Janela móvel de 4 anos (1 eleição municipal + 1 geral): com
# consulta_cand_2026 publicado e liberado, a janela desliza para
# {2022, 2024, 2026} e 2020 sai (pares possíveis: 2022->2024, 2024->2026 e
# o par direto 2022->2026 de quem pulou 2024). Até lá, vale a janela
# anterior, {2020, 2022, 2024}.
if (tse_2026_liberado() && tse_arquivo_disponivel(url_consulta_cand(2026))) {
  anos <- c(2022, 2024, 2026)
} else {
  anos <- c(2020, 2022, 2024)
}

# baixa dados -------------------------------------------------------------
for (ano in anos) {
  dir_ano <- file.path(dir_candidatos, ano)
  if (!dir.exists(dir_ano)) dir.create(dir_ano, recursive = TRUE)
  path_zip <- file.path(dir_ano, glue("consulta_cand_{ano}.zip"))
  path_brasil <- file.path(dir_ano, glue("consulta_cand_{ano}_BRASIL.csv"))
  # o arquivo de 2026 cresce durante o registro: não congelar o cache
  if (ano == 2026 && tse_2026_em_atualizacao()) {
    refresh_tse_2026(c(path_zip, path_brasil))
  }
  if (!file.exists(path_brasil)) {
    cat("   |- Baixando consulta_cand_", ano, "\n", sep = "")
    baixar_com_retry(url_consulta_cand(ano), path_zip)
    unzip(path_zip, files = glue("consulta_cand_{ano}_BRASIL.csv"),
          exdir = dir_ano)
  }
}

# lê e formata os dados ---------------------------------------------------

# ID único entre diferentes eleições:
# se tiver o CPF usamos ele; se não, o título de eleitor; se não tiver o
# título, o nome completo, a data de nascimento e a UF de nascimento.
# CPF ausente é preenchido via mapa título -> CPF, para ligar registros
# entre eleições.
candidatos <- anos %>%
  map(function(ano) {
    path <- file.path(dir_candidatos, ano, glue("consulta_cand_{ano}_BRASIL.csv"))
    fread(path, colClasses = "character",
          select = c("DT_NASCIMENTO", "NR_CPF_CANDIDATO", "ANO_ELEICAO",
                     "NR_TITULO_ELEITORAL_CANDIDATO", "SG_UF_NASCIMENTO",
                     "NM_CANDIDATO", "SG_PARTIDO"),
          encoding = "Latin-1")
  }) %>%
  bind_rows() %>%
  mutate(
    cpf    = ifelse(NR_CPF_CANDIDATO %in% c("-4", "-1", "", "#NULO#"),
                    NA_character_, NR_CPF_CANDIDATO),
    titulo = ifelse(NR_TITULO_ELEITORAL_CANDIDATO %in% c("-4", "-1", "", "#NULO#"),
                    NA_character_, NR_TITULO_ELEITORAL_CANDIDATO)
  )

# preenche CPF ausente via mapa título -> CPF (liga registros entre eleições)
mapa_titulo_cpf <- candidatos %>%
  filter(!is.na(cpf), !is.na(titulo)) %>%
  distinct(titulo, cpf) %>%
  distinct(titulo, .keep_all = TRUE)

candidatos <- candidatos %>%
  left_join(rename(mapa_titulo_cpf, cpf_mapa = cpf), by = "titulo") %>%
  mutate(cpf = coalesce(cpf, cpf_mapa)) %>%
  mutate(ID = case_when(
    !is.na(cpf)    ~ paste0("cpf_", cpf),
    !is.na(titulo) ~ paste0("tit_", titulo),
    TRUE           ~ paste0("nom_", NM_CANDIDATO, "_", DT_NASCIMENTO, "_",
                            SG_UF_NASCIMENTO)
  )) %>%
  mutate(SG_PARTIDO = harmonizar_sigla(SG_PARTIDO)) %>%
  # uma linha por candidato em cada eleição
  distinct(ANO_ELEICAO, ID, .keep_all = TRUE) %>%
  select(ANO_ELEICAO, ID, SG_PARTIDO)

# tabulação ---------------------------------------------------------------

# Gera uma tabela com uma linha por candidato com o partido anterior e
# posterior
antes_e_depois <- candidatos %>%
  arrange(ID, ANO_ELEICAO) %>%
  group_by(ID) %>%
  mutate(
    ano_1 = lag(ANO_ELEICAO),
    ano_2 = ANO_ELEICAO,
    partido_antes = lag(SG_PARTIDO),
    partido_depois = SG_PARTIDO
  ) %>%
  ungroup() %>%
  drop_na(ano_1, partido_antes)

# Faz a tabulação antes e depois. Como não importa a direção (indo ou
# vindo), cria um ID para cada par que é o mesmo independente da direção
mudancas_tbl <- antes_e_depois %>%
  group_by(partido_antes, partido_depois) %>%
  summarise(n = n(), .groups = "drop") %>%
  rowwise() %>%
  mutate(id_par = paste(sort(c(partido_antes, partido_depois)),
                        collapse = "/")) %>%
  ungroup() %>%
  group_by(id_par) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  separate(id_par, into = c("partido_1", "partido_2"), sep = "/")

# filtro de partidos ------------------------------------------------------

# Remove partidos com menos de 10 migrações
partidos_remover <- mudancas_tbl %>%
  filter(partido_1 != partido_2) %>%
  pivot_longer(cols = c(partido_1, partido_2)) %>%
  group_by(value) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  filter(n <= 10) %>%
  pull(value)

mudancas_tbl <- mudancas_tbl %>%
  filter(!partido_1 %in% partidos_remover,
         !partido_2 %in% partidos_remover)

# análise de correspondência ----------------------------------------------

# Faz a matriz: inverte partido 1 e 2 e junta, para termos os dois
# triângulos da matriz
ca_data <- mudancas_tbl %>%
  bind_rows(set_colnames(mudancas_tbl, c("partido_2", "partido_1", "n"))) %>%
  filter(partido_1 != partido_2) %>%
  distinct() %>%
  pivot_wider(names_from = partido_2, values_from = n, values_fill = 0) %>%
  column_to_rownames("partido_1") %>%
  as.matrix()

# Faz a matriz ser simétrica na ordem das linhas e colunas
ca_data <- ca_data[rownames(ca_data), rownames(ca_data)]

# A diagonal (o partido com ele mesmo) é reconstituída por
# quasi-independência (ver 00_config.R)
ca_data <- diag_quasi(ca_data)

# Faz a análise
ca_result <- ca(ca_data)

# salva o resultado -------------------------------------------------------
resultado_d1 <- ca_result %>%
  extract2("rowcoord") %>%
  as.data.frame() %>%
  rownames_to_column("partido") %>%
  select(partido, Dim1) %>%
  arrange(Dim1)

write.csv(resultado_d1,
          glue("{dir_resultado}/d1_migracao_partidaria.csv"), row.names = FALSE)
write.csv(mudancas_tbl,
          glue("{dir_resultado}/tbl_migracao_partidaria.csv"), row.names = FALSE)
write.csv(ca_result$rowcoord,
          glue("{dir_resultado}/ca_result_migracao_partidaria.csv"))


# bootstrap ---------------------------------------------------------------

# Incerteza da dimensão: bootstrap não paramétrico. A unidade de
# reamostragem é o par candidato/eleições-consecutivas, a mesma linha que
# antes_e_depois tabula. Reamostra com reposição, reconta, refaz a CA.
# O alinhamento de sinal (PT no lado negativo) fica centralizado em
# 06_juntando_tudo.R: aqui o Dim1 exportado é bruto.
B_BOOT <- 1000
set.seed(20260705)
partidos_base_mig <- resultado_d1$partido
n_pares <- nrow(antes_e_depois)
pares_base <- antes_e_depois[, c("partido_antes", "partido_depois")]

boot_migracao <- matrix(NA_real_, nrow = length(partidos_base_mig), ncol = B_BOOT,
                        dimnames = list(partidos_base_mig, NULL))

for (b in seq_len(B_BOOT)) {
  amostra_b <- pares_base[sample.int(n_pares, n_pares, replace = TRUE), ]
  mud_b <- amostra_b %>%
    count(partido_antes, partido_depois, name = "n") %>%
    rowwise() %>%
    mutate(id_par = paste(sort(c(partido_antes, partido_depois)), collapse = "/")) %>%
    ungroup() %>%
    group_by(id_par) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    separate(id_par, into = c("partido_1", "partido_2"), sep = "/")

  remover_b <- mud_b %>%
    filter(partido_1 != partido_2) %>%
    pivot_longer(cols = c(partido_1, partido_2)) %>%
    group_by(value) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n <= 10) %>%
    pull(value)
  mud_b <- mud_b %>% filter(!partido_1 %in% remover_b, !partido_2 %in% remover_b)

  ca_b <- tryCatch({
    m_b <- diag_quasi(matriz_pares(mud_b))
    if (nrow(m_b) < 3) NULL else ca(m_b)
  }, error = function(e) NULL)
  if (is.null(ca_b)) next
  dim1_b <- ca_b$rowcoord[, "Dim1"]
  comuns <- intersect(names(dim1_b), partidos_base_mig)
  boot_migracao[comuns, b] <- dim1_b[comuns]
}

write.csv(as.data.frame(boot_migracao) %>% rownames_to_column("partido"),
          glue("{dir_resultado}/d1_migracao_partidaria_boot.csv"), row.names = FALSE)
