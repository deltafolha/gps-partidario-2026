# pacotes -----------------------------------------------------------------
library(glue)
library(stringr)
library(dplyr)
library(purrr)
library(data.table)
library(magrittr)
library(tibble)
library(tidyr)
library(ca)

source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------
dir_coligacoes <- file.path(dir_brutos, "coligacoes")
dir_candidatos <- file.path(dir_brutos, "candidatos")

# cria diretorio ----------------------------------------------------------
if (!dir.exists(dir_coligacoes)) dir.create(dir_coligacoes, recursive = TRUE)

# anos e cargos -----------------------------------------------------------

# Janela móvel de 4 anos (1 municipal + 1 geral): municipais de 2024 mais as
# majoritárias de 2026, que entram automaticamente quando o TSE publica
# consulta_coligacao_2026
cargos_por_ano <- list(
  "2024" = "PREFEITO",
  "2026" = c("PRESIDENTE", "GOVERNADOR", "SENADOR")
)

anos <- 2024
if (tse_2026_liberado() && tse_arquivo_disponivel(url_consulta_coligacao(2026))) {
  anos <- c(anos, 2026)
}

# baixa e lê ---------------------------------------------------------------

# Os pares dentro de uma federação contam como aliança: membros de federação
# aparecem nos arquivos do TSE como linhas próprias de partido no mesmo
# SQ_COLIGACAO, então a contagem já acontece sem tratamento especial.
# Depende dos arquivos de candidatos baixados pelo script 01.
ler_coligacoes_ano <- function(ano) {
  dir_ano <- file.path(dir_coligacoes, ano)
  if (!dir.exists(dir_ano)) dir.create(dir_ano, recursive = TRUE)
  path_zip <- file.path(dir_ano, glue("consulta_coligacao_{ano}.zip"))
  path_brasil <- file.path(dir_ano, glue("consulta_coligacao_{ano}_BRASIL.csv"))
  # o arquivo de 2026 cresce durante o registro: não congelar o cache
  if (ano == 2026 && tse_2026_em_atualizacao()) {
    refresh_tse_2026(c(path_zip,
                       list.files(dir_ano, pattern = "BRASIL\\.csv$",
                                  full.names = TRUE, recursive = TRUE)))
  }
  if (!file.exists(path_brasil)) {
    cat("   |- Baixando consulta_coligacao_", ano, "\n", sep = "")
    baixar_com_retry(url_consulta_coligacao(ano), path_zip)
    unzip(path_zip, exdir = dir_ano)
  }
  # coligações que de fato tiveram candidatos (via arquivo de candidatos do 01)
  path_cand <- file.path(dir_candidatos, ano, glue("consulta_cand_{ano}_BRASIL.csv"))
  stopifnot(file.exists(path_cand))
  coligacoes_com_candidatos <- path_cand %>%
    fread(select = "SQ_COLIGACAO", colClasses = "character") %>%
    distinct()

  colig <- list.files(dir_ano, full.names = TRUE, recursive = TRUE,
                      pattern = "BRASIL.csv") %>%
    keep(str_detect, "coligacao") %>%
    map(fread, colClasses = "character", encoding = "Latin-1") %>%
    bind_rows() %>%
    filter(DS_CARGO %in% cargos_por_ano[[as.character(ano)]]) %>%
    inner_join(coligacoes_com_candidatos, by = "SQ_COLIGACAO")

  colig %>%
    mutate(SG_PARTIDO = harmonizar_sigla(SG_PARTIDO)) %>%
    # coligações repetidas com sequenciais distintos: ID = partidos ordenados + UE
    group_by(SQ_COLIGACAO) %>%
    mutate(id_fix = paste(sort(SG_PARTIDO), collapse = "_")) %>%
    ungroup() %>%
    mutate(id_coligacao = paste0(ano, "_", id_fix, "_", SG_UE)) %>%
    select(id_coligacao, SG_PARTIDO) %>%
    distinct()
}

coligacao <- anos %>%
  map(ler_coligacoes_ano) %>%
  bind_rows()

# pares de partidos por coligação -----------------------------------------
tbl_coligacoes <- coligacao %>%
  left_join(coligacao, by = "id_coligacao", relationship = "many-to-many") %>%
  filter(SG_PARTIDO.x < SG_PARTIDO.y) %>%
  group_by(SG_PARTIDO.x, SG_PARTIDO.y) %>%
  summarise(n_coligacoes = n(), .groups = "drop") %>%
  rename(partido_1 = SG_PARTIDO.x, partido_2 = SG_PARTIDO.y)

# análise de correspondência ----------------------------------------------

# Faz a matriz: inverte partido 1 e 2 e junta, para termos os dois
# triângulos da matriz
ca_data <- tbl_coligacoes %>%
  bind_rows(set_colnames(tbl_coligacoes,
                         c("partido_2", "partido_1", "n_coligacoes"))) %>%
  pivot_wider(names_from = partido_2, values_from = n_coligacoes,
              values_fill = 0) %>%
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
          glue("{dir_resultado}/d1_coligacoes.csv"), row.names = FALSE)
write.csv(tbl_coligacoes,
          glue("{dir_resultado}/tbl_coligacoes.csv"), row.names = FALSE)
write.csv(ca_result$rowcoord,
          glue("{dir_resultado}/ca_result_coligacoes.csv"))
# tabela no nível da chapa, insumo do script 07
write.csv(coligacao,
          glue("{dir_resultado}/tbl_coligacoes_chapas.csv"), row.names = FALSE)

# bootstrap ---------------------------------------------------------------

# Incerteza da dimensão: bootstrap não paramétrico. A unidade de
# reamostragem é a chapa (id_coligacao): ao sortear uma chapa com reposição,
# entram todos os partidos dela juntos, o que preserva quem se coligou com
# quem. Cada sorteio recebe um rótulo `draw` próprio (mesmo quando a mesma
# chapa é sorteada mais de uma vez) para o autojoin de pares não multiplicar
# o par errado. O sinal fica para depois, em 06_juntando_tudo.R.
B_BOOT <- 1000
set.seed(20260705)
coligacao_base <- as.data.frame(coligacao)
chapas_unicas <- unique(coligacao_base$id_coligacao)
n_chapas <- length(chapas_unicas)
linhas_por_chapa <- split(seq_len(nrow(coligacao_base)), coligacao_base$id_coligacao)

partidos_base_col <- resultado_d1$partido
boot_coligacoes <- matrix(NA_real_, nrow = length(partidos_base_col), ncol = B_BOOT,
                          dimnames = list(partidos_base_col, NULL))

for (b in seq_len(B_BOOT)) {
  chapas_b <- sample(chapas_unicas, n_chapas, replace = TRUE)
  linhas_b <- linhas_por_chapa[chapas_b]
  draw_b <- rep(seq_len(n_chapas), lengths(linhas_b))
  amostra_b <- coligacao_base[unlist(linhas_b, use.names = FALSE), ]
  amostra_b$draw <- draw_b

  ca_b <- tryCatch({
    tbl_b <- amostra_b %>%
      left_join(amostra_b, by = "draw", relationship = "many-to-many") %>%
      filter(SG_PARTIDO.x < SG_PARTIDO.y) %>%
      group_by(SG_PARTIDO.x, SG_PARTIDO.y) %>%
      summarise(n_coligacoes = n(), .groups = "drop") %>%
      rename(partido_1 = SG_PARTIDO.x, partido_2 = SG_PARTIDO.y)
    m_b <- diag_quasi(matriz_pares(tbl_b))
    if (nrow(m_b) < 3) NULL else ca(m_b)
  }, error = function(e) NULL)
  if (is.null(ca_b)) next
  dim1_b <- ca_b$rowcoord[, "Dim1"]
  comuns <- intersect(names(dim1_b), partidos_base_col)
  boot_coligacoes[comuns, b] <- dim1_b[comuns]
}

write.csv(as.data.frame(boot_coligacoes) %>% rownames_to_column("partido"),
          glue("{dir_resultado}/d1_coligacoes_boot.csv"), row.names = FALSE)
