# pacotes -----------------------------------------------------------------
library(dplyr)
library(glue)
library(rvest)
library(stringr)
library(purrr)
library(magrittr)
library(tibble)
library(tidyr)
library(ca)
library(data.table)

source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------
dir_frentes <- file.path(dir_brutos, "frentes")

# cria diretorio ----------------------------------------------------------
if (!dir.exists(dir_frentes)) dir.create(dir_frentes, recursive = TRUE)

# helper com retry e backoff para read_html
ler_html_retry <- function(url, tentativas = 5) {
  for (i in seq_len(tentativas)) {
    html <- tryCatch(read_html(url), error = function(e) NULL)
    if (!is.null(html)) return(html)
    Sys.sleep(3 * i)
  }
  stop(glue("Falha ao ler {url}"))
}

# baixa dados das frentes -------------------------------------------------

# O cache em dados/brutos/frentes/ congela a composição de cada frente no
# primeiro download; para atualizar uma frente já baixada, apague o CSV
# correspondente. Em modo reprodução nada é consultado na Câmara.
if (baixar_novos_dados) {
  html_main <- ler_html_retry(
    "https://www.camara.leg.br/internet/deputado/frentes.asp?leg=57")

  id_frentes <- html_main %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    keep(str_detect, "frenteDetalhe") %>%
    str_remove(".*=") %>%
    unique()

  n_frentes <- length(id_frentes)
  stopifnot(n_frentes > 100)  # sanidade: leg 57 tem centenas de frentes

  # Vai para a página de cada frente e baixa a lista de parlamentares
  for (i_frente in seq_len(n_frentes)) {
    id <- id_frentes[i_frente]
    path_frente <- glue("{dir_frentes}/{id}.csv")
    if (file.exists(path_frente)) next
    cat("   |- Frente:", i_frente, "/", n_frentes, "\n")
    url_frente <- glue("https://www.camara.leg.br/internet/deputado/",
                       "frenteDetalhe.asp?id={id}")
    html <- ler_html_retry(url_frente)
    nome_frente <- html %>%
      html_node("h3") %>%
      html_text()
    tabelas <- html %>% html_table()
    if (!length(tabelas)) next
    tabelas %>%
      extract2(1) %>%
      mutate(frente_nome = nome_frente, frente_id = id) %>%
      write.csv(path_frente, row.names = FALSE)
    Sys.sleep(0.2)
  }
} else {
  stopifnot(length(list.files(dir_frentes, pattern = "\\.csv$")) > 100)
}

# junta e formata ---------------------------------------------------------

# No fim vamos ter uma tabela de parlamentares e seus partidos e das
# frentes de que participam
frentes <- dir_frentes %>%
  list.files(full.names = TRUE) %>%
  map(fread, colClasses = "character") %>%
  bind_rows()

# 1. Remove parlamentar sem partido e a linha que diz o total
# 2. Tabula a quantidade de parlamentares por partido em cada frente
partidos_por_frente <- frentes %>%
  filter(!str_detect(Partido, "Total: \\d+")) %>%
  filter(Partido != "S.PART.") %>%
  mutate(Partido = harmonizar_sigla(Partido)) %>%
  group_by(frente_id, Partido, frente_nome) %>%
  summarise(n = n(), .groups = "drop")

# Faz a matriz com a frequência de partidos nas frentes
ca_data <- partidos_por_frente %>%
  select(-frente_nome) %>%
  pivot_wider(names_from = frente_id, values_from = n, values_fill = 0) %>%
  column_to_rownames("Partido") %>%
  as.matrix()

# Análise
ca_result <- ca(ca_data)

# Resultado
resultado_d1_linhas <- ca_result %>%
  extract2("rowcoord") %>%
  as.data.frame() %>%
  rownames_to_column("partido") %>%
  select(partido, Dim1) %>%
  arrange(Dim1)

resultado_d1_colunas <- ca_result %>%
  extract2("colcoord") %>%
  as.data.frame() %>%
  rownames_to_column("frente_id") %>%
  select(frente_id, Dim1) %>%
  arrange(Dim1) %>%
  left_join(distinct(frentes, frente_nome, frente_id), by = "frente_id") %>%
  select(frente_id, frente_nome, Dim1)

# Salva
write.csv(resultado_d1_linhas,
          glue("{dir_resultado}/d1_frentes_politicos.csv"), row.names = FALSE)
write.csv(resultado_d1_colunas,
          glue("{dir_resultado}/d1_frentes.csv"), row.names = FALSE)
write.csv(partidos_por_frente,
          glue("{dir_resultado}/partidos_por_frente.csv"), row.names = FALSE)

# bootstrap ---------------------------------------------------------------

# Incerteza da dimensão: bootstrap não paramétrico. A unidade de
# reamostragem é o deputado: ao sortear um deputado, entram todas as
# frentes de que ele participa, o que preserva a correlação entre elas.
# Mesma lógica do script 01: o sinal fica para depois, em 06_juntando_tudo.R.
B_BOOT <- 1000
set.seed(20260705)
membros <- frentes %>%
  filter(!str_detect(Partido, "Total: \\d+"), Partido != "S.PART.") %>%
  mutate(Partido = harmonizar_sigla(Partido)) %>%
  select(dep_id = `Deputado Signatário`, Partido, frente_id) %>%
  as.data.frame()

partidos_base_fr <- resultado_d1_linhas$partido
deputados_unicos <- unique(membros$dep_id)
n_dep <- length(deputados_unicos)
linhas_por_dep <- split(seq_len(nrow(membros)), membros$dep_id)

boot_frentes <- matrix(NA_real_, nrow = length(partidos_base_fr), ncol = B_BOOT,
                       dimnames = list(partidos_base_fr, NULL))

for (b in seq_len(B_BOOT)) {
  dep_b <- sample(deputados_unicos, n_dep, replace = TRUE)
  linhas_b <- unlist(linhas_por_dep[dep_b], use.names = FALSE)
  amostra_b <- membros[linhas_b, ]

  ca_b <- tryCatch({
    m_b <- amostra_b %>%
      count(Partido, frente_id, name = "n") %>%
      pivot_wider(names_from = frente_id, values_from = n, values_fill = 0) %>%
      column_to_rownames("Partido") %>%
      as.matrix()
    if (nrow(m_b) < 3) NULL else ca(m_b)
  }, error = function(e) NULL)
  if (is.null(ca_b)) next
  dim1_b <- ca_b$rowcoord[, "Dim1"]
  comuns <- intersect(names(dim1_b), partidos_base_fr)
  boot_frentes[comuns, b] <- dim1_b[comuns]
}

write.csv(as.data.frame(boot_frentes) %>% rownames_to_column("partido"),
          glue("{dir_resultado}/d1_frentes_politicos_boot.csv"), row.names = FALSE)
