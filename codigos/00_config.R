# Configuração compartilhada por todos os scripts.
# Carregar com: source("codigos/00_config.R")

# pacotes -----------------------------------------------------------------
library(httr)
library(glue)

options(timeout = 3600)

# modo de execução --------------------------------------------------------

# Com TRUE, a análise é atualizada baixando os dados mais recentes do TSE e
# da Câmara (as janelas temporais avançam sozinhas). Com FALSE, a análise
# reproduz a matéria publicada em agosto de 2026 usando os dados congelados
# da época, que precisam ser baixados antes (ver README): nenhum download é
# feito e as janelas temporais ficam travadas nas da publicação.
baixar_novos_dados <- TRUE

# diretórios --------------------------------------------------------------
dir_brutos     <- "dados/brutos/"
dir_interim    <- "dados/interim/"
dir_resultado  <- "dados/interim/resultados/"
dir_processado <- "dados/processado/"

for (d in c(dir_brutos, dir_interim, dir_resultado, dir_processado)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# harmonização de siglas --------------------------------------------------

# Fusões, incorporações e renomeações homologadas pelo TSE até jul/2026.
# Verificado em jul/2026: a fusão PSDB+Podemos não se concretizou (desistência
# em 2025, sem homologação). Solidariedade-PRD e União-PP são federações, não
# fusões: as siglas permanecem separadas.
recode_siglas <- c(
  "DEM"      = "UNIÃO",
  "PSL"      = "UNIÃO",
  "PATRIOTA" = "PRD",
  "PATRI"    = "PRD",
  "PTB"      = "PRD",
  "PMN"      = "MOBILIZA",
  "PTC"      = "AGIR",
  "PRN"      = "AGIR",
  "PSC"      = "PODE",
  "PODEMOS"  = "PODE",
  "PROS"     = "SOLIDARIEDADE",
  "PCdoB"    = "PC do B",
  "PCDOB"    = "PC do B",
  # PMB renomeado para Democrata (TSE, dez/2025); dados históricos usam PMB.
  # Não confundir com o antigo DEM (Democratas), que virou UNIÃO.
  "DEMOCRATA" = "PMB",
  # MISSÃO é partido novo (registro no TSE em nov/2025): não recodificar.
  "Republicanos" = "REPUBLICANOS",
  "Solidariedade" = "SOLIDARIEDADE",
  "Avante"   = "AVANTE",
  "Podemos"  = "PODE",
  "Cidadania" = "CIDADANIA",
  "União"    = "UNIÃO",
  "Novo"     = "NOVO",
  "Rede"     = "REDE"
)

harmonizar_sigla <- function(sigla) {
  sigla <- trimws(sigla)
  unname(ifelse(sigla %in% names(recode_siglas), recode_siglas[sigla], sigla))
}

# disponibilidade de arquivos do TSE --------------------------------------

# Usado para incluir automaticamente os dados de 2026 quando forem publicados.
# Em modo reprodução não consulta a rede: os arquivos de 2026 fazem parte do
# pacote de dados congelados e a janela de 4 anos fica sempre ativada.
tse_arquivo_disponivel <- function(url) {
  if (!baixar_novos_dados) return(TRUE)
  resp <- tryCatch(HEAD(url, timeout(30)), error = function(e) NULL)
  if (is.null(resp)) return(FALSE)
  status_code(resp) == 200
}

# dados TSE 2026: liberação e frescor -------------------------------------

# O TSE publica consulta_cand/consulta_coligacao de 2026 de forma incremental
# durante o registro de candidaturas (prazo legal: 15/ago/2026). Duas
# salvaguardas:
#  1. 2026 só entra na análise a partir de data_liberacao_tse_2026, quando o
#     registro de candidaturas já fechou e os arquivos cobrem todas as UFs;
#  2. enquanto o TSE ainda atualiza os arquivos (até data_congela_tse_2026),
#     o cache local de 2026 é apagado a cada execução para re-download, de
#     modo que substituições de candidatos e julgamentos até novembro entram
#     sozinhos a cada re-execução.
data_liberacao_tse_2026 <- as.Date("2026-08-17")
data_congela_tse_2026   <- as.Date("2026-11-01")

tse_2026_liberado <- function() {
  !baixar_novos_dados || Sys.Date() >= data_liberacao_tse_2026
}

# em modo reprodução o cache nunca é apagado: os arquivos congelados são
# exatamente os usados na matéria
tse_2026_em_atualizacao <- function() {
  baixar_novos_dados && Sys.Date() <= data_congela_tse_2026
}

refresh_tse_2026 <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths)) file.remove(paths)
  invisible(NULL)
}

url_consulta_cand <- function(ano) {
  glue("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/",
       "consulta_cand_{ano}.zip")
}

url_consulta_coligacao <- function(ano) {
  glue("https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_coligacao/",
       "consulta_coligacao_{ano}.zip")
}

# download com retry ------------------------------------------------------
baixar_com_retry <- function(url, destino, tentativas = 5) {
  if (file.exists(destino) && file.size(destino) > 0) {
    return(invisible(TRUE))
  }
  if (!baixar_novos_dados) {
    stop(glue("Modo reprodução (baixar_novos_dados = FALSE) e arquivo ausente: ",
              "{destino}. Baixe os dados congelados da matéria (ver README)."))
  }
  for (i in seq_len(tentativas)) {
    ok <- tryCatch({
      download.file(url, destino, mode = "wb", quiet = TRUE)
      file.exists(destino) && file.size(destino) > 0
    }, error = function(e) {
      cat("      !! tentativa", i, "falhou:", conditionMessage(e), "\n")
      FALSE
    })
    if (ok) return(invisible(TRUE))
    Sys.sleep(5 * i)
  }
  stop(glue("Download falhou após {tentativas} tentativas: {url}"))
}

# GET de API com retry (Câmara) -------------------------------------------
api_get_texto <- function(url, tentativas = 10) {
  if (!baixar_novos_dados) {
    stop(glue("Modo reprodução (baixar_novos_dados = FALSE): a API da Câmara ",
              "não deveria ser consultada. Está faltando arquivo no cache de ",
              "dados congelados? (ver README) URL: {url}"))
  }
  for (i in seq_len(tentativas)) {
    conteudo <- tryCatch({
      resp <- GET(url, timeout(60))
      txt <- content(resp, "text", encoding = "UTF-8")
      if (identical(txt, "upstream request timeout")) NULL
      else if (status_code(resp) >= 500) NULL
      else txt
    }, error = function(e) NULL)
    if (!is.null(conteudo)) return(conteudo)
    Sys.sleep(2 * i)
  }
  stop(glue("API falhou após {tentativas} tentativas: {url}"))
}

# diagonal das matrizes quadradas de afinidade (CA) -----------------------

# Um partido não se alia consigo mesmo, então a diagonal das matrizes
# partido x partido (migração, coligações) não tem valor observado. Ela é
# reconstituída por quasi-independência: iterada até a expectativa de
# independência (r_i * c_i / n), de modo que contribui zero para o
# qui-quadrado (van der Heijden & de Leeuw 1988; Greenacre 2000). Substitui
# o preenchimento usado na versão de 2024 (diagonal = colSums), que dobrava
# a massa de cada partido. Impacto verificado antes da troca: zero mudanças
# de faixa; correlação de 0,9991 no composto.
diag_quasi <- function(m, tol = 1e-8, max_iter = 10000) {
  stopifnot(nrow(m) == ncol(m))
  m2 <- m
  diag(m2) <- 0
  d <- rep(0, nrow(m2))
  iter_usadas <- NA_integer_
  for (i in seq_len(max_iter)) {
    mm <- m2
    diag(mm) <- d
    r <- rowSums(mm)
    n <- sum(mm)
    d_novo <- r^2 / n
    if (max(abs(d_novo - d)) < tol) {
      iter_usadas <- i
      d <- d_novo
      break
    }
    d <- d_novo
  }
  stopifnot(!is.na(iter_usadas))
  diag(m2) <- d
  m2
}

# matriz simétrica partido x partido a partir da tabela de pares ----------

# Espelha a construção dos scripts 01/03 (diagonal zero, pronta para
# diag_quasi). Requer dplyr/tidyr/tibble carregados pelo script chamador.
matriz_pares <- function(tbl) {
  names(tbl) <- c("p1", "p2", "n")
  tbl <- dplyr::filter(tbl, p1 != p2)
  m <- dplyr::bind_rows(tbl, tibble::tibble(p1 = tbl$p2, p2 = tbl$p1,
                                            n = tbl$n)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = p2, values_from = n, values_fill = 0) %>%
    tibble::column_to_rownames("p1") %>%
    as.matrix()
  m <- m[rownames(m), rownames(m)]
  diag(m) <- 0
  m
}
