# pacotes -----------------------------------------------------------------
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(lubridate)
library(glue)
library(purrr)
library(data.table)
library(magrittr)
library(tibble)
library(tidyr)
library(MCMCpack)
library(coda)

source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------
dir_lista_votacoes   <- file.path(dir_brutos, "votacoes_camara/list")
dir_votacao          <- file.path(dir_brutos, "votacoes_camara/votos")
dir_detalhes_votacao <- file.path(dir_brutos, "votacoes_camara/detalhes")

# cria diretorios ---------------------------------------------------------
for (d in c(dir_lista_votacoes, dir_votacao, dir_detalhes_votacao)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# meses a baixar ----------------------------------------------------------

# De jan/2023 até o último mês completo. Meses incompletos não são baixados
# para o cache não congelar um mês parcial. Em modo reprodução a janela
# congela na da execução da matéria (dados até jul/2026).
ultimo_mes_completo <- if (baixar_novos_dados) {
  floor_date(Sys.Date(), "month") - months(1)
} else {
  as.Date("2026-07-01")
}
meses_inicio <- seq.Date(as.Date("2023-01-01"), ultimo_mes_completo, by = "month")

# lista de votações por mês -----------------------------------------------
for (inicio_mes in as.list(meses_inicio)) {
  fim_mes <- ceiling_date(inicio_mes, "month") - days(1)
  rotulo <- format(inicio_mes, "%Y_%m")
  path_lista <- glue("{dir_lista_votacoes}/{rotulo}.csv")
  if (file.exists(path_lista)) next
  cat("   |-", rotulo, "\n")

  # A API pagina por conta própria (hoje limita "itens" a 100, mesmo pedindo
  # mais). Seguimos o link rel="next" até acabar, robusto a qualquer limite.
  votacoes_no_mes <- list()
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/votacoes?",
                "dataInicio=", inicio_mes,
                "&dataFim=", fim_mes,
                "&ordem=ASC",
                "&ordenarPor=dataHoraRegistro",
                "&itens=100")
  repeat {
    resposta <- api_get_texto(url) %>%
      fromJSON()
    lista <- resposta$dados
    if (length(lista)) {
      votacoes_no_mes[[length(votacoes_no_mes) + 1]] <- lista
    }
    prox <- resposta$links %>%
      filter(rel == "next") %>%
      pull(href)
    if (!length(prox)) break
    url <- prox[1]
  }
  if (!length(votacoes_no_mes)) {
    # salva vazio para não re-tentar todo mês sem votações
    write.csv(data.frame(), path_lista, row.names = FALSE)
    next
  }
  votacoes_no_mes %>%
    bind_rows() %>%
    write.csv(path_lista, row.names = FALSE)
}

# votações nominais do plenário -------------------------------------------
listas <- dir_lista_votacoes %>%
  list.files(full.names = TRUE) %>%
  keep(~ file.size(.x) > 5) %>%
  map(fread, colClasses = "character") %>%
  bind_rows()

# Votações nominais têm a contagem de votos na descrição. A capitalização
# mudou ao longo do tempo ("total: 408" até 2024, "Total: 409" a partir de
# 2025); por isso o match é case-insensitive.
plen_nominais <- listas %>%
  filter(siglaOrgao == "PLEN") %>%
  filter(str_detect(descricao, regex("total:", ignore_case = TRUE)))

ids_votacoes <- plen_nominais %>%
  pull(id) %>%
  unique()

n_votacoes <- length(ids_votacoes)
stopifnot(n_votacoes > 500)  # sanidade p/ 3,5 anos de legislatura

# baixa votos e detalhes de cada votação -----------------------------------

# Algumas votações retornam 504 permanente na API (problema do lado da
# Câmara); são puladas. Um limite de sanidade aborta se forem muitas.
ids_votos_falhos <- character(0)
for (i_votacao in seq_len(n_votacoes)) {
  id <- ids_votacoes[i_votacao]
  if (i_votacao %% 100 == 0) cat("   |- Votação:", i_votacao, "/", n_votacoes, "\n")

  path_votos <- glue("{dir_votacao}/{id}.csv")
  if (!file.exists(path_votos)) {
    url <- paste0("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id,
                  "/votos")
    votos_ok <- tryCatch({
      votos <- api_get_texto(url) %>%
        fromJSON(flatten = TRUE) %>%
        extract2("dados")
      if (length(votos)) write.csv(votos, path_votos, row.names = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (!votos_ok) {
      ids_votos_falhos <- c(ids_votos_falhos, id)
      cat("      !! votos da votação", id, "indisponíveis na API, pulando\n")
    }
    Sys.sleep(0.1)
  }

  path_detalhe <- glue("{dir_detalhes_votacao}/{id}.csv")
  if (!file.exists(path_detalhe)) {
    url <- paste0("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id)
    detalhe_ok <- tryCatch({
      dados <- api_get_texto(url) %>%
        fromJSON() %>%
        extract2("dados")
      pa <- dados$proposicoesAfetadas
      if (length(pa)) pa <- rename_all(pa, paste0, "_proposicoesAfetadas")
      op <- dados$objetosPossiveis
      if (length(op)) op <- rename_all(op, paste0, "_objetosPossiveis")
      n_op <- ifelse(is.null(nrow(op)), 0, nrow(op))
      n_pa <- ifelse(is.null(nrow(pa)), 0, nrow(pa))
      detalhe <- if (n_op > 1 && n_pa > 1) bind_cols(pa) else bind_cols(op, pa)
      detalhe %>%
        mutate(id_votacao = id,
               descricao = dados$descricao %||% NA,
               data = dados$data %||% NA) %>%
        write.csv(path_detalhe, row.names = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (!detalhe_ok) cat("      !! detalhe da votação", id, "falhou, seguindo\n")
    Sys.sleep(0.1)
  }
}

stopifnot(length(ids_votos_falhos) <= 20)

# formata -----------------------------------------------------------------

# Cada deputado como votou em cada votação
votacoes <- dir_votacao %>%
  list.files(full.names = TRUE) %>%
  map(~ fread(.x, colClasses = "character") %>% mutate(path = .x)) %>%
  bind_rows() %>%
  mutate(id_votacao = str_remove_all(path, ".*/|\\.csv")) %>%
  dplyr::select(tipoVoto, deputado_.id, deputado_.nome, deputado_.siglaPartido,
         deputado_.siglaUf, id_votacao, dataRegistroVoto) %>%
  mutate(deputado_.siglaPartido = harmonizar_sigla(deputado_.siglaPartido))

# Tabela com os detalhes das votações
# (para ilustrar o texto com as votações mais ou menos divididas)
detalhes_votacoes <- dir_detalhes_votacao %>%
  list.files(full.names = TRUE) %>%
  keep(~ file.size(.x) > 5) %>%
  map(~ tryCatch(read.csv(.x, colClasses = "character"),
                 error = function(e) NULL)) %>%
  compact() %>%
  bind_rows()
write.csv(detalhes_votacoes,
          glue("{dir_resultado}/detalhes_votacao.csv"), row.names = FALSE)

# formata para analise ----------------------------------------------------

# Votação no formato binário que a função `MCMCirt1d` pede
votacoes_mtx <- votacoes %>%
  dplyr::select(deputado_.id, tipoVoto, id_votacao) %>%
  filter(tipoVoto %in% c("Sim", "Não")) %>%
  mutate(tipoVoto = as.numeric(tipoVoto == "Sim")) %>%
  distinct(deputado_.id, id_votacao, .keep_all = TRUE) %>%
  pivot_wider(names_from = id_votacao, values_from = tipoVoto) %>%
  column_to_rownames("deputado_.id") %>%
  as.matrix()

# informação dos deputados, para anexar no resultado
info_deputados <- votacoes %>%
  dplyr::select(deputado_.id, dataRegistroVoto, deputado_.nome,
         deputado_.siglaPartido) %>%
  arrange(desc(dataRegistroVoto)) %>%
  distinct(deputado_.id, .keep_all = TRUE) %>%
  drop_na() %>%
  dplyr::select(-dataRegistroVoto) %>%
  rename(id_deputado = deputado_.id,
         nome_deputado = deputado_.nome,
         partido = deputado_.siglaPartido) %>%
  mutate(id_deputado = as.character(id_deputado))

# Remove deputados com menos de 10% das votações
deputados_remover <- votacoes %>%
  filter(tipoVoto %in% c("Sim", "Não")) %>%
  mutate(n_votacoes = length(unique(id_votacao))) %>%
  group_by(deputado_.id, n_votacoes) %>%
  summarise(n_votacoes_dep = n_distinct(id_votacao), .groups = "drop") %>%
  mutate(pct = n_votacoes_dep / n_votacoes * 100) %>%
  filter(pct <= 10) %>%
  pull(deputado_.id)

votacoes_mtx <- votacoes_mtx[!rownames(votacoes_mtx) %in% deputados_remover, ]
info_deputados <- filter(info_deputados, !id_deputado %in% deputados_remover)

# análise -----------------------------------------------------------------

# Sem restrição de sinal a posterior do IRT é bimodal por reflexão (Bafumi,
# Gelman, Park & Kaplan 2005). Restringimos o sinal de dois deputados-âncora
# (o de maior participação do PT, negativo; o de maior do NOVO, positivo),
# rodamos 2 cadeias com sementes e partidas dispersas e exigimos
# convergência (Gelman-Rubin R-hat e Geweke). As duas cadeias são
# combinadas para as estimativas finais.
participacao <- rowSums(!is.na(votacoes_mtx))
escolher_ancora <- function(sigla) {
  ids <- info_deputados$id_deputado[info_deputados$partido == sigla]
  ids <- intersect(ids, rownames(votacoes_mtx))
  stopifnot(length(ids) > 0)
  ids[which.max(participacao[ids])]
}
ancora_neg <- escolher_ancora("PT")
ancora_pos <- escolher_ancora("NOVO")
restricoes <- setNames(list("-", "+"), c(ancora_neg, ancora_pos))

# Cadeias longas com thinning: com cadeias curtas o amostrador não converge
# nesta matriz (R-hat máximo de 1,33 numa tentativa com burnin 5.000)
rodar_cadeia <- function(seed_cadeia) {
  set.seed(seed_cadeia)
  theta_ini <- rnorm(nrow(votacoes_mtx), 0, 1.5)
  theta_ini[rownames(votacoes_mtx) == ancora_neg] <- -2
  theta_ini[rownames(votacoes_mtx) == ancora_pos] <- 2
  MCMCirt1d(votacoes_mtx, theta.constraints = restricoes,
            theta.start = theta_ini,
            burnin = 50000, mcmc = 50000, thin = 10, verbose = 25000,
            seed = seed_cadeia)
}
cadeia_1 <- rodar_cadeia(12345)
cadeia_2 <- rodar_cadeia(54321)

# diagnósticos de convergência
gelman <- gelman.diag(mcmc.list(cadeia_1, cadeia_2), multivariate = FALSE)
rhat <- gelman$psrf[, "Point est."]
geweke_z <- geweke.diag(cadeia_1)$z
pct_geweke_ruim <- mean(abs(geweke_z) > 2, na.rm = TRUE) * 100

# A locação e a escala da dimensão são identificadas só pelos priors e podem
# derivar lentamente dentro da cadeia (deriva comum a todos os thetas), o que
# estoura o Geweke bruto sem afetar as posições relativas, que são o que
# usamos (medianas por partido numa escala relativa). O Geweke padronizado
# (cada draw centrado e escalado na própria iteração) testa a estacionaridade
# do que de fato interessa.
padronizar_draws <- function(cadeia) {
  m <- as.matrix(cadeia)
  t(apply(m, 1, function(x) (x - mean(x)) / sd(x)))
}
geweke_std_z <- geweke.diag(mcmc(padronizar_draws(cadeia_1)))$z
pct_geweke_std_ruim <- mean(abs(geweke_std_z) > 2, na.rm = TRUE) * 100

tibble(diagnostico = c("rhat_max", "rhat_acima_1.1", "n_thetas",
                       "pct_geweke_z_maior_2",
                       "pct_geweke_z_maior_2_padronizado",
                       "cor_medias_entre_cadeias"),
      valor = c(round(max(rhat, na.rm = TRUE), 4),
                sum(rhat > 1.1, na.rm = TRUE),
                length(rhat),
                round(pct_geweke_ruim, 2),
                round(pct_geweke_std_ruim, 2),
                round(cor(colMeans(as.matrix(cadeia_1)),
                          colMeans(as.matrix(cadeia_2))), 6))) %>%
  write.csv(glue("{dir_processado}/diagnostico_irt_convergencia.csv"),
            row.names = FALSE)
stopifnot(max(rhat, na.rm = TRUE) < 1.2)

# amostras salvas para auditoria (thin 10 já aplicado; ~45 MB)
saveRDS(list(cadeia_1 = as.matrix(cadeia_1), cadeia_2 = as.matrix(cadeia_2)),
        glue("{dir_resultado}/irt_theta_amostras.rds"))

# Posição dos deputados (cadeias combinadas)
theta_amostras <- rbind(as.matrix(cadeia_1), as.matrix(cadeia_2))
ponto_ideal_deputados <- tibble(
    id_deputado = str_remove(colnames(theta_amostras), "theta\\."),
    Mean = colMeans(theta_amostras),
    SD = apply(theta_amostras, 2, sd)
  ) %>%
  left_join(info_deputados, by = "id_deputado") %>%
  arrange(Mean) %>%
  mutate(escala_ranking = scales::rescale(Mean, to = c(1, 100)))

# Posição dos partidos
ponto_ideal_partidos <- ponto_ideal_deputados %>%
  filter(!is.na(partido), partido != "", partido != "S.PART.") %>%
  group_by(partido) %>%
  summarise(ponto_ideal = median(Mean), n_deputados = n(), .groups = "drop") %>%
  arrange(ponto_ideal)

# Tabulação com a quantidade de votos para cada lado em cada um dos partidos
votacoes_por_partido <- votacoes %>%
  group_by(id_votacao, deputado_.siglaPartido, tipoVoto) %>%
  summarise(n_votos = n(), .groups = "drop") %>%
  filter(tipoVoto %in% c("Sim", "Não")) %>%
  set_colnames(c("id_votacao", "partido", "voto", "n_votos"))

# salva os resultados -----------------------------------------------------
write.csv(ponto_ideal_partidos,
          glue("{dir_resultado}/d1_ponto_ideal_partidos.csv"), row.names = FALSE)
write.csv(ponto_ideal_deputados,
          glue("{dir_resultado}/d1_ponto_ideal_deputados.csv"), row.names = FALSE)
write.csv(votacoes_por_partido,
          glue("{dir_resultado}/votacoes_por_partido.csv"), row.names = FALSE)
