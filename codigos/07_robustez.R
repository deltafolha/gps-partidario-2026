# Suíte de auditoria: testa se as escolhas metodológicas do pipeline mudam o
# resultado. Não altera nenhuma saída publicada: lê o que os scripts 01 a 06
# produziram e gera os dados/processado/diagnostico_auditoria_*.csv.
# Rodar depois do pipeline principal (01..06).
#
# Seções:
#  A. Diagonal das CAs quadradas (migração, coligações): o tratamento do
#     pipeline (quasi-independência) vs o da versão de 2024 (colSums).
#  B. Sensibilidade do corte de <= 10 migrações: {5, 10, 15, 20}.
#  C. IRT: sensibilidade do corte de participação {5%, 10%, 20%} e obstrução
#     recodificada como "não". Cadeias curtas ancoradas: sensibilidade de
#     ordenação, não estimativa final.
#  D. Composto restrito aos partidos com 5 dimensões + fator latente 1D
#     (lavaan, FIML) com incerteza por partido.

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
library(scales)
library(Ckmeans.1d.dp)
library(MCMCpack)
library(coda)

source("codigos/00_config.R")

set.seed(20260809)

tabela_final <- fread(glue("{dir_processado}/tabela_final.csv")) %>% as_tibble()
dimensoes <- c("migracao", "frentes", "coligacao", "votacao", "doacoes")

# ---------------------------------------------------------------------------
# helpers espelhados do 06 (manter em sincronia com 06_juntando_tudo.R).
# Cada cenário alternativo é re-rotulado com a mesma regra publicada: cortes
# herdados do levantamento por equating, re-derivados no composto do cenário, +
# subfaixas Ckmeans com k editorial. Assim a comparação mede o efeito do
# cenário, não uma diferença de regra.
# ---------------------------------------------------------------------------
K_SUB_EDITORIAL <- c(esquerda = 3L, centro = 3L, direita = 3L)

bolognesi_ext_06 <- read.csv("dados/brutos/dados-externos/bolognesi2022.csv")
escores_bol <- set_names(bolognesi_ext_06$bolognesi22, bolognesi_ext_06$partido)

cortes_equating <- function(posicoes, escores_ext, cortes_ext = c(4.5, 7.0)) {
  comuns <- intersect(names(posicoes), names(escores_ext))
  comuns <- comuns[!is.na(escores_ext[comuns]) & !is.na(posicoes[comuns])]
  ord <- comuns[order(posicoes[comuns])]
  iso <- isoreg(posicoes[ord], escores_ext[ord])
  vapply(cortes_ext, function(cc) {
    abaixo <- which(iso$yf < cc)
    stopifnot(length(abaixo) > 0, length(abaixo) < length(ord),
              all(diff(abaixo) == 1))
    i <- max(abaixo)
    (posicoes[ord][i] + posicoes[ord][i + 1]) / 2
  }, numeric(1))
}

# espelho do 05: corte centro|direita pelo kernel equipercentil depurado;
# esquerda|centro segue isotônica + ponto médio.
silverman <- function(v) 0.9 * min(sd(v), IQR(v) / 1.34) * length(v)^(-1/5)

corte_kernel <- function(x, y, cortes_ext = c(4.5, 7.0), mult_h = 1) {
  hx <- silverman(x) * mult_h
  hy <- silverman(y) * mult_h
  Fy <- function(q) mean(pnorm((q - y) / hy))
  Fx <- function(q) mean(pnorm((q - x) / hx))
  vapply(cortes_ext, function(cc) {
    p <- Fy(cc)
    uniroot(function(q) Fx(q) - p, interval = c(min(x) - 30, max(x) + 30))$root
  }, numeric(1))
}

kernel_depurado <- function(posicoes, escores_ext, cortes_ext = c(4.5, 7.0)) {
  comuns <- intersect(names(posicoes), names(escores_ext))
  comuns <- comuns[!is.na(escores_ext[comuns]) & !is.na(posicoes[comuns])]
  x <- posicoes[comuns]; y <- escores_ext[comuns]
  ord <- comuns[order(x)]
  iso <- isoreg(x[ord], y[ord])
  residuos <- set_names(y[ord] - iso$yf, ord)
  depurados <- names(residuos)[abs(residuos) > 3 * mad(residuos)]
  mantidos <- setdiff(ord, depurados)
  corte_kernel(x[mantidos], y[mantidos], cortes_ext)
}

cortes_publicados <- function(posicoes, escores_ext) {
  c(cortes_equating(posicoes, escores_ext)[1],
    kernel_depurado(posicoes, escores_ext)[2])
}

rotular_ancorado <- function(posicoes, cortes, k_editorial = K_SUB_EDITORIAL) {
  nomes_familia <- c("esquerda", "centro", "direita")
  familia_idx <- findInterval(posicoes, sort(cortes)) + 1L
  sub_idx <- integer(length(posicoes))
  k_por_familia <- integer(3)
  for (f in 1:3) {
    idx <- which(familia_idx == f)
    if (!length(idx)) next
    k <- min(k_editorial[[nomes_familia[f]]], length(idx))
    k_por_familia[f] <- k
    sub_idx[idx] <- if (k == 1) 1L else Ckmeans.1d.dp(posicoes[idx], k)$cluster
  }
  offset <- c(0L, cumsum(k_por_familia)[1:2])
  codigo <- offset[familia_idx] + sub_idx
  tem_sub <- k_por_familia[familia_idx] > 1
  label <- ifelse(tem_sub,
                  paste0(nomes_familia[familia_idx], "_", sub_idx),
                  nomes_familia[familia_idx])
  tibble(partido = names(posicoes),
         cluster = codigo,
         label = label,
         familia = nomes_familia[familia_idx])
}

# re-rotula um composto alternativo com a regra publicada (cortes
# re-derivados no próprio composto + subfaixas com k editorial)
rotular_regra_publicada <- function(posicoes) {
  posicoes <- posicoes[!is.na(posicoes) & !is.nan(posicoes)]
  rotular_ancorado(posicoes, cortes_publicados(posicoes, escores_bol))
}

compor_rank <- function(tab, dims) {
  tab %>%
    mutate(across(all_of(dims),
                  ~ rank(., ties.method = "min", na.last = "keep"),
                  .names = "{.col}_rank")) %>%
    mutate(across(ends_with("_rank"), ~ rescale(., to = c(1, 100)))) %>%
    rowwise() %>%
    mutate(media_rank = mean(c_across(ends_with("_rank")), na.rm = TRUE)) %>%
    ungroup()
}

# alinhamento de sinal por âncora (PT sempre negativo)
alinhar_pt <- function(v) {
  stopifnot("PT" %in% names(v))
  if (v[["PT"]] > 0) -v else v
}

ca_dim1 <- function(m) {
  d <- ca(m)$rowcoord[, "Dim1"]
  alinhar_pt(d)
}

# A. diagonal das CAs quadradas -------------------------------------------
# consistência externa: Spearman da Dim1 com a média dos z-scores das
# outras dimensões, por partido
consistencia_externa <- function(dim1, dimensao) {
  outras <- paste0("z_", setdiff(dimensoes, dimensao))
  ref <- tabela_final %>%
    dplyr::select(partido, all_of(outras)) %>%
    rowwise() %>%
    mutate(media_outras = mean(c_across(all_of(outras)), na.rm = TRUE)) %>%
    ungroup()
  comum <- intersect(names(dim1), ref$partido[!is.nan(ref$media_outras)])
  cor(dim1[comum],
      ref$media_outras[match(comum, ref$partido)],
      method = "spearman")
}

avaliar_diagonal <- function(nome_dim, tbl_pares, d1_publicada) {
  m0 <- matriz_pares(tbl_pares)

  # tratamento do pipeline: quasi-independência
  m_quasi <- diag_quasi(m0)
  dim1_quasi <- ca_dim1(m_quasi)

  # tratamento da versão de 2024 (colSums), mantido como comparação
  m_legado <- m0
  diag(m_legado) <- colSums(m0)
  dim1_legado <- ca_dim1(m_legado)

  # sanidade da reconstrução: a variante do pipeline tem que reproduzir a Dim1
  # publicada (mesma matriz que o pipeline)
  comum <- intersect(names(dim1_quasi), names(d1_publicada))
  fidelidade <- cor(dim1_quasi[comum], alinhar_pt(d1_publicada)[comum])
  stopifnot(fidelidade > 0.9999)

  # âncoras nos polos corretos
  ancoras_ok <- dim1_quasi[["PT"]] < 0 &&
    (!"PL" %in% names(dim1_quasi) || dim1_quasi[["PL"]] > 0)

  ce_quasi <- consistencia_externa(dim1_quasi, nome_dim)
  ce_legado <- consistencia_externa(dim1_legado, nome_dim)
  cor_entre <- cor(dim1_quasi[comum], dim1_legado[comum], method = "spearman")

  ordem <- names(dim1_quasi)
  legado_ord <- dim1_legado[ordem]
  tibble(partido = ordem,
         dim1_quasi = round(unname(dim1_quasi), 4),
         dim1_colsums_legado = round(unname(legado_ord), 4),
         rank_quasi = rank(unname(dim1_quasi)),
         rank_legado = rank(unname(legado_ord))) %>%
    mutate(dimensao = nome_dim,
           ce_quasi = round(ce_quasi, 4),
           ce_legado = round(ce_legado, 4))
}

tbl_mig <- fread(glue("{dir_resultado}/tbl_migracao_partidaria.csv")) %>%
  as_tibble() %>%
  dplyr::select(partido_1, partido_2, n)
tbl_col <- fread(glue("{dir_resultado}/tbl_coligacoes.csv")) %>%
  as_tibble() %>%
  dplyr::select(partido_1, partido_2, n_coligacoes)
d1_mig_pub <- fread(glue("{dir_resultado}/d1_migracao_partidaria.csv")) %>%
  { set_names(.$Dim1, .$partido) }
d1_col_pub <- fread(glue("{dir_resultado}/d1_coligacoes.csv")) %>%
  { set_names(.$Dim1, .$partido) }

res_diagonal <- bind_rows(
  avaliar_diagonal("migracao", tbl_mig, d1_mig_pub),
  avaliar_diagonal("coligacao", tbl_col, d1_col_pub)
)
write.csv(res_diagonal,
          glue("{dir_processado}/diagnostico_auditoria_diagonal.csv"),
          row.names = FALSE)

# B. sensibilidade do corte de migrações ----------------------------------

# reconstrói os pares a partir dos brutos (espelha o script 01). A janela é
# decidida pela presença do bruto local de 2026, não por nova consulta ao
# CDN do TSE: se o 01 baixou 2026, a janela publicada é 2022-2026 e é ela
# que a sensibilidade tem que espelhar.
dir_candidatos <- file.path(dir_brutos, "candidatos")
arq_2026 <- file.path(dir_candidatos, 2026, "consulta_cand_2026_BRASIL.csv")
anos_mig <- if (file.exists(arq_2026)) c(2022, 2024, 2026) else c(2020, 2022, 2024)

candidatos <- anos_mig %>%
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

mapa_titulo_cpf <- candidatos %>%
  filter(!is.na(cpf), !is.na(titulo)) %>%
  distinct(titulo, cpf) %>%
  distinct(titulo, .keep_all = TRUE)

pares <- candidatos %>%
  left_join(rename(mapa_titulo_cpf, cpf_mapa = cpf), by = "titulo") %>%
  mutate(cpf = coalesce(cpf, cpf_mapa)) %>%
  mutate(ID = case_when(
    !is.na(cpf)    ~ paste0("cpf_", cpf),
    !is.na(titulo) ~ paste0("tit_", titulo),
    TRUE           ~ paste0("nom_", NM_CANDIDATO, "_", DT_NASCIMENTO, "_",
                            SG_UF_NASCIMENTO)
  )) %>%
  mutate(SG_PARTIDO = harmonizar_sigla(SG_PARTIDO)) %>%
  distinct(ANO_ELEICAO, ID, .keep_all = TRUE) %>%
  dplyr::select(ANO_ELEICAO, ID, SG_PARTIDO) %>%
  arrange(ID, ANO_ELEICAO) %>%
  group_by(ID) %>%
  mutate(partido_antes = lag(SG_PARTIDO)) %>%
  ungroup() %>%
  drop_na(partido_antes) %>%
  dplyr::select(partido_antes, partido_depois = SG_PARTIDO)

pares_tbl <- pares %>%
  count(partido_antes, partido_depois) %>%
  rowwise() %>%
  mutate(id_par = paste(sort(c(partido_antes, partido_depois)),
                        collapse = "/")) %>%
  ungroup() %>%
  group_by(id_par) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  separate(id_par, into = c("partido_1", "partido_2"), sep = "/")

sens_corte <- map(c(5, 10, 15, 20), function(corte) {
  remover <- pares_tbl %>%
    filter(partido_1 != partido_2) %>%
    pivot_longer(cols = c(partido_1, partido_2)) %>%
    group_by(value) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    filter(n <= corte) %>%
    pull(value)
  tbl_c <- pares_tbl %>%
    filter(!partido_1 %in% remover, !partido_2 %in% remover)
  m <- matriz_pares(dplyr::select(tbl_c, partido_1, partido_2, n))
  diag(m) <- colSums(m)
  d1 <- ca_dim1(m)
  tibble(corte = corte, partido = names(d1),
         dim1 = round(unname(d1), 4), rank = rank(d1))
}) %>%
  bind_rows()

write.csv(sens_corte,
          glue("{dir_processado}/diagnostico_auditoria_corte_migracao.csv"),
          row.names = FALSE)

# C. IRT: corte de participação e obstrução -------------------------------
dir_votacao <- file.path(dir_brutos, "votacoes_camara/votos")
votos <- dir_votacao %>%
  list.files(full.names = TRUE) %>%
  map(~ fread(.x, colClasses = "character") %>% mutate(path = .x)) %>%
  bind_rows() %>%
  mutate(id_votacao = str_remove_all(path, ".*/|\\.csv")) %>%
  dplyr::select(tipoVoto, deputado_.id, deputado_.nome,
                deputado_.siglaPartido, id_votacao, dataRegistroVoto) %>%
  mutate(deputado_.siglaPartido = harmonizar_sigla(deputado_.siglaPartido))

info_dep <- votos %>%
  dplyr::select(deputado_.id, dataRegistroVoto, deputado_.siglaPartido) %>%
  arrange(desc(dataRegistroVoto)) %>%
  distinct(deputado_.id, .keep_all = TRUE) %>%
  drop_na() %>%
  transmute(id_deputado = as.character(deputado_.id),
            partido = deputado_.siglaPartido)

matriz_votos <- function(votos, obstrucao_como_nao = FALSE, corte_pct = 10) {
  v <- votos
  if (obstrucao_como_nao) {
    v <- mutate(v, tipoVoto = ifelse(tipoVoto == "Obstrução", "Não", tipoVoto))
  }
  v <- v %>%
    filter(tipoVoto %in% c("Sim", "Não")) %>%
    mutate(valor = as.numeric(tipoVoto == "Sim")) %>%
    distinct(deputado_.id, id_votacao, .keep_all = TRUE)
  n_vot <- n_distinct(v$id_votacao)
  remover <- v %>%
    group_by(deputado_.id) %>%
    summarise(pct = n_distinct(id_votacao) / n_vot * 100, .groups = "drop") %>%
    filter(pct <= corte_pct) %>%
    pull(deputado_.id)
  v %>%
    filter(!deputado_.id %in% remover) %>%
    dplyr::select(deputado_.id, valor, id_votacao) %>%
    pivot_wider(names_from = id_votacao, values_from = valor) %>%
    column_to_rownames("deputado_.id") %>%
    as.matrix()
}

# cadeia curta ancorada (sensibilidade de ordenação, não estimativa final)
irt_curto_partidos <- function(mtx, seed = 12345) {
  participacao <- rowSums(!is.na(mtx))
  escolher <- function(sigla) {
    ids <- intersect(info_dep$id_deputado[info_dep$partido == sigla],
                     rownames(mtx))
    ids[which.max(participacao[ids])]
  }
  ancora_neg <- escolher("PT")
  ancora_pos <- escolher("NOVO")
  restricoes <- setNames(list("-", "+"), c(ancora_neg, ancora_pos))
  set.seed(seed)
  theta_ini <- rnorm(nrow(mtx), 0, 2)
  theta_ini[rownames(mtx) == ancora_neg] <- -2
  theta_ini[rownames(mtx) == ancora_pos] <- 2
  res <- MCMCirt1d(mtx, theta.constraints = restricoes,
                   theta.start = theta_ini,
                   burnin = 2000, mcmc = 8000, verbose = 0, seed = seed)
  theta <- colMeans(as.matrix(res))
  names(theta) <- str_remove(names(theta), "theta\\.")
  dep <- tibble(id_deputado = names(theta), theta = unname(theta)) %>%
    left_join(info_dep, by = "id_deputado") %>%
    filter(!is.na(partido), partido != "", partido != "S.PART.")
  alinhar_pt(tapply(dep$theta, dep$partido, median))
}

cenarios_irt <- list(
  corte_5  = list(obst = FALSE, corte = 5),
  corte_10 = list(obst = FALSE, corte = 10),
  corte_20 = list(obst = FALSE, corte = 20),
  obstrucao_nao = list(obst = TRUE, corte = 10)
)

pub <- fread(glue("{dir_resultado}/d1_ponto_ideal_partidos.csv")) %>%
  { alinhar_pt(set_names(.$ponto_ideal, .$partido)) }

res_irt <- imap(cenarios_irt, function(cfg, nome) {
  cat("   |- Cenário IRT:", nome, "\n")
  mtx <- matriz_votos(votos, cfg$obst, cfg$corte)
  med <- irt_curto_partidos(mtx)
  tibble(cenario = nome, partido = names(med),
         ponto_ideal = round(unname(med), 4), rank = rank(med))
}) %>%
  bind_rows()

# obstrução: efeito nas FAMÍLIAS (troca a dimensão votação e re-rotula)
med_obst <- filter(res_irt, cenario == "obstrucao_nao") %>%
  { set_names(.$ponto_ideal, .$partido) }
tab_obst <- tabela_final %>%
  dplyr::select(partido, all_of(dimensoes)) %>%
  mutate(votacao = as.numeric(med_obst[partido])) %>%
  compor_rank(dimensoes)
grupos_obst <- rotular_regra_publicada(set_names(tab_obst$media_rank, tab_obst$partido))
grupos_obst %>%
  left_join(dplyr::select(tabela_final, partido, familia_pub = familia),
            by = "partido") %>%
  dplyr::select(partido, familia_obstrucao_nao = familia, familia_pub) %>%
  write.csv(glue("{dir_processado}/diagnostico_auditoria_obstrucao.csv"),
            row.names = FALSE)

write.csv(res_irt,
          glue("{dir_processado}/diagnostico_auditoria_irt_sens.csv"),
          row.names = FALSE)

# D. composto restrito + fator latente ------------------------------------
dimensoes5 <- dimensoes  # já inclui doações

completos <- filter(tabela_final, n_dimensoes == 5)
tab_r <- completos %>%
  dplyr::select(partido, all_of(dimensoes5)) %>%
  compor_rank(dimensoes5)
grupos_r <- rotular_regra_publicada(set_names(tab_r$media_rank, tab_r$partido))
mudou_r <- grupos_r %>%
  left_join(dplyr::select(tabela_final, partido, familia, label),
            by = "partido", suffix = c("_restrito", "_pub")) %>%
  filter(familia_restrito != familia_pub | label_restrito != label_pub)
write.csv(grupos_r %>%
            left_join(dplyr::select(tabela_final, partido, familia_pub = familia,
                                    label_pub = label), by = "partido"),
          glue("{dir_processado}/diagnostico_auditoria_composto_restrito.csv"),
          row.names = FALSE)

# fator latente 1D com FIML (assume MAR; leitura alternativa com incerteza)
fator_ok <- tryCatch({
  dat <- as.data.frame(tabela_final[, paste0("z_", dimensoes5)])
  fit <- lavaan::cfa("f =~ z_migracao + z_frentes + z_coligacao + z_votacao + z_doacoes",
                     data = dat, missing = "fiml", std.lv = TRUE)
  sc <- lavaan::lavPredict(fit, se = "standard")
  se <- attr(sc, "se")
  if (is.list(se)) se <- se[[1]]
  escore <- as.numeric(sc[, "f"])
  if (escore[tabela_final$partido == "PT"] > 0) escore <- -escore
  fator <- tibble(partido = tabela_final$partido,
                  escore_fator = round(escore, 4),
                  se_fator = round(as.numeric(se), 4),
                  media_rank = tabela_final$media_rank,
                  media_z = tabela_final$media_z)
  write.csv(fator, glue("{dir_processado}/diagnostico_auditoria_fator.csv"),
            row.names = FALSE)
  TRUE
}, error = function(e) FALSE)
