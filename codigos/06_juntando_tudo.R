# pacotes -----------------------------------------------------------------
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(scales)
library(tidyr)
library(tibble)
library(glue)
library(purrr)
library(cluster)
library(Ckmeans.1d.dp)
library(mclust)
library(jsonlite)

source("codigos/00_config.R")

set.seed(20260705)  # clusGap e reamostragens

# helpers -----------------------------------------------------------------

# Número de subfaixas guiado pelos dados (usado na regra de diagnóstico):
# o k de cada família (1 a 3) é o que maximiza a silhueta média. A silhueta
# é calculada para k=2 e k=3 (k=1 não tem silhueta definida); vence a maior;
# se nem a vencedora passar de 0,5 (limiar clássico de Kaufman & Rousseeuw
# 1990 para estrutura razoável), a família fica em k=1, sem subfaixa. Com
# menos de 4 partidos não dá para testar k=3, e com menos de 3, nem k=2.
k_sub_natural <- function(pos, k_min = 1, k_max = 3, limiar_silhueta = 0.5) {
  candidatos <- (max(2, k_min)):min(k_max, length(pos) - 1)
  if (!length(candidatos) || candidatos[1] > tail(candidatos, 1)) return(1L)
  d <- dist(pos)
  sils <- vapply(candidatos, function(k) {
    cl <- Ckmeans.1d.dp(pos, k)$cluster
    mean(cluster::silhouette(cl, d)[, "sil_width"])
  }, numeric(1))
  melhor <- candidatos[which.max(sils)]
  if (max(sils) < limiar_silhueta) return(1L)
  melhor
}

# Famílias da regra de diagnóstico: 3 grupos contíguos na escala, por
# Ckmeans k=3; se o corte não deixar PT na 1ª família e PL na 3ª, prevalece
# a regra de âncoras: o menor k que separa PT de PL, com as famílias
# definidas pela posição relativa às âncoras.
macro_familias <- function(posicoes) {
  cl3 <- Ckmeans.1d.dp(posicoes, 3)$cluster
  fam_pt <- cl3[names(posicoes) == "PT"]
  fam_pl <- cl3[names(posicoes) == "PL"]
  if (length(fam_pt) == 1 && length(fam_pl) == 1 &&
      fam_pt == 1 && fam_pl == 3) {
    return(cl3)
  }
  for (k_cand in 4:10) {
    cl <- Ckmeans.1d.dp(posicoes, k_cand)$cluster
    cl_pt <- cl[names(posicoes) == "PT"]
    cl_pl <- cl[names(posicoes) == "PL"]
    if (!length(cl_pt) || !length(cl_pl) || cl_pt >= cl_pl) next
    fam <- ifelse(cl <= cl_pt, 1L, ifelse(cl >= cl_pl, 3L, 2L))
    if (all(1:3 %in% fam)) {
      return(fam)
    }
  }
  stop("Não foi possível separar as âncoras PT e PL em famílias distintas.")
}

# Partição hierárquica 3 -> (1 a 3) sobre um vetor nomeado de posições: 3
# famílias fixas (macro_familias), depois dentro de cada uma o número de
# subfaixas que a própria família sustenta (k_sub_natural). Famílias com
# menos partidos que o k escolhido recebem k interno menor automaticamente
# (guarda de k_sub_natural). Rótulo sem sufixo quando a família não separa
# em subfaixas (k=1): "centro", não "centro_1".
rotular_faixas <- function(posicoes) {
  nomes_familia <- c("esquerda", "centro", "direita")
  familia_idx <- macro_familias(posicoes)
  sub_idx <- integer(length(posicoes))
  k_por_familia <- integer(3)
  for (f in 1:3) {
    idx <- familia_idx == f
    k_sub <- k_sub_natural(posicoes[idx])
    k_por_familia[f] <- k_sub
    sub_idx[idx] <- Ckmeans.1d.dp(posicoes[idx], k_sub)$cluster
  }
  offset <- c(0L, cumsum(k_por_familia)[1:2])
  codigo <- offset[familia_idx] + sub_idx  # 1..sum(k_por_familia), esquerda p/ direita
  tem_sub <- k_por_familia[familia_idx] > 1
  label <- ifelse(tem_sub,
                  paste0(nomes_familia[familia_idx], "_", sub_idx),
                  nomes_familia[familia_idx])
  tibble(partido = names(posicoes),
         cluster = codigo,
         label = label,
         familia = nomes_familia[familia_idx])
}

# ---------------------------------------------------------------------------
# rotulagem publicada: cortes de família herdados por equating + subfaixas
# Ckmeans com k editorial por família
# ---------------------------------------------------------------------------

# Número de subfaixas por família: 3 nas três, por decisão editorial (a
# paleta de 9 gradações é padrão da Folha). Custo declarado na metodologia:
# a silhueta prefere k=2 na esquerda e no centro (só a direita prefere 3).
# Guarda automática para famílias pequenas via min(k, n) em rotular_ancorado.
K_SUB_EDITORIAL <- c(esquerda = 3L, centro = 3L, direita = 3L)

# Cortes herdados do instrumento externo por equating: regressão isotônica
# do escore externo sobre a nossa escala (preserva a nossa ordem, sem forma
# funcional imposta); cada corte externo (4,5 e 7,0, as bandas do levantamento)
# vira o ponto médio de media_rank entre os dois partidos adjacentes ao
# cruzamento do ajuste. A monotonicidade da isotônica garante cruzamento
# único (o conjunto abaixo do corte é um prefixo da ordem).
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

# O corte centro|direita é traduzido por um segundo estimador de equating, o
# equipercentil com continuização por kernel gaussiano (banda de Silverman)
# e depuração de âncoras: partidos em que o levantamento e a nossa régua divergem
# muito (resíduo isotônico acima de 3 vezes o MAD) saem do ajuste, para uma
# divergência pontual não puxar a fronteira. O corte esquerda|centro segue a
# isotônica com ponto médio: ali o kernel moveria o PDT para o centro, e os
# dois levantamentos de especialistas o colocam na esquerda. A regra combinada é
# mais estável sob a incerteza das medições do que usar um só estimador nos
# dois cortes.
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

# regra publicada: isotônica no corte 4,5; kernel depurado no corte 7,0
cortes_publicados <- function(posicoes, escores_ext) {
  c(cortes_equating(posicoes, escores_ext)[1],
    kernel_depurado(posicoes, escores_ext)[2])
}

# Partição publicada: famílias pelo lado dos cortes herdados (fixos), depois
# subfaixas por Ckmeans dentro de cada família (guarda min(k, n)). Rótulo
# sem sufixo quando a família fica com 1 subfaixa.
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

# Rotulagem alternativa (diagnóstico): menor k com 3 famílias não-vazias por
# âncoras PT/PL, usada na comparação de robustez entre escalas.
rotular_ancoras <- function(posicoes) {
  for (k_cand in 3:10) {
    ck <- Ckmeans.1d.dp(posicoes, k_cand)
    cl <- ck$cluster
    cl_pt <- cl[names(posicoes) == "PT"]
    cl_pl <- cl[names(posicoes) == "PL"]
    if (!length(cl_pt) || !length(cl_pl) || cl_pt >= cl_pl) next
    fam <- dplyr::case_when(seq_len(k_cand) <= cl_pt ~ "esquerda",
                            seq_len(k_cand) >= cl_pl ~ "direita",
                            TRUE ~ "centro")
    if (all(c("esquerda", "centro", "direita") %in% fam[unique(cl)])) {
      return(tibble(partido = names(posicoes), familia_ancoras = fam[cl]))
    }
  }
  tibble(partido = names(posicoes), familia_ancoras = NA_character_)
}

# Regra alternativa, testada e rejeitada (a fronteira de família não cai no
# maior buraco): família pelos 2 maiores vãos da régua composta, sem olhar a
# estrutura de aglomerados. Só serve ao teste de robustez abaixo; nunca é
# usada para as faixas publicadas.
fronteira_gap2 <- function(posicoes) {
  pos <- sort(posicoes)
  g <- diff(pos)
  b <- sort(order(-g)[1:2])
  fam <- integer(length(pos))
  fam[1:b[1]] <- 1L
  fam[(b[1] + 1):b[2]] <- 2L
  fam[(b[2] + 1):length(pos)] <- 3L
  names(fam) <- names(pos)
  par <- paste(names(pos)[b], names(pos)[b + 1], sep = "·", collapse = " | ")
  list(familia = fam, par = par)
}

# Ranking composto 1-100 (mesmo método de escala da versão de 2024)
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

# lê as cinco dimensões ---------------------------------------------------
ler_dimensao <- function(path, nome) {
  tab <- fread(path)
  tab <- tab[, 1:2]
  setnames(tab, c("partido", nome))
  tab %>%
    as_tibble() %>%
    mutate(partido = harmonizar_sigla(partido))
}

migracao  <- ler_dimensao(glue("{dir_resultado}/d1_migracao_partidaria.csv"), "migracao")
frentes   <- ler_dimensao(glue("{dir_resultado}/d1_frentes_politicos.csv"), "frentes")
coligacao <- ler_dimensao(glue("{dir_resultado}/d1_coligacoes.csv"), "coligacao")
votacao   <- ler_dimensao(glue("{dir_resultado}/d1_ponto_ideal_partidos.csv"), "votacao")
# doações de pessoas físicas (TSE 2022+2024; ver 05_doacoes.R), com a
# janela deliberadamente um ciclo atrás das demais dimensões do TSE
doacoes   <- ler_dimensao(glue("{dir_resultado}/d1_doacoes_pf.csv"), "doacoes")

dimensoes <- c("migracao", "frentes", "coligacao", "votacao", "doacoes")

# piso de bancada nas dimensões parlamentares: votações e frentes medem o
# partido pelo comportamento dos deputados; com menos de 3 o agregado é o
# indivíduo (mediana sem ponto de ruptura, bootstrap de cluster degenerado,
# IC de largura zero). O piso vale para o ponto partidário publicado; a
# estimação (IRT do 04, CA do 02) segue com todos os deputados. Critério:
# n_deputados de d1_ponto_ideal_partidos.csv.
MIN_BANCADA_PARLAMENTAR <- 3
bancada_tbl <- fread(glue("{dir_resultado}/d1_ponto_ideal_partidos.csv")) %>%
  as_tibble() %>%
  mutate(partido = harmonizar_sigla(partido))
partidos_sub_piso <- bancada_tbl$partido[
  bancada_tbl$n_deputados < MIN_BANCADA_PARLAMENTAR]
stopifnot(all(frentes$partido %in% bancada_tbl$partido))
votacao <- filter(votacao, !partido %in% partidos_sub_piso)
frentes <- filter(frentes, !partido %in% partidos_sub_piso)

tudo <- list(migracao, frentes, coligacao, votacao, doacoes) %>%
  reduce(full_join, by = "partido")

# alinha o sinal: PT sempre no lado negativo (esquerda) -----------------------
stopifnot("PT" %in% tudo$partido)
for (dim_nome in dimensoes) {
  valor_pt <- tudo[[dim_nome]][tudo$partido == "PT"]
  stopifnot(!is.na(valor_pt))
  if (valor_pt > 0) tudo[[dim_nome]] <- tudo[[dim_nome]] * -1
}

# escalas compostas -------------------------------------------------------------
tudo <- tudo %>%
  mutate(across(all_of(dimensoes), ~ as.numeric(scale(.)), .names = "z_{.col}"))

cols_z <- paste0("z_", dimensoes)

tudo <- tudo %>%
  rowwise() %>%
  mutate(media_z = mean(c_across(all_of(cols_z)), na.rm = TRUE),
         n_dimensoes = sum(!is.na(c_across(all_of(cols_z))))) %>%
  ungroup() %>%
  compor_rank(dimensoes) %>%
  arrange(media_rank)

# unidimensionalidade (premissa testada) ----------------------------------------
mtx_dims <- as.matrix(tudo[, cols_z])
rownames(mtx_dims) <- tudo$partido
cors <- cor(mtx_dims, use = "pairwise.complete.obs")
write.csv(round(cors, 4),
          glue("{dir_processado}/diagnostico_correlacao_dimensoes.csv"))

# A mesma correlação, agora sobre os rankings 1-100 e não sobre os z. A
# escala publicada é media_rank, e é dela que sai a régua desenhada, então a
# concordância entre as medições citada na matéria é a desta escala. A de z
# fica, porque é a que testa a premissa de unidimensionalidade; são
# perguntas diferentes.
cols_rank_dim <- paste0(dimensoes, "_rank")
mtx_rank <- as.matrix(tudo[, cols_rank_dim])
rownames(mtx_rank) <- tudo$partido
cors_rank <- cor(mtx_rank, use = "pairwise.complete.obs", method = "spearman")
write.csv(round(cors_rank, 4),
          glue("{dir_processado}/diagnostico_correlacao_dimensoes_rank.csv"))

# evidência sobre grau de estrutura na escala publicada (media_rank) ------
posicoes <- set_names(tudo$media_rank, tudo$partido)
posicoes_z <- set_names(tudo$media_z, tudo$partido)
k_range <- 2:12

d1 <- dist(posicoes)
silhuetas <- map_dbl(k_range, function(k) {
  mean(silhouette(Ckmeans.1d.dp(posicoes, k)$cluster, d1)[, "sil_width"])
})

gap <- clusGap(matrix(posicoes, ncol = 1),
               FUNcluster = function(x, k) {
                 list(cluster = Ckmeans.1d.dp(as.numeric(x[, 1]), k)$cluster)
               },
               K.max = 12, B = 500, verbose = FALSE)
k_gap <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "firstSEmax")

mc <- Mclust(as.numeric(posicoes), G = 1:12, verbose = FALSE)
k_mclust <- mc$G
k_ckmeans <- length(Ckmeans.1d.dp(posicoes, k = c(1, 12))$size)

gap_vals <- round(gap$Tab[k_range, "gap"], 4)
gap_ses  <- round(gap$Tab[k_range, "SE.sim"], 4)
tbl_selecao_k <- tibble(k = k_range,
                        silhueta_media = round(silhuetas, 4),
                        gap = gap_vals, gap_se = gap_ses)
write.csv(tbl_selecao_k, glue("{dir_processado}/diagnostico_selecao_k.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# faixas publicadas: famílias por cortes herdados do levantamento de especialistas
# (isotônica no 4,5; kernel depurado no 7,0); subfaixas por Ckmeans dentro
# de cada família
# ---------------------------------------------------------------------------
bolognesi_ext <- read.csv("dados/brutos/dados-externos/bolognesi2022.csv") %>%
  as_tibble()
escores_bol <- set_names(bolognesi_ext$bolognesi22, bolognesi_ext$partido)
cortes_familia <- cortes_publicados(posicoes, escores_bol)

grupos <- rotular_ancorado(posicoes, cortes_familia)
tudo <- left_join(tudo, grupos, by = "partido")
K_FAIXAS <- length(unique(grupos$cluster))

# regra de agrupamento direto (estilo 2024): diagnóstico e comparador
grupos_direto <- rotular_faixas(posicoes) %>%
  select(partido, label_direto = label, familia_direto = familia)
tudo <- left_join(tudo, grupos_direto, by = "partido")

# concordância externa por família (Bolognesi: esquerda < 4,5 <= centro
# < 7,0 <= direita; BLS9: banda centrista de +-0,25)
fold3 <- function(x, c1, c2) ifelse(x < c1, "esquerda", ifelse(x < c2, "centro", "direita"))
bls_ext <- read.csv("dados/brutos/dados-externos/bls9.csv") %>% as_tibble()
concord <- tudo %>%
  select(partido, familia, familia_direto) %>%
  left_join(bolognesi_ext %>% transmute(partido, bol = bolognesi22,
                                        bol_fold = fold3(bolognesi22, 4.5, 7.0)),
            by = "partido") %>%
  left_join(bls_ext %>% transmute(partido, bls = bls9,
                                  bls_fold = fold3(bls9, -0.25, 0.25)),
            by = "partido") %>%
  mutate(concorda_bol = familia == bol_fold,
         concorda_bls = familia == bls_fold,
         concorda_bol_direto = familia_direto == bol_fold,
         concorda_bls_direto = familia_direto == bls_fold)
write.csv(concord, glue("{dir_processado}/diagnostico_concordancia_externa.csv"),
          row.names = FALSE)

# par (media_rank, escore externo) e o ajuste isotônico partido a partido,
# base da figura de equating da matéria
comuns_eq <- intersect(names(posicoes), names(escores_bol))
ord_eq <- comuns_eq[order(posicoes[comuns_eq])]
iso_eq <- isoreg(posicoes[ord_eq], escores_bol[ord_eq])
tibble(partido = ord_eq,
       media_rank = round(as.numeric(posicoes[ord_eq]), 3),
       bolognesi22 = as.numeric(escores_bol[ord_eq]),
       iso = round(iso_eq$yf, 3)) %>%
  write.csv(glue("{dir_processado}/diagnostico_equating.csv"), row.names = FALSE)

# perda do aninhamento vs a partição plana ótima do mesmo k: partições
# ótimas de k diferentes não aninham necessariamente; medimos o custo de
# impor a hierarquia e a concordância entre as duas partições. A versão
# plana não respeita família, então seus clusters levam rótulo genérico.
ck_flat <- Ckmeans.1d.dp(posicoes, K_FAIXAS)
wss_por_grupo <- function(x, cl) {
  sum(tapply(x, cl, function(v) sum((v - mean(v))^2)))
}
wss_flat <- sum(ck_flat$withinss)
wss_aninhado <- wss_por_grupo(posicoes, grupos$cluster)
perda_wss <- (wss_aninhado - wss_flat) / wss_flat
ari_aninhado_flat <- adjustedRandIndex(grupos$cluster, ck_flat$cluster)
tibble(partido = names(posicoes),
       media_rank = round(as.numeric(posicoes), 4),
       label_aninhado = grupos$label,
       label_flat = paste0("flat_", ck_flat$cluster)) %>%
  write.csv(glue("{dir_processado}/diagnostico_aninhamento.csv"),
            row.names = FALSE)

# âncoras de sanidade
stopifnot(startsWith(tudo$label[tudo$partido == "PT"], "esquerda"))
stopifnot(startsWith(tudo$label[tudo$partido == "PL"], "direita"))
stopifnot(startsWith(tudo$label[tudo$partido == "NOVO"], "direita"))

# rotulagem alternativa (diagnóstico): âncoras sobre a escala z
alt_z <- rotular_ancoras(posicoes_z) %>%
  rename(label_z_ancoras = familia_ancoras)
tudo <- left_join(tudo, alt_z, by = "partido")

# ---------------------------------------------------------------------------
# escala métrica robusta e classificação das divisas
# Os partidos nanicos orientam os eixos das CAs e por isso ficam no cálculo,
# mas suas coordenadas são médias de amostras minúsculas; na escala métrica
# de diagnóstico eles são truncados à borda dos bem medidos (censura: "pelo
# menos tão extremo quanto"). Nada aqui altera media_rank nem as faixas.
# ---------------------------------------------------------------------------

massa_ca <- function(path_tbl) {
  tbl <- fread(path_tbl)
  m <- diag_quasi(matriz_pares(as_tibble(tbl[, 1:3])))
  rowSums(m) / sum(m)
}

# z calculado sobre os bem medidos (massa >= 1%); demais truncados à borda
z_robusto_ca <- function(valores_nomeados, massas, corte_massa = 0.01) {
  v <- valores_nomeados
  bem <- names(v)[!is.na(v) &
                    !is.na(massas[names(v)]) &
                    massas[names(v)] >= corte_massa]
  mu <- mean(v[bem])
  dp <- sd(v[bem])
  z <- (v - mu) / dp
  pmin(pmax(z, min(z[bem])), max(z[bem]))
}

massa_mig <- massa_ca(glue("{dir_resultado}/tbl_migracao_partidaria.csv"))
massa_col <- massa_ca(glue("{dir_resultado}/tbl_coligacoes.csv"))
# doações: as massas de coluna da CA doador x partido vêm prontas do 05
# (mesma regra: nanicos orientam o eixo mas são truncados à borda)
massa_doa_tbl <- fread(glue("{dir_resultado}/tbl_doacoes_massa.csv"))
massa_doa <- set_names(massa_doa_tbl$massa, harmonizar_sigla(massa_doa_tbl$partido))

z_rob_mig <- z_robusto_ca(set_names(tudo$migracao, tudo$partido), massa_mig)
z_rob_col <- z_robusto_ca(set_names(tudo$coligacao, tudo$partido), massa_col)
z_rob_doa <- z_robusto_ca(set_names(tudo$doacoes, tudo$partido), massa_doa)

tudo <- tudo %>%
  mutate(z_rob_migracao = as.numeric(z_rob_mig[partido]),
         z_rob_coligacao = as.numeric(z_rob_col[partido]),
         z_rob_doacoes = as.numeric(z_rob_doa[partido])) %>%
  rowwise() %>%
  # frentes e votações mantêm o z comum (coordenadas sem caudas selvagens)
  mutate(media_z_rob = mean(c(z_rob_migracao, z_rob_coligacao, z_rob_doacoes,
                              z_frentes, z_votacao), na.rm = TRUE)) %>%
  ungroup()

# classificação das divisas entre faixas vizinhas por sustentação
# (precedência: cobertura > corroborada > consenso)
ordem_pub <- arrange(tudo, media_rank)
saltos_zr <- diff(ordem_pub$media_z_rob)
salto_mediano_zr <- median(saltos_zr)
divisas_cls <- list()
for (i in seq_len(nrow(ordem_pub) - 1)) {
  if (ordem_pub$label[i] == ordem_pub$label[i + 1]) next
  razao <- saltos_zr[i] / salto_mediano_zr
  nd_min <- min(ordem_pub$n_dimensoes[c(i, i + 1)])
  tipo <- if (nd_min <= 2) "cobertura"
          else if (razao >= 2) "corroborada"
          else "consenso"
  divisas_cls[[length(divisas_cls) + 1]] <- tibble(
    divisa = paste(ordem_pub$partido[i], ordem_pub$partido[i + 1], sep = " · "),
    nivel = ifelse(ordem_pub$familia[i] != ordem_pub$familia[i + 1],
                   "família", "subfaixa"),
    salto_rank = round(ordem_pub$media_rank[i + 1] - ordem_pub$media_rank[i], 2),
    salto_z_rob = round(saltos_zr[i], 3),
    razao_vs_mediano = round(razao, 1),
    n_dim_minimo = nd_min,
    tipo = tipo)
}
divisas_cls <- bind_rows(divisas_cls)
write.csv(divisas_cls,
          glue("{dir_processado}/diagnostico_corroboracao_divisas.csv"),
          row.names = FALSE)

# robustez ----------------------------------------------------------------

# (i) concordância entre métodos no k publicado (Rand ajustado); checagem
# multivariada restrita a partidos com >= 3 dimensões (pares sempre com
# sobreposição de >= 2 dimensões; evita Gower NA, ex.: MISSÃO x PSTU)
idx3 <- tudo$n_dimensoes >= 3
dist_gower <- daisy(as.data.frame(mtx_dims[idx3, ]), metric = "gower")
stopifnot(!anyNA(as.matrix(dist_gower)))
ari <- c(
  ward_gower = adjustedRandIndex(tudo$cluster[idx3],
                                 cutree(hclust(dist_gower, "ward.D2"), K_FAIXAS)),
  pam_gower  = adjustedRandIndex(tudo$cluster[idx3],
                                 pam(dist_gower, K_FAIXAS, cluster.only = TRUE)),
  jenks_na_z = adjustedRandIndex(tudo$cluster,
                                 Ckmeans.1d.dp(posicoes_z, K_FAIXAS)$cluster)
)

# (ii) sensibilidade a k vizinhos (clusters; nomenclatura só existe p/ k=9)
sens_k <- purrr::map(c(K_FAIXAS - 1, K_FAIXAS + 1), function(k) {
  tibble(partido = names(posicoes),
         !!paste0("cluster_k", k) := Ckmeans.1d.dp(posicoes, k)$cluster)
}) %>%
  reduce(full_join, by = "partido")

# (iii) estabilidade sob a incerteza das cinco medições. As votações têm a
# posterior bayesiana do IRT; migração, frentes, coligações e doações vêm de
# Análise de Correspondência sobre contagens e a incerteza amostral delas
# sai do bootstrap não paramétrico rodado em 01/02/03/05, cada um com sua
# unidade de reamostragem própria (o par de candidaturas na migração, o
# deputado nas frentes, a chapa nas coligações, o doador nas doações).
# B = 1.000 em todas, mesma semente (20260705), rodadas independentes entre
# dimensões: são fontes de dado diferentes, não há razão para casar a rodada
# 37 de votação com a rodada 37 de migração. Votação: reamostra os pontos
# ideais dos deputados ~ N(Mean, SD) da posterior (aproximação normal,
# independência entre deputados).
dep <- fread(glue("{dir_resultado}/d1_ponto_ideal_deputados.csv")) %>%
  as_tibble() %>%
  filter(!is.na(partido), partido != "", partido != "S.PART.") %>%
  mutate(partido = harmonizar_sigla(partido)) %>%
  # piso de bancada: o loop reproduz a régua publicada, então o partido sem
  # ponto de votação também não entra na mediana por rodada
  filter(!partido %in% partidos_sub_piso)
stopifnot(all(c("Mean", "SD", "partido") %in% names(dep)))

# migração/frentes/coligações: Dim1 bruto de CA sobre dado reamostrado (sinal
# ainda não alinhado); ver bootstrap em 01/02/03. Cada matriz é partido x B.
ler_boot <- function(nome) {
  tb <- fread(glue("{dir_resultado}/{nome}")) %>% as_tibble()
  m <- as.matrix(tb[, -1])
  rownames(m) <- tb$partido
  m
}
boot_migracao_m  <- ler_boot("d1_migracao_partidaria_boot.csv")
boot_frentes_m   <- ler_boot("d1_frentes_politicos_boot.csv")
# piso de bancada: a linha do partido fora do piso sai da matriz de
# bootstrap das frentes, como saiu do ponto central
boot_frentes_m   <- boot_frentes_m[
  !rownames(boot_frentes_m) %in% partidos_sub_piso, , drop = FALSE]
boot_coligacao_m <- ler_boot("d1_coligacoes_boot.csv")
# doações: bootstrap de doador (multinomial, 05), mesma semente das demais
boot_doacoes_m   <- ler_boot("d1_doacoes_pf_boot.csv")

# alinha sinal (PT negativo) igual à convenção usada no ponto central
# (linhas 156-162): sem isso, ~metade das rodadas de CA sai espelhada.
alinhar_sinal_pt <- function(v) {
  pt <- v["PT"]
  if (!is.na(pt) && pt > 0) v <- v * -1
  v
}

# coluna b de uma matriz de bootstrap, alinhada aos partidos de `tudo` (NA
# para quem não apareceu naquela rodada, ex.: partido abaixo do corte de
# migração, ou sem deputado sorteado nas frentes)
coluna_boot <- function(m, b, partidos) {
  v <- set_names(rep(NA_real_, length(partidos)), partidos)
  comuns <- intersect(partidos, rownames(m))
  v[comuns] <- m[comuns, b]
  v
}

B <- 1000
label_base <- set_names(tudo$label, tudo$partido)
familia_base <- set_names(tudo$familia, tudo$partido)
manteve_label <- matrix(NA, nrow = nrow(tudo), ncol = B,
                        dimnames = list(tudo$partido, NULL))
manteve_familia <- manteve_label

# comparadores, na mesma reamostragem (não influenciam a regra publicada):
# o agrupamento direto re-derivado por rodada e a regra dos 2 maiores vãos
# (testada e rejeitada).
familia_base_direto <- set_names(tudo$familia_direto, tudo$partido)
manteve_familia_direto <- manteve_familia
gap2_base <- fronteira_gap2(posicoes)
manteve_familia_gap2 <- manteve_familia
par_gap2 <- character(B)
media_rank_sims <- matrix(NA_real_, nrow = nrow(tudo), ncol = B,
                          dimnames = list(tudo$partido, NULL))
# incerteza por régua, usada nos gráficos da matéria: guarda, por rodada, o
# rank escalado 1-100 (o eixo x das réguas) e a posição ordinal (a "15ª de
# 21" das legendas) de cada partido em cada dimensão. Só coleta o que o loop
# já calcula; nenhum sorteio novo, então o gerador de números aleatórios e,
# com ele, todos os números publicados ficam intactos.
rank_sims_dim <- purrr::map(set_names(dimensoes), ~ matrix(
  NA_real_, nrow = nrow(tudo), ncol = B, dimnames = list(tudo$partido, NULL)))
pos_sims_dim <- rank_sims_dim

for (b in seq_len(B)) {
  theta_b <- rnorm(nrow(dep), dep$Mean, dep$SD)
  votacao_b <- alinhar_sinal_pt(tapply(theta_b, dep$partido, median))
  migracao_b  <- alinhar_sinal_pt(coluna_boot(boot_migracao_m,  b, tudo$partido))
  frentes_b   <- alinhar_sinal_pt(coluna_boot(boot_frentes_m,   b, tudo$partido))
  coligacao_b <- alinhar_sinal_pt(coluna_boot(boot_coligacao_m, b, tudo$partido))
  doacoes_b   <- alinhar_sinal_pt(coluna_boot(boot_doacoes_m,   b, tudo$partido))
  tab_b <- tudo %>%
    select(partido) %>%
    mutate(
      migracao  = as.numeric(migracao_b[partido]),
      frentes   = as.numeric(frentes_b[partido]),
      coligacao = as.numeric(coligacao_b[partido]),
      votacao   = as.numeric(votacao_b[partido]),
      doacoes   = as.numeric(doacoes_b[partido])
    ) %>%
    compor_rank(dimensoes)
  # posição de cada partido em cada RÉGUA nesta rodada: o rank escalado que o
  # compor_rank já produziu e a posição ordinal correspondente (rank cru do
  # valor da dimensão, empates pelo mínimo, como no compor_rank)
  for (d in dimensoes) {
    rank_sims_dim[[d]][, b] <- set_names(tab_b[[paste0(d, "_rank")]],
                                         tab_b$partido)[tudo$partido]
    pos_sims_dim[[d]][, b] <- set_names(rank(tab_b[[d]], ties.method = "min",
                                             na.last = "keep"),
                                        tab_b$partido)[tudo$partido]
  }
  pos_b <- set_names(tab_b$media_rank, tab_b$partido)
  # partido sem nenhuma dimensão naquela rodada (o bootstrap tirou as poucas
  # que ele tem) vira NaN em media_rank e fica fora do agrupamento da rodada
  # (NaN quebraria o Ckmeans.1d.dp); conta como "não bateu" abaixo, via
  # !is.na(), sem entrar na conta de quem bateu.
  pos_b_validos <- pos_b[!is.nan(pos_b) & !is.na(pos_b)]
  # regra publicada: cortes de família fixos (os herdados no ponto central) +
  # subfaixas re-derivadas por rodada
  grupos_b <- rotular_ancorado(pos_b_validos, cortes_familia)
  lab_b <- set_names(grupos_b$label, grupos_b$partido)[tudo$partido]
  fam_b <- set_names(grupos_b$familia, grupos_b$partido)[tudo$partido]
  manteve_label[, b] <- !is.na(lab_b) & lab_b == label_base[tudo$partido]
  manteve_familia[, b] <- !is.na(fam_b) & fam_b == familia_base[tudo$partido]

  # comparador: agrupamento direto, re-derivado por rodada
  direto_b <- rotular_faixas(pos_b_validos)
  fam_direto_b <- set_names(direto_b$familia, direto_b$partido)[tudo$partido]
  manteve_familia_direto[, b] <- !is.na(fam_direto_b) & fam_direto_b == familia_base_direto[tudo$partido]

  media_rank_sims[, b] <- pos_b[tudo$partido]
  gap2_b <- fronteira_gap2(pos_b_validos)
  fam_gap2_b <- gap2_b$familia[tudo$partido]
  manteve_familia_gap2[, b] <- !is.na(fam_gap2_b) & fam_gap2_b == gap2_base$familia[tudo$partido]
  par_gap2[b] <- gap2_b$par
}
estabilidade <- tibble(
  partido = tudo$partido,
  pct_label_estavel = round(rowMeans(manteve_label) * 100, 1),
  pct_familia_estavel = round(rowMeans(manteve_familia) * 100, 1)
)

# intervalo de 90% da posição por régua: quantis 5% e 95% do rank escalado
# (1-100) e da posição ordinal, por partido e por régua, nas mesmas B
# rodadas acima. O ordinal usa quantil tipo 1 (estatística de ordem: devolve
# posições inteiras, que de fato ocorreram). O composto entra como "media",
# para a régua final.
pos_sims_media <- apply(media_rank_sims, 2, function(v)
  rank(v, ties.method = "min", na.last = "keep"))
q_linha <- function(m, prob, tipo) apply(m, 1, function(v) {
  v <- v[!is.na(v) & !is.nan(v)]
  if (!length(v)) return(NA_real_)
  quantile(v, prob, type = tipo, names = FALSE)
})
incerteza_reguas <- purrr::map_dfr(c(dimensoes, "media"), function(d) {
  m_rank <- if (d == "media") media_rank_sims else rank_sims_dim[[d]]
  m_pos  <- if (d == "media") pos_sims_media  else pos_sims_dim[[d]]
  tibble(
    dimensao  = d,
    partido   = tudo$partido,
    n_rodadas = as.integer(rowSums(!is.na(m_rank) & !is.nan(m_rank))),
    rank_p05  = round(q_linha(m_rank, .05, 7), 1),
    rank_p95  = round(q_linha(m_rank, .95, 7), 1),
    pos_p05   = as.integer(q_linha(m_pos, .05, 1)),
    pos_p95   = as.integer(q_linha(m_pos, .95, 1))
  ) %>% filter(n_rodadas > 0)
})
write.csv(incerteza_reguas,
          glue("{dir_processado}/diagnostico_incerteza_reguas.csv"),
          row.names = FALSE)

# margem de erro do corte traduzido: re-estima o estimador publicado
# (isotônica no 4,5; kernel depurado no 7,0) em cada uma das B reamostragens
# (escores externos fixos; posições da rodada). É a distribuição amostral
# dos dois cortes sob a incerteza das medições.
cortes_sims <- t(vapply(seq_len(B), function(b) {
  pos_b <- media_rank_sims[, b]
  pos_b <- pos_b[!is.na(pos_b) & !is.nan(pos_b)]
  tryCatch(cortes_publicados(pos_b, escores_bol), error = function(e) c(NA_real_, NA_real_))
}, numeric(2)))
ic_cortes <- apply(cortes_sims, 2, quantile, probs = c(.05, .5, .95), na.rm = TRUE)

# o número que conta a história é quantos partidos mudam de família por
# rodada. Regra publicada (cortes fixos) vs os dois comparadores: o
# agrupamento direto re-derivado e os 2 maiores vãos (rejeitada).
n_dif_ancorado <- colSums(!manteve_familia)
n_dif_direto <- colSums(!manteve_familia_direto)
n_dif_gap2 <- colSums(!manteve_familia_gap2)

write_json(
  list(
    seed = 20260705L, B = B,
    partidos = tudo$partido,
    media_rank_base = round(as.numeric(posicoes[tudo$partido]), 3),
    familia_base = tudo$familia,
    familia_base_direto = tudo$familia_direto,
    cortes_ancorados = list(
      esquerda_centro = round(cortes_familia[1], 3),
      centro_direita = round(cortes_familia[2], 3)),
    cortes_ic90 = list(
      esquerda_centro = round(as.numeric(ic_cortes[c(1, 3), 1]), 2),
      centro_direita = round(as.numeric(ic_cortes[c(1, 3), 2]), 2)),
    ancorado_dif_media = round(mean(n_dif_ancorado), 2),
    ancorado_dif_mediana = median(n_dif_ancorado),
    direto_dif_media = round(mean(n_dif_direto), 2),
    direto_dif_mediana = median(n_dif_direto),
    gap2_par_base = gap2_base$par,
    gap2_dif_media = round(mean(n_dif_gap2), 2),
    gap2_dif_mediana = median(n_dif_gap2),
    simulacoes = lapply(seq_len(B), function(b) list(
      media_rank = round(as.numeric(media_rank_sims[tudo$partido, b]), 3),
      ancorado_n_dif = as.integer(n_dif_ancorado[b]),
      direto_n_dif = as.integer(n_dif_direto[b]),
      gap2_n_dif = as.integer(n_dif_gap2[b]),
      gap2_par = par_gap2[b]
    ))
  ),
  glue("{dir_processado}/diagnostico_simulacoes_fronteira.json"),
  auto_unbox = TRUE, digits = 3
)

# (iv) leave-one-dimension-out (refaz o rank composto sem cada dimensão)
loo <- purrr::map(dimensoes, function(d) {
  dims_resto <- setdiff(dimensoes, d)
  tab_loo <- tudo %>%
    select(partido, all_of(dims_resto)) %>%
    compor_rank(dims_resto)
  ok <- !is.nan(tab_loo$media_rank)
  grupos_loo <- rotular_ancorado(set_names(tab_loo$media_rank[ok],
                                           tab_loo$partido[ok]), cortes_familia)
  grupos_loo %>% select(partido, !!paste0("familia_sem_", d) := familia)
}) %>%
  reduce(full_join, by = "partido")

# (v) exclusão dos partidos com 1 só dimensão
bem_medidos <- tudo$n_dimensoes >= 2
grupos_bm <- rotular_ancorado(posicoes[bem_medidos], cortes_familia)
mudou_bm <- grupos_bm %>%
  left_join(select(tudo, partido, familia), by = "partido",
            suffix = c("_sub", "_base")) %>%
  filter(familia_sub != familia_base) %>%
  pull(partido)

robustez <- tudo %>%
  select(partido, n_dimensoes, familia, label, label_direto, familia_direto,
         label_z_ancoras) %>%
  left_join(estabilidade, by = "partido") %>%
  left_join(sens_k, by = "partido") %>%
  left_join(loo, by = "partido")
write.csv(robustez, glue("{dir_processado}/diagnostico_robustez.csv"),
          row.names = FALSE)

# salva resultados --------------------------------------------------------
tabela_final <- tudo %>%
  select(partido, all_of(dimensoes), all_of(cols_z), ends_with("_rank"),
         n_dimensoes, media_z, z_rob_migracao, z_rob_coligacao, z_rob_doacoes,
         media_z_rob, cluster, familia, label) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write.csv(tabela_final, glue("{dir_processado}/tabela_final.csv"),
          row.names = FALSE)

resumo <- c(
  glue("# Diagnóstico das faixas ({format(Sys.Date(), '%Y-%m-%d')})"),
  "",
  "- Faixas publicadas: famílias por cortes herdados do levantamento de especialistas",
  glue("  (isotônica na banda 4,5; kernel depurado na banda 7,0; cortes na nossa régua: {round(cortes_familia[1], 2)} e {round(cortes_familia[2], 2)});"),
  glue("  subfaixas por Ckmeans dentro de cada família (k editorial {paste(K_SUB_EDITORIAL, collapse = '/')})."),
  glue("- Concordância de família (regra publicada): Bolognesi {sum(concord$concorda_bol, na.rm = TRUE)}/{sum(!is.na(concord$bol_fold))}, BLS9 {sum(concord$concorda_bls, na.rm = TRUE)}/{sum(!is.na(concord$bls_fold))}; agrupamento direto: {sum(concord$concorda_bol_direto, na.rm = TRUE)} e {sum(concord$concorda_bls_direto, na.rm = TRUE)}."),
  glue("- Perda de WSS vs partição plana ótima do mesmo k: {percent(perda_wss, accuracy = 0.1)}; ARI entre as duas: {round(ari_aninhado_flat, 3)}."),
  glue("- Evidência de estrutura na escala (contexto; as famílias não saem daqui):"),
  glue("  gap statistic = {k_gap}; BIC mclust = {k_mclust}; BIC Ckmeans = {k_ckmeans}; máx. silhueta = {k_range[which.max(silhuetas)]}."),
  glue("  O espectro é um contínuo; as famílias vêm do significado externo, as subfaixas das quebras."),
  glue("- Concordância (ARI): Ward/Gower = {round(ari[1], 3)}; PAM/Gower = {round(ari[2], 3)}; Jenks na escala z = {round(ari[3], 3)}"),
  glue("- Correlação média |r| entre as {length(dimensoes)} dimensões: {round(mean(abs(cors[upper.tri(cors)])), 3)}"),
  glue("- Correlação média de ranking (Spearman) entre as {length(dimensoes)} dimensões: {round(mean(abs(cors_rank[upper.tri(cors_rank)])), 4)} (faixa {round(min(cors_rank[upper.tri(cors_rank)]), 3)} a {round(max(cors_rank[upper.tri(cors_rank)]), 3)}); é o número compatível com a escala publicada, media_rank"),
  glue("- Estabilidade sob a incerteza das medições (B = {B}): {sum(estabilidade$pct_familia_estavel >= 90)}/{nrow(estabilidade)} partidos com >= 90% na família; {sum(estabilidade$pct_label_estavel >= 90)}/{nrow(estabilidade)} com >= 90% na faixa fina"),
  "",
  glue("- Divisas por sustentação (os espaços do composto medem acordo entre as medições, não distância):"),
  glue("  {paste(divisas_cls$divisa, '=', divisas_cls$tipo, collapse = ' | ')}"),
  "",
  "Detalhes: diagnostico_selecao_k.csv, diagnostico_robustez.csv,",
  "diagnostico_correlacao_dimensoes.csv, diagnostico_corroboracao_divisas.csv,",
  "diagnostico_posicao_clusters.png"
)
writeLines(resumo, glue("{dir_processado}/diagnostico_resumo.md"))

# gráfico -------------------------------------------------------------------------
fronteiras <- tudo %>%
  group_by(cluster) %>%
  summarise(fim = max(media_rank), ini = min(media_rank), .groups = "drop") %>%
  arrange(cluster)
cortes <- head(fronteiras$fim, -1) + (tail(fronteiras$ini, -1) - head(fronteiras$fim, -1)) / 2

niveis_label <- tudo %>% arrange(cluster) %>% pull(label) %>% unique()
p <- tudo %>%
  mutate(partido = factor(partido, levels = partido),
         label = factor(label, levels = niveis_label)) %>%
  ggplot(aes(x = media_rank, y = partido, color = label)) +
  geom_vline(xintercept = cortes, linetype = "dashed",
             color = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = cortes_familia, color = "grey30", linewidth = 0.4) +
  geom_point(size = 3) +
  labs(title = glue("Espectro partidário 2026: famílias ancoradas externamente + subfaixas por coesão ({K_FAIXAS} faixas)"),
       subtitle = "Escala: ranking composto 1-100; fronteiras de família herdadas do levantamento de especialistas (isotônica/kernel); subfaixas onde as quebras internas caem",
       x = "ranking composto (1 = mais à esquerda, 100 = mais à direita)", y = NULL,
       color = "faixa") +
  theme_minimal(base_size = 11)
ggsave(glue("{dir_processado}/diagnostico_posicao_clusters.png"), p,
       width = 10, height = 8, dpi = 120)
