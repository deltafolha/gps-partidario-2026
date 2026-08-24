# pacotes -----------------------------------------------------------------
library(data.table)
library(Matrix)
library(arrow)
library(glue)

source("codigos/00_config.R")

# variáveis ---------------------------------------------------------------

# Janela 2022+2024, deliberadamente um ciclo atrás das demais dimensões do
# TSE: o arquivo de um ano eleitoral se acumula durante a própria campanha e
# só fecha com a prestação final de contas, em dezembro; uma foto de
# setembro mediria quem doa cedo. Desliza para 2024+2026 na atualização
# pós-eleição.
anos_doacoes <- c(2022, 2024)
dir_doacoes_brutos <- file.path(dir_brutos, "doacoes")
dir_doacoes <- file.path(dir_interim, "doacoes")

min_doadores <- 30
n_boot <- 1000

# Piso de R$ 1 por relação doador-sigla: existe uma campanha de trote de
# doar R$ 0,01 ao adversário, na crença de que processar a doação custa mais
# ao partido do que ela rende (em 2022 o PL recebeu 4.617 doadores de até 5
# centavos contra 10 do PT). São eleitores hostis, não apoiadores, e
# criariam pares falsos entre os extremos. O piso é por relação (a soma do
# que o CPF deu àquela sigla na janela), não por doação, então a vaquinha
# legítima de R$ 1,00 fica toda.
min_valor_relacao <- 1

# cria diretorios ---------------------------------------------------------
for (d in c(dir_doacoes_brutos, dir_doacoes))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# base doador x partido ---------------------------------------------------

# Constrói do zip do TSE se o parquet não existir (a primeira execução baixa
# ~1,7 GB; em modo reprodução os parquets vêm nos dados congelados). Só
# pessoa física com CPF válido de 11 dígitos; financiamento coletivo entra
# pelo doador originário (o CPF de quem doou na vaquinha); sem
# autofinanciamento (CPF do doador igual ao do candidato).
so_digitos <- function(x) gsub("[^0-9]", "", x)

url_prestacao <- function(ano)
  glue("https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/",
       "prestacao_de_contas_eleitorais_candidatos_{ano}.zip")

construir_parquet <- function(ano) {
  destino <- file.path(dir_doacoes, glue("doacoes_pf_{ano}.parquet"))
  if (file.exists(destino)) return(destino)
  path_zip <- file.path(dir_doacoes_brutos,
                        glue("prestacao_de_contas_eleitorais_candidatos_{ano}.zip"))
  if (!file.exists(path_zip)) {
    if (!baixar_novos_dados) {
      stop(glue("Modo reprodução e parquet/zip de doações {ano} ausentes. Os ",
                "parquets fazem parte dos dados congelados (ver README)."))
    }
    cat("   |- Baixando prestação de contas de", ano, "\n")
    download.file(url_prestacao(ano), path_zip, mode = "wb", quiet = TRUE)
  }
  ler <- function(arquivo, colunas)
    fread(cmd = glue("unzip -p '{path_zip}' '{arquivo}'"), sep = ";",
          encoding = "Latin-1", select = colunas, colClasses = "character",
          quote = "\"", showProgress = FALSE)

  rec <- ler(glue("receitas_candidatos_{ano}_BRASIL.csv"),
             c("SG_PARTIDO", "NR_CPF_CANDIDATO", "DS_ORIGEM_RECEITA",
               "NR_CPF_CNPJ_DOADOR", "SQ_RECEITA", "VR_RECEITA"))
  rec[, valor := as.numeric(gsub(",", ".", VR_RECEITA))]
  rec <- rec[!is.na(valor) & valor > 0]

  pf <- rec[DS_ORIGEM_RECEITA %in% c("Recursos de pessoas físicas",
                                     "Doações pela Internet"),
            .(cpf_doador = so_digitos(NR_CPF_CNPJ_DOADOR),
              cpf_candidato = so_digitos(NR_CPF_CANDIDATO),
              partido = trimws(SG_PARTIDO), valor)]

  fc_receitas <- rec[grepl("Financiamento Coletivo", DS_ORIGEM_RECEITA,
                           ignore.case = TRUE),
                     .(SQ_RECEITA, partido = trimws(SG_PARTIDO),
                       cpf_candidato = so_digitos(NR_CPF_CANDIDATO))]
  if (nrow(fc_receitas) > 0) {
    orig <- ler(glue("receitas_candidatos_doador_originario_{ano}_BRASIL.csv"),
                c("NR_CPF_CNPJ_DOADOR_ORIGINARIO", "TP_DOADOR_ORIGINARIO",
                  "SQ_RECEITA", "VR_RECEITA"))
    orig[, valor := as.numeric(gsub(",", ".", VR_RECEITA))]
    orig <- orig[!is.na(valor) & valor > 0]
    orig <- orig[!grepl("^J", toupper(trimws(TP_DOADOR_ORIGINARIO)))]
    fc <- merge(orig, fc_receitas, by = "SQ_RECEITA")
    pf <- rbindlist(list(pf,
      fc[, .(cpf_doador = so_digitos(NR_CPF_CNPJ_DOADOR_ORIGINARIO),
             cpf_candidato, partido, valor)]), use.names = TRUE)
  }

  pf <- pf[nchar(cpf_doador) == 11 & !grepl("^(\\d)\\1{10}$", cpf_doador)]
  pf <- pf[cpf_doador != cpf_candidato]
  base <- pf[, .(valor = sum(valor), n_doacoes = .N), by = .(cpf_doador, partido)]
  write_parquet(base, destino)
  destino
}

for (ano in anos_doacoes) {
  construir_parquet(ano)
}

# formata para analise ----------------------------------------------------

# Junta os anos, aplica o piso e remove partidos com poucos doadores
base <- rbindlist(lapply(anos_doacoes, function(a)
  setDT(read_parquet(file.path(dir_doacoes, glue("doacoes_pf_{a}.parquet"))))))
base[, partido := harmonizar_sigla(partido)]
base <- base[, .(valor = sum(valor)), by = .(cpf_doador, partido)]

base <- base[valor >= min_valor_relacao]

n_por_partido <- base[, .(n = uniqueN(cpf_doador)), by = partido]
base <- base[partido %in% n_por_partido[n >= min_doadores, partido]]

# análise de correspondência ----------------------------------------------

# CA exata via produto cruzado partido x partido: escala para 1 milhão de
# linhas sem aproximação. A célula é binária: 1 se o CPF doou àquela sigla
# na janela, 0 se não. A unidade da régua é a pessoa, não o dinheiro: o que
# o registro mede é o ato político de doar.
doadores <- sort(unique(base$cpf_doador))
partidos_ca <- sort(unique(base$partido))
X <- Matrix::sparseMatrix(
  i = match(base$cpf_doador, doadores),
  j = match(base$partido, partidos_ca),
  x = rep(1, nrow(base)),
  dims = c(length(doadores), length(partidos_ca)),
  dimnames = list(NULL, partidos_ca)
)

ca_colunas <- function(X, pesos_doador = NULL) {
  if (is.null(pesos_doador)) pesos_doador <- rep(1, nrow(X))
  rs <- Matrix::rowSums(X)
  ok <- rs > 0 & pesos_doador > 0
  Xo <- X[ok, , drop = FALSE]; rs <- rs[ok]; m <- pesos_doador[ok]
  N  <- sum(m * rs)
  cj <- as.numeric(Matrix::colSums(Xo * m)) / N
  if (any(cj == 0)) return(NULL)
  B  <- as.matrix(Matrix::crossprod(Xo, Xo * (m / rs)))
  M  <- diag(1 / sqrt(cj)) %*% (B / N - tcrossprod(cj)) %*% diag(1 / sqrt(cj))
  eig <- eigen(M, symmetric = TRUE)
  list(partidos = colnames(Xo), phi1 = eig$vectors[, 1] / sqrt(cj), massa = cj)
}

# salva o resultado (sinal bruto; o 05 alinha pela convenção do PT)
principal <- ca_colunas(X)
fwrite(data.table(partido = principal$partidos,
                  Dim1 = round(principal$phi1, 6)),
       file.path(dir_resultado, "d1_doacoes_pf.csv"))
fwrite(data.table(partido = principal$partidos,
                  massa = round(principal$massa, 6)),
       file.path(dir_resultado, "tbl_doacoes_massa.csv"))

# bootstrap ---------------------------------------------------------------

# Incerteza da dimensão: bootstrap de doador, multinomial (cada rodada
# re-pesa os doadores como uma reamostragem com reposição). Sinal bruto por
# rodada, como nas outras CAs.
set.seed(20260705)
n_d <- nrow(X)
boot <- matrix(NA_real_, nrow = length(partidos_ca), ncol = n_boot,
               dimnames = list(partidos_ca, NULL))
for (b in seq_len(n_boot)) {
  pesos <- as.numeric(rmultinom(1, n_d, rep(1 / n_d, n_d)))
  r <- ca_colunas(X, pesos_doador = pesos)
  if (is.null(r)) next
  boot[match(r$partidos, partidos_ca), b] <- r$phi1
  if (b %% 100 == 0) cat("   |- Bootstrap:", b, "/", n_boot, "\n")
}
tb_boot <- data.table(partido = partidos_ca)
for (b in seq_len(n_boot)) tb_boot[[paste0("b", b)]] <- round(boot[, b], 6)
fwrite(tb_boot, file.path(dir_resultado, "d1_doacoes_pf_boot.csv"))
