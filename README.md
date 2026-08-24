# Espectro Político-Partidário Brasileiro em 2026

## Sobre o Projeto

Este projeto visa analisar e quantificar a proximidade dos partidos políticos brasileiros em diferentes dimensões, utilizando dados públicos. A análise busca capturar as relações entre os partidos em várias esferas de atuação política e ordená-los numa régua de 1 a 100, do mais à esquerda ao mais à direita, dividida em nove faixas nomeadas.

Esta é a edição de 2026 do projeto, sucessora de [proximidade-partidaria-2024](https://github.com/deltafolha/proximidade-partidaria-2024).

## Metodologia

Utilizamos cinco fontes principais de dados para nossa análise:

1. **Migração Partidária**: analisamos os padrões de mudança de partido entre os candidatos usando dados do TSE para as eleições de 2022, 2024 e 2026. Todo par de participações consecutivas do mesmo candidato conta como um evento: quem troca de sigla tende a ir para partidos próximos.

2. **Frentes Parlamentares**: examinamos a participação das bancadas nas centenas de frentes temáticas da Câmara dos Deputados na 57ª legislatura.

3. **Coligações Eleitorais**: observamos as alianças formadas pelos partidos nas eleições municipais de 2024 e nas disputas majoritárias de 2026 (presidente, governador e senador), utilizando dados do TSE. Federações partidárias contam como aliança.

4. **Votações na Câmara**: analisamos o padrão de todos os votos nominais de sim e não em plenário, de janeiro de 2023 até o último mês completo antes da execução (na matéria, julho de 2026), usando a API de Dados Abertos da Câmara.

5. **Doações de Pessoas Físicas**: verificamos quais partidos dividem os mesmos doadores, usando a prestação de contas eleitorais de candidatos do TSE em 2022 e 2024. Essa janela fica deliberadamente um ciclo atrás das demais: o arquivo de um ano eleitoral só se completa com a prestação final de contas, em dezembro.

Para migração, coligações e doações, aplicamos a técnica de Análise de Correspondência (CA) para reduzir a dimensionalidade dos dados e extrair a principal tendência de posicionamento dos partidos. Nas matrizes quadradas de partido por partido, a diagonal é reconstituída por quasi-independência, para que a relação de cada partido consigo mesmo não distorça o resultado. Nas doações, a matriz é de doador por partido, com célula binária: vale a pessoa que doou, não o valor doado.

Para as frentes, aplicamos a CA na matriz de partido por frente.

Para as votações na Câmara, utilizamos MCMC IRT (Markov Chain Monte Carlo Item Response Theory) para obter o posicionamento, considerando a ausência de dados em votações e a natureza binária dos votos. O modelo é identificado por dois deputados-âncora escolhidos programaticamente (o de maior participação do PT, com sinal negativo, e o do Novo, positivo) e roda em duas cadeias independentes; a execução aborta se os diagnósticos de convergência reprovarem.

Nas duas dimensões medidas pelos deputados (votações e frentes), o ponto de um partido só é publicado se a bancada tem ao menos 3 deputados. Com menos que isso, o agregado é o indivíduo: a mediana de 1 ou 2 deputados não resiste a uma única saída, o bootstrap por deputado degenera e a margem de erro sai com largura zero exatamente onde o dado é mais fino. A estimação continua usando todos os deputados; o piso vale só para o ponto partidário. Na régua desta edição, apenas o Missão (1 deputado) fica abaixo do piso, e por isso entra só com a dimensão de migração. As demais dimensões têm pisos próprios, declarados nos scripts (mais de 10 migrações; ao menos 30 doadores).

Para combinar as cinco medidas, o código segue os seguintes passos:

1. Para cada medida, os partidos são classificados (ranked) com base em sua posição relativa.
2. Esses rankings são então normalizados para uma escala de 1 a 100, onde 1 representa a posição mais à esquerda e 100 a posição mais à direita no espectro político.
3. O posicionamento final de cada partido é determinado pela média aritmética simples dos rankings normalizados disponíveis (cinco para a maioria dos partidos).

As faixas nomeadas têm duas camadas, de natureza diferente:

- As três **famílias** (esquerda, centro e direita) têm significado público absoluto, e por isso os cortes não saem da forma da nossa distribuição: são herdados de um instrumento externo, o [levantamento de especialistas de Bolognesi, Ribeiro, Codato e Silva](https://periodicos.sbu.unicamp.br/ojs/index.php/op/article/view/8682072), que pediu a cientistas políticos que dessem uma nota de 0 a 10 a cada partido. As fronteiras das famílias na nossa régua ficam onde a tradução estatística entre as duas escalas (equating por regressão isotônica na fronteira esquerda e centro, e por kernel equipercentil na fronteira centro e direita) cruza as bandas fixas de 4,5 e 7,0 do próprio instrumento. As notas dos dois levantamentos usados na validação acompanham o repositório, em `dados/brutos/dados-externos/`.
- As três **subfaixas** dentro de cada família são gradação relativa ("os mais parecidos entre si"): quebras naturais encontradas por particionamento ótimo unidimensional (equivalente ao método de Jenks), com três subfaixas por família.

A incerteza de toda a régua é medida por bootstrap não paramétrico em cada dimensão (a unidade de reamostragem é o par de candidaturas na migração, o deputado nas frentes, a chapa nas coligações e o doador nas doações) e pela distribuição posterior do IRT nas votações, com mil recálculos e semente fixa. Toda estatística de estabilidade publicada sai desse mesmo conjunto de simulações.

## O que faz cada código

- `00_config.R`: configuração compartilhada por todos os scripts. Diretórios, harmonização de siglas (fusões e incorporações homologadas pelo TSE até julho de 2026), a chave `baixar_novos_dados` (ver abaixo), funções de download com retry e a reconstituição de diagonal usada nas CAs.
- `01_migracao_partidaria.R`: baixa os arquivos de candidatos do TSE, monta os pares de participações consecutivas de cada candidato, tabula as trocas de partido, roda a CA e o bootstrap. Partidos com 10 ou menos migrações ficam de fora da dimensão.
- `02_frentes_parlamentares.R`: baixa a composição das frentes parlamentares da 57ª legislatura no site da Câmara, monta a matriz de partido por frente, roda a CA e o bootstrap por deputado.
- `03_coligacoes.R`: baixa os arquivos de coligações do TSE, monta os pares de partidos que dividiram chapa (2024 municipais e 2026 majoritárias), roda a CA e o bootstrap por chapa.
- `04_votacoes_deputados.R`: baixa a lista de votações nominais em plenário e o voto de cada deputado pela API da Câmara, monta a matriz binária de deputado por votação e estima o IRT (duas cadeias, diagnósticos de convergência obrigatórios). A posição de cada partido é a mediana dos seus deputados.
- `05_doacoes.R`: constrói a base de doador por partido a partir da prestação de contas do TSE (só pessoa física com CPF válido, financiamento coletivo atribuído ao doador originário, sem autofinanciamento e com piso de R$ 1 por relação doador-partido, que barra a campanha de trote de doar um centavo ao adversário), roda a CA exata e o bootstrap de doador.
- `06_juntando_tudo.R`: junta as cinco dimensões, alinha os sinais (PT sempre à esquerda), calcula os rankings normalizados e a média composta, herda os cortes de família do instrumento externo, encontra as subfaixas, roda as mil simulações de estabilidade e escreve `dados/processado/tabela_final.csv` e todos os diagnósticos.
- `07_robustez.R`: suíte de auditoria, que não altera nenhuma saída publicada. Refaz a análise sob escolhas metodológicas alternativas (outros tratamentos da diagonal, outros cortes de entrada, outras formas de compor a régua) e mede quanto o resultado muda.

## Interpretando os Resultados

Cada dimensão mede um comportamento com incentivos próprios. A participação em frentes parlamentares reflete afinidade ideológica, mas também busca de visibilidade e de influência legislativa; as votações na Câmara respondem a dinâmicas conjunturais; as migrações partidárias misturam afinidade com interesse prático individual; as coligações equilibram ideologia com estratégia eleitoral; e a fidelidade dos doadores mistura convicção com a visibilidade das campanhas.

A régua composta, portanto, mede proximidade revelada por comportamento, não ideologia declarada. Os espaços entre partidos vizinhos medem o acordo entre as cinco medições, não distância ideológica: quando as cinco concordam, o partido fica isolado dos vizinhos; quando discordam, os partidos se embolam.

A régua foi validada contra dois levantamentos de especialistas independentes: a correlação da posição composta é de 0,92 com o [levantamento de Bolognesi, Ribeiro, Codato e Silva](https://periodicos.sbu.unicamp.br/ojs/index.php/op/article/view/8682072) e de 0,90 com a nona onda da [Pesquisa Legislativa Brasileira](https://dataverse.harvard.edu/dataverse/bls), de Timothy Power e Cesar Zucco. Nas famílias, a classificação concorda com o primeiro em 25 de 28 partidos e com a segunda em 14 de 17.

## Limitações e Considerações

- O método de ranking e normalização usado para combinar as medidas preserva a ordem, mas não a magnitude das diferenças entre os partidos.
- Partidos pequenos aparecem em poucas dimensões (PSTU, UP e PCB em duas; o Missão, criado em 2025, em três) e suas posições carregam mais incerteza, quantificada nas simulações de `06_juntando_tudo.R`.
- A dimensão de doações usa a janela 2022 e 2024, um ciclo atrás das demais, porque o arquivo do ano eleitoral corrente só se completa em dezembro.

## Reproduzindo os Resultados

1. Clone o repositório:

```
git clone https://github.com/deltafolha/gps-partidario-2026.git
```

2. Vá para a pasta da análise:

```
cd gps-partidario-2026/
```

3. No R, instale os pacotes que vamos utilizar:

```r
packages <- c("dplyr", "stringr", "purrr", "data.table", "tidyr", "tibble",
              "ca", "magrittr", "glue", "rvest", "jsonlite", "httr",
              "lubridate", "MCMCpack", "coda", "scales", "cluster",
              "Ckmeans.1d.dp", "mclust", "Matrix", "arrow", "lavaan",
              "ggplot2")
install.packages(packages)
```

4. (opcional) Caso queira reproduzir os dados da matéria, e não atualizar a análise com dados mais atuais, baixe os dados brutos utilizados na época:

```
pip install gdown
gdown 1pZcnlC7pxHKEUOBMPyvZpoq9uu4mOTeH -O ./dados_congelados.zip
unzip ./dados_congelados.zip
```

e altere a seguinte linha, presente no código `00_config.R`:

de

```r
baixar_novos_dados <- TRUE
```

para

```r
baixar_novos_dados <- FALSE
```

Com a chave em `TRUE` (o padrão), os scripts baixam os dados mais recentes do TSE e da Câmara na primeira execução (cerca de 2,3 GB) e as janelas temporais avançam sozinhas: a migração e as coligações incorporam as atualizações que o TSE publicar até novembro de 2026, as votações vão até o último mês completo, e as frentes novas entram no cache. Com a chave em `FALSE`, nada é baixado e as janelas ficam travadas nas da publicação, o que reproduz exatamente os números da matéria.

5. Execute os scripts presentes na pasta `codigos` pela ordem, a partir da raiz do repositório:

```
Rscript codigos/01_migracao_partidaria.R
Rscript codigos/02_frentes_parlamentares.R
Rscript codigos/03_coligacoes.R
Rscript codigos/04_votacoes_deputados.R
Rscript codigos/05_doacoes.R
Rscript codigos/06_juntando_tudo.R
Rscript codigos/07_robustez.R
```

A etapa mais demorada é a `04`: o MCMC roda duas cadeias de 100 mil iterações e leva em torno de uma hora. O script `07` é opcional para quem só quer a tabela final: roda a auditoria de robustez e apenas lê o que os anteriores produziram.

Após isso será criada a tabela `dados/processado/tabela_final.csv` (uma cópia acompanha este repositório), além dos arquivos de diagnóstico em `dados/processado/`.

## Resultado (agosto de 2026)

| faixa | partidos |
| --- | --- |
| esquerda_1 | PSTU, UP |
| esquerda_2 | PCB, PSOL, PT, PC do B, REDE |
| esquerda_3 | PV, PSB, PDT |
| centro_1 | MDB, PSD, AVANTE |
| centro_2 | SOLIDARIEDADE |
| centro_3 | AGIR, MOBILIZA |
| direita_1 | PP, CIDADANIA, PRD, REPUBLICANOS, PODE |
| direita_2 | UNIÃO, PSDB, PMB, DC |
| direita_3 | MISSÃO, PRTB, PL, NOVO |

Em números: a correlação média de ranking entre as cinco dimensões, tomadas duas a duas, é de 0,89 (de 0,82 a 0,97). Sob as mil simulações de incerteza, 28 dos 29 partidos permanecem na mesma família em pelo menos 90% dos recálculos, e em média 0,2 partido muda de família por recálculo. Os cortes de família herdados do instrumento externo caem em 37,2 (entre PDT e MDB) e 58,8 (entre MOBILIZA e PP) na escala de 1 a 100.

A tabela final completa, com a posição de cada partido em cada dimensão (colunas de coordenadas), o ranking normalizado correspondente (colunas `_rank`), a média composta e a faixa:

|partido       | migracao| frentes| coligacao| votacao| doacoes| migracao_rank| frentes_rank| coligacao_rank| votacao_rank| doacoes_rank| media_rank|label      |
|:-------------|--------:|-------:|---------:|-------:|-------:|-------------:|------------:|--------------:|------------:|------------:|----------:|:----------|
|PSTU          |       NA|      NA|     -6.30|      NA|   -3.77|            NA|           NA|           1.00|           NA|         1.00|       1.00|esquerda_1 |
|UP            |       NA|      NA|     -5.64|      NA|   -2.91|            NA|           NA|           4.67|           NA|         4.67|       4.67|esquerda_1 |
|PCB           |       NA|      NA|     -5.35|      NA|   -2.74|            NA|           NA|           8.33|           NA|         8.33|       8.33|esquerda_2 |
|PSOL          |    -4.67|   -2.35|     -2.79|   -1.04|   -2.38|          4.96|         1.00|          12.00|        20.80|        12.00|      10.15|esquerda_2 |
|PT            |    -3.62|   -1.95|     -1.67|   -1.56|   -2.22|          8.92|        10.90|          19.33|         1.00|        15.67|      11.16|esquerda_2 |
|PC do B       |    -4.95|   -1.91|     -1.67|   -1.37|   -1.76|          1.00|        15.85|          19.33|         5.95|        19.33|      12.29|esquerda_2 |
|REDE          |    -2.54|   -1.99|     -2.79|   -1.24|   -1.41|         12.88|         5.95|          12.00|        10.90|        23.00|      12.95|esquerda_2 |
|PV            |    -1.60|   -0.71|     -1.67|   -1.11|   -0.94|         16.84|        20.80|          19.33|        15.85|        30.33|      20.63|esquerda_3 |
|PSB           |    -1.34|   -0.55|     -0.17|   -0.80|   -1.01|         20.80|        25.75|          30.33|        25.75|        26.67|      25.86|esquerda_3 |
|PDT           |    -0.95|   -0.42|      0.00|   -0.75|   -0.58|         24.76|        35.65|          34.00|        30.70|        34.00|      31.82|esquerda_3 |
|MDB           |    -0.09|    0.19|      0.27|   -0.30|   -0.20|         32.68|        55.45|          37.67|        45.55|        41.33|      42.54|centro_1   |
|PSD           |     0.05|    0.04|      0.31|   -0.29|   -0.22|         44.56|        40.60|          41.33|        50.50|        37.67|      42.93|centro_1   |
|AVANTE        |     0.13|   -0.48|      0.45|   -0.41|   -0.19|         56.44|        30.70|          48.67|        35.65|        45.00|      43.29|centro_1   |
|SOLIDARIEDADE |    -0.01|    0.05|      0.34|   -0.26|   -0.18|         36.64|        45.55|          45.00|        55.45|        52.33|      46.99|centro_2   |
|AGIR          |    -0.01|      NA|      0.56|      NA|   -0.18|         40.60|           NA|          63.33|           NA|        56.00|      53.31|centro_3   |
|MOBILIZA      |     0.06|      NA|      0.56|      NA|   -0.14|         48.52|           NA|          59.67|           NA|        59.67|      55.95|centro_3   |
|PP            |     0.27|    0.35|      0.52|   -0.16|   -0.05|         60.40|        70.30|          52.33|        70.30|        67.00|      64.07|direita_1  |
|CIDADANIA     |    -0.14|    0.38|      0.80|    0.02|   -0.18|         28.72|        80.20|          85.33|        85.15|        48.67|      65.61|direita_1  |
|PRD           |     0.66|    0.18|      0.71|   -0.39|    0.12|         84.16|        50.50|          78.00|        40.60|        81.67|      66.99|direita_1  |
|REPUBLICANOS  |     0.43|    0.33|      0.53|   -0.17|   -0.01|         76.24|        65.35|          56.00|        65.35|        74.33|      67.45|direita_1  |
|PODE          |     0.42|    0.21|      0.59|   -0.19|    0.04|         72.28|        60.40|          67.00|        60.40|        78.00|      67.62|direita_1  |
|UNIÃO         |     0.37|    0.35|      0.60|   -0.16|   -0.02|         68.32|        75.25|          70.67|        75.25|        70.67|      72.03|direita_2  |
|PSDB          |     0.12|    0.40|      0.80|   -0.15|   -0.11|         52.48|        85.15|          85.33|        80.20|        63.33|      73.30|direita_2  |
|PMB           |     0.36|      NA|      0.64|      NA|    0.75|         64.36|           NA|          74.33|           NA|        92.67|      77.12|direita_2  |
|DC            |     0.49|      NA|      0.74|      NA|    0.16|         80.20|           NA|          81.67|           NA|        85.33|      82.40|direita_2  |
|MISSÃO        |     0.84|    0.82|        NA|    1.08|      NA|         92.08|        90.10|             NA|        90.10|           NA|      90.76|direita_3  |
|PRTB          |     0.76|      NA|      0.86|      NA|    0.88|         88.12|           NA|          92.67|           NA|       100.00|      93.60|direita_3  |
|PL            |     0.89|    1.12|      1.09|    1.36|    0.78|         96.04|        95.05|          96.33|        95.05|        96.33|      95.76|direita_3  |
|NOVO          |     0.99|    1.29|      1.31|    1.85|    0.27|        100.00|       100.00|         100.00|       100.00|        89.00|      97.80|direita_3  |
