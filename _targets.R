# =============================================================================
# _targets.R
# Pipeline {targets} — CNPJ Estabelecimentos
# =============================================================================
#
# ESTRUTURA DO PROJETO
# ├── _targets.R              ← este arquivo (orquestração do pipeline)
# ├── R/
# │   └── funcoes.R           ← todas as funções do pipeline
# └── data/
#     ├── Estabelecimentos/   ← arquivos brutos da Receita Federal (*.ESTABELE)
#     └── parquet/
#         ├── estabelecimentos/  ← dataset particionado por UF (gerado)
#         └── fortaleza/         ← subset filtrado de Fortaleza (gerado)
#
# USO BÁSICO
#   library(targets)
#   tar_make()          # executa o pipeline completo
#   tar_visnetwork()    # visualiza o grafo de dependências
#   tar_read(fortaleza) # lê o tibble em memória sem reexecutar
#
# REEXECUÇÃO SELETIVA
#   O targets só reexecuta um alvo quando ele ou alguma dependência muda.
#   Ex.: mudar `apenas_ativas = TRUE` reexecuta apenas `parquet_fortaleza`
#   e `fortaleza` — a consolidação (cara) não é refeita.
#
# PARALELISMO
#   Para habilitar execução paralela entre alvos independentes:
#     tar_make_clustermq(workers = 4L)   # via {clustermq}
#     tar_make_future(workers = 4L)      # via {future}
# =============================================================================

library(targets)
library(tarchetypes)  # tar_file_read(), tar_plan() e outros helpers

# Carrega todas as funções definidas em R/funcoes.R
tar_source("R/functions.R")

# -----------------------------------------------------------------------------
# OPÇÕES GLOBAIS DO PIPELINE
# -----------------------------------------------------------------------------
tar_option_set(
  packages = c("arrow", "dplyr", "readr", "stringr", "fs"),
  
  # Formato padrão de serialização dos alvos.
  # "qs" é mais rápido e compacto que "rds" para tibbles grandes.
  # Instale com: install.packages("qs")
  format = "qs",
  
  # Semente para reprodutibilidade
  seed = 42L
)

# -----------------------------------------------------------------------------
# PARÂMETROS DO PIPELINE
# (altere aqui sem precisar tocar nas funções)
# -----------------------------------------------------------------------------
DIR_ENTRADA  <- "data/Estabelecimentos"
DIR_PARQUET  <- "data/parquet/estabelecimentos"
DIR_FORTALEZA <- "data/parquet/fortaleza"

UF_ALVO          <- "CE"
MUNICIPIO_ALVO   <- "1051"   # Fortaleza — tabela de municípios da RF
APENAS_ATIVAS    <- TRUE
CHUNK_SIZE       <- 100000L  # Ajuste conforme RAM disponível

# -----------------------------------------------------------------------------
# DEFINIÇÃO DOS ALVOS
# -----------------------------------------------------------------------------
list(
  
  # ---------------------------------------------------------------------------
  # ALVO 1 — Listar arquivos de entrada
  # ---------------------------------------------------------------------------
  # `tar_files()` é um alvo especial do {tarchetypes}: ele monitora o vetor
  # de caminhos e invalida os alvos dependentes quando um arquivo muda
  # (baseado em hash do conteúdo, não apenas na data de modificação).
  # Cada arquivo vira um sub-alvo independente, permitindo paralelismo.
  # ---------------------------------------------------------------------------
  tarchetypes::tar_files(
    name    = arquivos_entrada,
    command = listar_arquivos_entrada(
      dir = DIR_ENTRADA,
      padrao = "ESTABELE$")
  ),
  
  # ---------------------------------------------------------------------------
  # ALVO 2 — Consolidar em Parquet particionado por UF
  # ---------------------------------------------------------------------------
  # Retorna o caminho do diretório do dataset.
  # `format = "file"` instrui o targets a rastrear o artefato em disco
  # (hash do diretório) em vez de serializar o valor em R.
  # Assim, este alvo pesado só é reexecutado se os arquivos de entrada
  # mudarem ou se o diretório de saída for apagado/modificado.
  # ---------------------------------------------------------------------------
  tar_target(
    name    = parquet_estabelecimentos,
    command = consolidar_para_parquet(
      arquivos = arquivos_entrada,
      dir_saida  = DIR_PARQUET,
      chunk_size = CHUNK_SIZE),
    format = "file"    # rastreia o diretório como artefato em disco
  ),
  
  # ---------------------------------------------------------------------------
  # ALVO 3 — Filtrar Fortaleza (aproveitando partition pruning por UF)
  # ---------------------------------------------------------------------------
  # Retorna o caminho do arquivo Parquet filtrado.
  # Depende de `parquet_estabelecimentos` (diretório) — se a consolidação
  # for refeita, este alvo também será invalidado automaticamente.
  # ---------------------------------------------------------------------------
  tar_target(
    name    = parquet_fortaleza,
    command = filtrar_municipio(
      dir_parquet      = parquet_estabelecimentos,
      dir_saida        = DIR_FORTALEZA,
      uf               = UF_ALVO,
      codigo_municipio = MUNICIPIO_ALVO,
      apenas_ativas    = APENAS_ATIVAS
    ),
    format = "file"
  ),
  
  # ---------------------------------------------------------------------------
  # ALVO 4 — Carregar tibble de Fortaleza em memória
  # ---------------------------------------------------------------------------
  # Este é o alvo que as etapas de análise devem consumir com `tar_read()`.
  # Como usa `format = "qs"` (padrão global), o tibble é cacheado localmente
  # e recarregado instantaneamente entre sessões sem reler o Parquet.
  # ---------------------------------------------------------------------------
  tar_target(
    name    = fortaleza,
    command = carregar_municipio(parquet_fortaleza)
    # format = "qs" herdado das opções globais
  )
  
  # ---------------------------------------------------------------------------
  # EXEMPLOS DE ALVOS DE ANÁLISE (descomente e adapte conforme necessidade)
  # ---------------------------------------------------------------------------
  
  # --- Contagem por CNAE principal ---
  # tar_target(
  #   name    = cnae_fortaleza,
  #   command = fortaleza |>
  #     count(cnae_fiscal_principal, sort = TRUE)
  # ),
  
  # --- Série histórica de abertura de empresas ---
  # tar_target(
  #   name    = serie_abertura,
  #   command = fortaleza |>
  #     filter(!is.na(data_inicio_atividade)) |>
  #     mutate(ano = format(data_inicio_atividade, "%Y")) |>
  #     count(ano)
  # ),
  
  # --- Relatório Quarto/RMarkdown ---
  # tarchetypes::tar_quarto(
  #   name = relatorio,
  #   path = "relatorio.qmd"
  # )
)
