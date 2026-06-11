# =============================================================================
# R/funcoes.R
# CNPJ Estabelecimentos — Funções do pipeline
# =============================================================================
# Todas as funções seguem o contrato do {targets}:
#   - Recebem apenas o que precisam como argumento
#   - Retornam um valor serializável (caminho, tibble, vetor…)
#   - Não produzem efeitos colaterais fora do que é declarado no _targets.R
# =============================================================================

library(arrow)
library(dplyr)
library(readr)
library(stringr)
library(fs)


# -----------------------------------------------------------------------------
# CONSTANTES
# -----------------------------------------------------------------------------

#' Nomes das colunas conforme layout oficial da Receita Federal
colunas_estabelecimentos <- c(
  "cnpj_basico",
  "cnpj_ordem",
  "cnpj_dv",
  "identificador_matriz_filial",  # 1 = Matriz | 2 = Filial
  "nome_fantasia",
  "situacao_cadastral",           # 2=Ativa 3=Suspensa 4=Inapta 08=Baixada
  "data_situacao_cadastral",
  "motivo_situacao_cadastral",
  "nome_cidade_exterior",
  "pais",
  "data_inicio_atividade",
  "cnae_fiscal_principal",
  "cnae_fiscal_secundario",
  "tipo_logradouro",
  "logradouro",
  "numero",
  "complemento",
  "bairro",
  "cep",
  "uf",
  "municipio",                    # Código próprio da RF (≠ IBGE)
  "ddd1",
  "telefone1",
  "ddd2",
  "telefone2",
  "ddd_fax",
  "fax",
  "correio_eletronico",
  "situacao_especial",
  "data_situacao_especial"
)


# -----------------------------------------------------------------------------
# 1. listar_arquivos_entrada()
# -----------------------------------------------------------------------------
#' Lista os arquivos de estabelecimentos no diretório de entrada.
#'
#' Retornar o vetor de caminhos como alvo separado permite que o {targets}
#' detecte automaticamente quando novos arquivos forem adicionados ao diretório,
#' invalidando apenas os alvos dependentes.
#'
#' @param dir   Caminho para o diretório com os arquivos ESTABELE.
#' @param padrao Regex para identificar os arquivos corretos.
#' @return Vetor de caracteres com os caminhos absolutos dos arquivos.
listar_arquivos_entrada <- function(
    dir    = "data/Estabelecimentos",
    padrao = "ESTABELE$"
) {
  if (!dir_exists(dir)) {
    stop("Diretório de entrada não encontrado: ", dir)
  }
  
  arquivos <- dir_ls(dir, regexp = padrao)
  
  if (length(arquivos) == 0) {
    stop("Nenhum arquivo encontrado com o padrão '", padrao, "' em: ", dir)
  }
  
  message("Arquivos de entrada encontrados: ", length(arquivos))
  arquivos
}


# -----------------------------------------------------------------------------
# 2. consolidar_para_parquet()
# -----------------------------------------------------------------------------
#' Le todos os arquivos CSV da Receita Federal e grava um dataset Parquet
#' particionado por arquivo de origem (um diretorio por arquivo bruto).
#'
#' Estrategia:
#'   - O callback le, limpa e acumula chunks em lista (dentro do escopo
#'     de cada arquivo).
#'   - Ao final de cada arquivo, um unico `write_parquet` grava o resultado
#'     em data/parquet/estabelecimentos/arquivo=N/part-0.parquet.
#'   - O particionamento por indice de arquivo e simples, sem contador global,
#'     sem risco de colisao e compativel com `open_dataset` do Arrow.
#'
#' Vantagens sobre particionar por UF no callback:
#'   - Sem `write_dataset` por chunk (evita overhead de I/O multiplo).
#'   - Sem `basename_template` ou contador global.
#'   - `bind_rows` ocorre uma vez por arquivo (~1 GB), nao sobre todos os 10.
#'   - O schema Parquet e identico em todas as particoes (Arrow nao precisa
#'     reconciliar schemas divergentes ao abrir o dataset).
#'
#' @param arquivos    Vetor de caminhos (saida de `listar_arquivos_entrada()`).
#' @param dir_saida   Diretorio raiz do dataset Parquet.
#' @param chunk_size  Linhas por ciclo de leitura (ajuste conforme RAM).
#' @return Caminho do diretorio do dataset Parquet.
consolidar_para_parquet <- function(
    arquivos,
    dir_saida    = "data/parquet/estabelecimentos",
    chunk_size   = 500000L,
    sobrescrever = FALSE   # TRUE apaga o dataset anterior antes de recriar
) {
  if (sobrescrever && dir_exists(dir_saida)) {
    message("Removendo dataset anterior: ", dir_saida)
    dir_delete(dir_saida)
  }
  dir_create(dir_saida, recurse = TRUE)
  
  n_esperado   <- length(colunas_estabelecimentos)  # 30
  total_linhas <- 0L
  
  for (i_arq in seq_along(arquivos)) {
    arquivo      <- arquivos[[i_arq]]
    nome_arquivo <- path_file(arquivo)
    chunks       <- list()
    message("\n[", i_arq, "/", length(arquivos), "] Lendo: ", nome_arquivo)
    
    # --- Callback: limpa e acumula cada chunk em lista ---
    processar_chunk <- function(chunk, pos) {
      
      # Descartar coluna extra gerada por ";" no final de cada linha (padrao RF)
      if (ncol(chunk) > n_esperado) {
        chunk <- chunk[, seq_len(n_esperado)]
      }
      
      if (ncol(chunk) != n_esperado) {
        warning(nome_arquivo, " pos=", pos, ": ", ncol(chunk),
                " colunas (esperado ", n_esperado, ") - chunk ignorado.")
        return(invisible(NULL))
      }
      
      names(chunk) <- colunas_estabelecimentos
      
      chunk <- chunk |>
        mutate(
          nome_fantasia  = str_squish(nome_fantasia),
          logradouro     = str_squish(logradouro),
          bairro         = str_squish(bairro),
          complemento    = str_squish(complemento),
          uf             = str_to_upper(str_trim(uf)),
          cnpj_basico    = str_trim(cnpj_basico),
          municipio      = str_trim(municipio)
        )
      
      chunks[[length(chunks) + 1L]] <<- chunk
      message("  chunk ", length(chunks), " | ",
              format(nrow(chunk), big.mark = "."), " linhas")
    }
    
    tryCatch(
      read_delim_chunked(
        file             = arquivo,
        callback         = DataFrameCallback$new(processar_chunk),
        chunk_size       = chunk_size,
        delim            = ";",
        col_names        = FALSE,
        col_types        = cols(.default = col_character()),
        locale           = locale(encoding = "latin1"),
        # quote = '"' e o padrao do readr: interpreta aspas duplas como
        # delimitador de campo, removendo-as do valor e tratando ";" internos
        # (ex.: cnae_fiscal_secundario) como parte do campo, nao como separador.
        # NAO usar quote = "" — isso desativa o quoting, faz as aspas virarem
        # parte do valor e transforma ";" dentro de campos em colunas extras.
        quote            = "\"",
        escape_backslash = FALSE,
        escape_double    = TRUE,   # padrao RFC 4180: "" dentro de campo = "
        progress         = FALSE
      ),
      error = function(e) warning("Erro em '", nome_arquivo, "': ", conditionMessage(e))
    )
    
    # --- Gravar arquivo consolidado numa unica operacao ---
    if (length(chunks) == 0L) {
      warning("Nenhum chunk valido em '", nome_arquivo, "' - arquivo ignorado.")
      next
    }
    
    df <- bind_rows(chunks)
    rm(chunks); gc(verbose = FALSE)
    
    # Estrutura: dir_saida/arquivo=1/part-0.parquet
    dir_particao <- path(dir_saida, paste0("arquivo=", i_arq))
    dir_create(dir_particao, recurse = TRUE)
    write_parquet(df, path(dir_particao, "part-0.parquet"), compression = "snappy")
    
    total_linhas <- total_linhas + nrow(df)
    message("  gravado: ", path(dir_particao, "part-0.parquet"),
            " | ", format(nrow(df), big.mark = "."), " linhas")
    
    rm(df); gc(verbose = FALSE)
  }
  
  message("\n Consolidacao concluida!")
  message("  Total de linhas : ", format(total_linhas, big.mark = "."))
  message("  Dataset em      : ", dir_saida)
  
  dir_saida
}


# -----------------------------------------------------------------------------
# 3. filtrar_municipio()
# -----------------------------------------------------------------------------
#' Filtra o dataset Parquet para um municipio especifico e salva o resultado.
#'
#' Abre o dataset consolidado (particionado por arquivo de origem) de forma
#' lazy e aplica os filtros de UF e municipio antes de coletar — o Arrow
#' percorre cada particao mas so transfere para a RAM as linhas que passam
#' nos predicados, mantendo o consumo de memoria sob controle.
#'
#' @param dir_parquet      Diretorio do dataset Parquet consolidado.
#' @param dir_saida        Diretorio onde o Parquet filtrado sera salvo.
#' @param uf               Sigla da UF (ex.: "CE").
#' @param codigo_municipio Codigo do municipio na tabela da RF.
#' @param apenas_ativas    Se TRUE, mantem apenas situacao cadastral "2".
#' @param colunas          Vetor de colunas a manter; NULL = todas.
#' @return Caminho do arquivo Parquet gerado.
filtrar_municipio <- function(
    dir_parquet,
    dir_saida          = "data/parquet/fortaleza",
    uf                 = "CE",
    codigo_municipio   = "1051",   # Fortaleza na tabela RF
    apenas_ativas      = FALSE,
    colunas            = NULL
) {
  if (!dir_exists(dir_parquet)) {
    stop("Dataset Parquet não encontrado: ", dir_parquet)
  }
  
  dir_create(dir_saida, recurse = TRUE)
  
  ds <- open_dataset(dir_parquet, format = "parquet")
  
  query <- ds |>
    filter(uf == .env$uf, municipio == codigo_municipio)
  
  if (apenas_ativas) {
    query <- query |> filter(situacao_cadastral == "2")
  }
  
  if (!is.null(colunas)) {
    query <- query |> select(all_of(colunas))
  }
  
  arquivo_saida <- path(dir_saida, paste0(tolower(uf), "_mun", codigo_municipio, ".parquet"))
  
  query |>
    collect() |>
    write_parquet(arquivo_saida, compression = "snappy")
  
  n <- open_dataset(arquivo_saida) |> count() |> collect() |> pull(n)
  message("✔ Filtro concluído | UF: ", uf,
          " | Município: ", codigo_municipio,
          " | Registros: ", format(n, big.mark = "."))
  
  arquivo_saida
}


# -----------------------------------------------------------------------------
# 4. carregar_municipio()
# -----------------------------------------------------------------------------
#' Carrega o Parquet filtrado em memória como tibble, convertendo datas.
#'
#' Esta etapa é separada de `filtrar_municipio()` para que o targets possa
#' cachear o tibble em memória (via `format = "qs"` ou `"rds"`) e reusá-lo
#' nas etapas de análise sem reler o disco a cada execução.
#'
#' @param arquivo_parquet Caminho para o arquivo Parquet filtrado.
#' @return Tibble com os dados do município.
carregar_municipio <- function(arquivo_parquet) {
  if (!file_exists(arquivo_parquet)) {
    stop("Arquivo não encontrado: ", arquivo_parquet)
  }
  
  df <- read_parquet(arquivo_parquet) |>
    mutate(
      across(
        c(data_situacao_cadastral, data_inicio_atividade, data_situacao_especial),
        ~ as.Date(suppressWarnings(.), format = "%Y%m%d")
      )
    )
  
  message("Base carregada: ", format(nrow(df), big.mark = "."),
          " registros | ", ncol(df), " colunas")
  df
}

# -----------------------------------------------------------------------------
# 5. salva_tabela()
# -----------------------------------------------------------------------------
#' Salva a tabela de estabelecimentos por bairro.
#'
#' @param df Tabela
#' @param pasta Caminho para o arquivo
#' @return Tibble com os dados por bairro
salva_tabela <- function(df, 
                         pasta = "data/") {
  tabela <- df |>
    dplyr::count(localidade_encontrada, sort = TRUE)
  
  arquivo_saida <- fs::path(pasta, "tabela_bairro.csv")
  
  readr::write_csv2(tabela, arquivo_saida)
  
  tabela
}