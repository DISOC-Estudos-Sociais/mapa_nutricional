get_data_microdata <- function(file) {
  readr::read_csv(file, readr::locale(encoding = "Latin1")) |> 
    janitor::row_to_names(1) |> 
    janitor::clean_names() |> 
    dplyr::rename_with(~ sub("_c_digo$", "", .x))
}

get_data_bdd <- function(){
  
  # Defina o seu projeto no Google Cloud
  basedosdados::set_billing_id("rapid-pact-400813")
  
  # Para carregar o dado direto no R
  query <- "
WITH 
dicionario_natureza_estabelecimento AS (
    SELECT
        chave AS chave_natureza_estabelecimento,
        valor AS descricao_natureza_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'natureza_estabelecimento'
        AND id_tabela = 'microdados_estabelecimentos'
),
dicionario_tamanho_estabelecimento AS (
    SELECT
        chave AS chave_tamanho_estabelecimento,
        valor AS descricao_tamanho_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tamanho_estabelecimento'
        AND id_tabela = 'microdados_estabelecimentos'
),
dicionario_tipo_estabelecimento AS (
    SELECT
        chave AS chave_tipo_estabelecimento,
        valor AS descricao_tipo_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tipo_estabelecimento'
        AND id_tabela = 'microdados_estabelecimentos'
),
dicionario_subsetor_ibge AS (
    SELECT
        chave AS chave_subsetor_ibge,
        valor AS descricao_subsetor_ibge
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'subsetor_ibge'
        AND id_tabela = 'microdados_estabelecimentos'
)
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.quantidade_vinculos_ativos as quantidade_vinculos_ativos,
    dados.quantidade_vinculos_clt as quantidade_vinculos_clt,
    dados.quantidade_vinculos_estatutarios as quantidade_vinculos_estatutarios,
    descricao_natureza_estabelecimento AS natureza_estabelecimento,
    dados.natureza_juridica as natureza_juridica,
    descricao_tamanho_estabelecimento AS tamanho_estabelecimento,
    descricao_tipo_estabelecimento AS tipo_estabelecimento,
    dados.indicador_cei_vinculado as indicador_cei_vinculado,
    dados.indicador_pat as indicador_pat,
    dados.indicador_simples as indicador_simples,
    dados.indicador_rais_negativa as indicador_rais_negativa,
    dados.indicador_atividade_ano as indicador_atividade_ano,
    dados.cnae_1 AS cnae_1,
    diretorio_cnae_1.descricao AS cnae_1_descricao,
    diretorio_cnae_1.descricao_grupo AS cnae_1_descricao_grupo,
    diretorio_cnae_1.descricao_divisao AS cnae_1_descricao_divisao,
    diretorio_cnae_1.descricao_secao AS cnae_1_descricao_secao,
    dados.cnae_2 as cnae_2,
    dados.cnae_2_subclasse AS cnae_2_subclasse,
    diretorio_cnae_2_subclasse.descricao_subclasse AS cnae_2_subclasse_descricao_subclasse,
    diretorio_cnae_2_subclasse.descricao_classe AS cnae_2_subclasse_descricao_classe,
    diretorio_cnae_2_subclasse.descricao_grupo AS cnae_2_subclasse_descricao_grupo,
    diretorio_cnae_2_subclasse.descricao_divisao AS cnae_2_subclasse_descricao_divisao,
    diretorio_cnae_2_subclasse.descricao_secao AS cnae_2_subclasse_descricao_secao,
    descricao_subsetor_ibge AS subsetor_ibge,
    dados.subatividade_ibge as subatividade_ibge,
    dados.cep as cep,
    dados.bairros_fortaleza as bairros_fortaleza
FROM `basedosdados.br_me_rais.microdados_estabelecimentos` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
LEFT JOIN `dicionario_natureza_estabelecimento`
    ON dados.natureza_estabelecimento = chave_natureza_estabelecimento
LEFT JOIN `dicionario_tamanho_estabelecimento`
    ON dados.tamanho_estabelecimento = chave_tamanho_estabelecimento
LEFT JOIN `dicionario_tipo_estabelecimento`
    ON dados.tipo_estabelecimento = chave_tipo_estabelecimento
LEFT JOIN (SELECT DISTINCT cnae_1,descricao,descricao_grupo,descricao_divisao,descricao_secao  FROM `basedosdados.br_bd_diretorios_brasil.cnae_1`) AS diretorio_cnae_1
    ON dados.cnae_1 = diretorio_cnae_1.cnae_1
LEFT JOIN (SELECT DISTINCT subclasse,descricao_subclasse,descricao_classe,descricao_grupo,descricao_divisao,descricao_secao  FROM `basedosdados.br_bd_diretorios_brasil.cnae_2`) AS diretorio_cnae_2_subclasse
    ON dados.cnae_2_subclasse = diretorio_cnae_2_subclasse.subclasse
LEFT JOIN `dicionario_subsetor_ibge`
    ON dados.subsetor_ibge = chave_subsetor_ibge
WHERE
    dados.ano = 2024
    AND dados.id_municipio = '2304400';
"

basedosdados::read_sql(query, billing_project_id = basedosdados::get_billing_id())
}
