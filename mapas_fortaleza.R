# =============================================================================
# mapas_fortaleza.R
# Exemplos de visualização dos estabelecimentos geocodificados
# =============================================================================
# Execute este script após rodar tar_make() e ter o alvo fortaleza_geoloc pronto
#
# Dados necessários:
#   - fortaleza_geoloc: estabelecimentos com lat/lon (via tar_read)
#   - data/i3geomap_bairros_de_fortaleza.shp: polígonos dos bairros
#
# Pacotes: sf, ggplot2, leaflet, tmap
# =============================================================================

library(targets)
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(stringr)

# Carregar dados do pipeline targets
geoloc <- tar_read(fortaleza_geoloc) |> 
  dplyr::mutate(cnpj_str = paste0(cnpj_basico, cnpj_ordem, cnpj_dv))

# Carrega estabelecimentos Ceará Sem Fome
csf_data <- readr::read_csv("data/Estabelecimentos credenciados Cartão Ceará Sem Fome.csv") |> 
  dplyr::mutate(cnpj_str = str_pad(sprintf("%.0f", CNPJ), width = 14, side = "left", pad = "0")) |> 
  dplyr::filter(CIDADE == "Fortaleza") |> 
  dplyr::select(CREDENCIADO, cnpj_str)

# Quantos CNPJs únicos dos estabelecimentos foram encontrados em geoloc?
csf_data$cnpj_str %>% unique() %>% intersect(geoloc$cnpj_str) %>% length()

geoloc_filtrado <- geoloc %>%
  right_join(csf_data, by = join_by(cnpj_str))

csf_nao_localizado <- csf_data %>%
  anti_join(geoloc, by = "cnpj_str")

rm(geoloc)

# Carregar shapefile de bairros
bairros <- st_read("data/vw_Fortaleza_Bairros.shp",
                   options = "ENCODING=LATIN1",
                   quiet = TRUE)

# Garantir CRS consistente (SIRGAS 2000 / UTM zone 24S é comum no Ceará)
# Se o shapefile estiver em outro CRS, transformar para WGS84 (usado pelo geocodebr)
if (st_crs(bairros)$epsg != 4326) {
  bairros <- st_transform(bairros, 4326)
}

# Converter geocodificação para objeto sf
geoloc_sf <- geoloc_filtrado |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

message("Estabelecimentos geocodificados: ", nrow(geoloc_sf))

# =============================================================================
# MAPA 1 — Estático simples com ggplot2
# =============================================================================
# Pontos sobre bairros, um mapa limpo para relatórios

mapa_simples <- ggplot() +
  # Bairros como base
  geom_sf(data = bairros, fill = "gray95", color = "gray60", linewidth = 0.3) +
  # Estabelecimentos como pontos
  geom_sf(data = geoloc_sf, color = "#E63946", alpha = 0.6, size = 1.5) +
  # Tema minimalista
  theme_void() +
  labs(
    title = "Varejo Alimentar em Fortaleza",
    subtitle = paste(nrow(geoloc_sf), "estabelecimentos geocodificados"),
    caption = "Fonte: CNPJ Receita Federal | Geocodificação: {geocodebr}"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 1)
  )

# Salvar
ggsave("output/mapa_simples.png", mapa_simples, width = 10, height = 8, dpi = 300, bg = "white")
message("✓ Mapa simples salvo em output/mapa_simples.png")


# =============================================================================
# MAPA 2 — Densidade de estabelecimentos por bairro (coroplético)
# =============================================================================
# Conta quantos estabelecimentos há em cada bairro e coloriza

# Spatial join: quantos pontos em cada polígono
contagem_bairro <- bairros |>
  st_join(geoloc_sf) |>
  group_by(nome_bairro = Nome) |>   # ajuste 'nome' para o nome correto da coluna do shapefile
  summarise(
    n_estabelecimentos = n(),
    .groups = "drop"
  )

mapa_densidade <- ggplot(contagem_bairro) +
  geom_sf(aes(fill = n_estabelecimentos), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Estabelecimentos",
    trans = "sqrt",   # transformação para lidar com outliers
    breaks = c(1, 10, 50, 100, 200)
  ) +
  theme_void() +
  labs(
    title = "Densidade de Varejo Alimentar por Bairro",
    subtitle = "Fortaleza — CE",
    caption = "Fonte: CNPJ Receita Federal"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "right"
  )

ggsave("output/mapa_densidade.png", mapa_densidade, width = 10, height = 8, dpi = 300, bg = "white")
message("✓ Mapa de densidade salvo em output/mapa_densidade.png")


# =============================================================================
# MAPA 3 — Interativo com leaflet (HTML)
# =============================================================================
# Explora os dados com zoom, clique e popups

library(leaflet)

# Preparar labels para popup
geoloc_popup <- geoloc_sf |>
  mutate(
    popup_text = paste0(
      "<b>", ifelse(is.na(CREDENCIADO) | CREDENCIADO == "", "Sem nome fantasia", CREDENCIADO), "</b><br>",
      "CNAE: ", cnae_fiscal_principal, "<br>",
      "Endereço: ", endereco_encontrado, "<br>",
      "Bairro: ", bairro
    )
  )

mapa_interativo <- leaflet() |>
  # Mapa base
  addProviderTiles(providers$CartoDB.Positron) |>
  # Polígonos de bairros (leve)
  addPolygons(
    data = bairros,
    fillColor = "transparent",
    color = "#999999",
    weight = 1.5,
    opacity = 0.6,
    label = ~Nome   # ajuste para o nome correto da coluna
  ) |>
  # Estabelecimentos
  addCircleMarkers(
    data = geoloc_popup,
    radius = 10,
    color = "#E63946",
    fillOpacity = 0.7,
    stroke = FALSE,
    popup = ~popup_text,
    clusterOptions = markerClusterOptions()  # agrupa quando zoom out
  ) |>
  # Controles
  addScaleBar(position = "bottomleft") |>
  addMiniMap(toggleDisplay = TRUE, position = "bottomright")

# Salvar como HTML
library(htmlwidgets)
saveWidget(mapa_interativo, "output/mapa_interativo.html", selfcontained = TRUE)
message("✓ Mapa interativo salvo em output/mapa_interativo.html")


# =============================================================================
# MAPA 4 — Kernel density (heatmap) com tmap
# =============================================================================
# Mostra concentrações espaciais de forma suave

library(tmap)

# Criar grid de polígonos (não centroides) para capturar pontos
# cellsize = 0.002 graus ≈ 220m (mais fino que antes)
densidade_grid <- st_make_grid(bairros, cellsize = 0.002, what = "polygons", square = FALSE) |>
  st_as_sf() |>
  mutate(
    # Contar quantos estabelecimentos caem dentro de cada célula
    densidade = lengths(st_intersects(x, geoloc_sf))
  ) |>
  filter(densidade > 0)  # Manter apenas células com pelo menos 1 estabelecimento

# Se ainda não houver dados, avisar
if (nrow(densidade_grid) == 0) {
  warning("Grid de densidade vazio. Possíveis causas:\n",
          "  - CRS incompatível entre bairros e geoloc_sf\n",
          "  - Estabelecimentos fora dos limites dos bairros\n",
          "  - cellsize muito grande")
  message("Tentando diagnóstico...")
  message("  Bairros CRS: ", st_crs(bairros)$input)
  message("  Geoloc CRS: ", st_crs(geoloc_sf)$input)
  message("  N estabelecimentos: ", nrow(geoloc_sf))
  message("  Bbox bairros: ", paste(round(st_bbox(bairros), 4), collapse = ", "))
  message("  Bbox geoloc: ", paste(round(st_bbox(geoloc_sf), 4), collapse = ", "))
}

mapa_heatmap <- tm_shape(bairros) +
  tm_borders(col = "gray70", lwd = 0.5) +
  tm_shape(densidade_grid) +
  tm_polygons(
    col = "densidade",
    palette = "YlOrRd",
    alpha = 0.7,
    title = "Estabelecimentos\npor célula",
    style = "jenks",  # quebras naturais (melhor que quantile para densidade)
    n = 7,
    border.col = NA
  ) +
  tm_layout(
    title = "Heatmap: Concentração de Varejo Alimentar",
    title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE
  ) +
  tm_credits("Fonte: CNPJ Receita Federal | Grid: 220m", position = c("left", "bottom"))

tmap_save(mapa_heatmap, "output/mapa_heatmap.png", width = 10, height = 8, dpi = 300)
message("✓ Heatmap salvo em output/mapa_heatmap.png")

# =============================================================================
# MAPA 5 — Facetado por tipo de CNAE (múltiplos painéis)
# =============================================================================
# Compara distribuição espacial de diferentes tipos de estabelecimento

# Dicionário de CNAEs (ajuste conforme os códigos usados)
dicionario_cnae <- tibble::tribble(
  ~cnae_fiscal_principal, ~tipo,
  "4711302", "Minimercado",
  "4711301", "Hipermercado",
  "4721104", "Padaria",
  "4722901", "Açougue",
  "4712100", "Mercearia",
  "4729699", "Outros alimentícios",
  "4771701", "Farmácia",
  "4771702", "Perfumaria",
  "4721103", "Confeitaria",
  "4723700", "Bebidas",
  "4724500", "Hortifrutigranjeiros",
  "4729602", "Produtos naturais",
  "4771703", "Produtos de beleza"
)

geoloc_sf_tipo <- geoloc_sf |>
  left_join(dicionario_cnae, by = "cnae_fiscal_principal") |>
  filter(!is.na(tipo))

mapa_facetado <- ggplot() +
  geom_sf(data = bairros, fill = "gray97", color = "gray80", linewidth = 0.2) +
  geom_sf(data = geoloc_sf_tipo, aes(color = tipo), alpha = 0.6, size = 0.8) +
  facet_wrap(~tipo, ncol = 4) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  theme_void() +
  labs(
    title = "Distribuição Espacial por Tipo de Estabelecimento",
    caption = "Fonte: CNPJ Receita Federal"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(size = 9, face = "bold"),
    plot.caption = element_text(size = 7, color = "gray50", hjust = 1)
  )

ggsave("output/mapa_facetado.png", mapa_facetado, width = 14, height = 10, dpi = 300, bg = "white")
message("✓ Mapa facetado salvo em output/mapa_facetado.png")


# =============================================================================
# BÔNUS — Estatísticas descritivas espaciais
# =============================================================================

cat("\n=== ESTATÍSTICAS ESPACIAIS ===\n\n")

# Top 5 bairros com mais estabelecimentos
top_bairros <- contagem_bairro |>
  st_drop_geometry() |>
  arrange(desc(n_estabelecimentos)) |>
  slice_head(n = 5)

cat("Top 5 bairros:\n")
print(top_bairros, n = 5)

# Centróide da distribuição (centro de massa)
centroide <- geoloc_sf |>
  st_union() |>
  st_centroid()

cat("\nCentróide da distribuição:\n")
print(st_coordinates(centroide))

# Densidade média (estabelecimentos por km²)
area_fortaleza <- st_area(st_union(bairros)) |> units::set_units(km^2)
densidade_media <- nrow(geoloc_sf) / as.numeric(area_fortaleza)

cat("\nDensidade média: ", round(densidade_media, 2), " estabelecimentos/km²\n")

