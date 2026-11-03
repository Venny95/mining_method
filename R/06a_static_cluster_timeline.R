
# ==============================================================================
# 06a_static_cluster_timeline.R
#
# Estimate temporal demolition material potential within static clusters.
# Cluster geometries and classifications remain unchanged.
# Building-to-cluster assignment uses the original clustering rasters.
#
# Outputs:
#   data/processed/wood_cluster_timeline.csv
#   data/processed/concrete_cluster_timeline.csv
#   data/processed/cluster_coverage_by_period.csv
#   data/processed/wood_cluster_timeline.gpkg
#   data/processed/concrete_cluster_timeline.gpkg
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mapview)

# ==============================================================================
# 1. Read input data
# ==============================================================================

buildings <- st_read(
  "data/processed/buildings_material_stock_joensuu.gpkg",
  quiet = TRUE
) |>
  st_transform(3067)

demolition <- read.csv(
  "data/processed/demolition_potential_by_building.csv",
  colClasses = c(pysyva_rakennustunnus = "character")
)

wood_clusters <- st_read("data/processed/wood_clusters.gpkg", quiet = TRUE)
concrete_clusters <- st_read("data/processed/concrete_clusters.gpkg", quiet = TRUE)

wood_raster <- rast("data/processed/wood_cluster_ids.tif")
concrete_raster <- rast("data/processed/concrete_cluster_ids.tif")

# ==============================================================================
# 2. Assign buildings to static clusters
# ==============================================================================

building_points <- terra::centroids(terra::vect(buildings), inside = TRUE)

buildings$wood_cluster_id <- terra::extract(
  wood_raster, building_points, ID = FALSE
)[[1]]

buildings$concrete_cluster_id <- terra::extract(
  concrete_raster, building_points, ID = FALSE
)[[1]]

stopifnot(
  !anyNA(buildings$pysyva_rakennustunnus),
  !anyDuplicated(buildings$pysyva_rakennustunnus),
  !anyNA(demolition$pysyva_rakennustunnus),
  !anyDuplicated(demolition[c("pysyva_rakennustunnus", "period")]),
  all(demolition$pysyva_rakennustunnus %in%
        buildings$pysyva_rakennustunnus)
)

building_timeline <- buildings |>
  st_drop_geometry() |>
  select(pysyva_rakennustunnus, wood_cluster_id, concrete_cluster_id) |>
  inner_join(demolition, by = "pysyva_rakennustunnus")

# ==============================================================================
# 3. Calculate temporal material potential within clusters
# ==============================================================================

calculate_cluster_timeline <- function(
    buildings, clusters, cluster_column, material_column
) {
  
  summary <- buildings |>
    mutate(cluster_id = .data[[cluster_column]]) |>
    filter(cluster_id %in% clusters$cluster_id) |>
    group_by(cluster_id, period) |>
    summarise(
      expected_material_t = sum(.data[[material_column]], na.rm = TRUE),
      expected_demolitions = sum(demolition_probability),
      .groups = "drop"
    )
  
  expand_grid(
    cluster_id = clusters$cluster_id,
    period = unique(buildings$period)
  ) |>
    left_join(summary, by = c("cluster_id", "period")) |>
    mutate(
      expected_material_t = replace_na(expected_material_t, 0),
      expected_demolitions = replace_na(expected_demolitions, 0)
    ) |>
    left_join(
      clusters |>
        st_drop_geometry() |>
        select(cluster_id, class, material_t),
      by = "cluster_id"
    ) |>
    arrange(cluster_id, period)
}

wood_timeline <- calculate_cluster_timeline(
  building_timeline, wood_clusters, "wood_cluster_id", "expected_wood_t"
)

concrete_timeline <- calculate_cluster_timeline(
  building_timeline, concrete_clusters, "concrete_cluster_id",
  "expected_concrete_t"
)

# ==============================================================================
# 4. Validate results and calculate cluster coverage
# ==============================================================================

stopifnot(
  nrow(wood_timeline) ==
    nrow(wood_clusters) * length(unique(demolition$period)),
  nrow(concrete_timeline) ==
    nrow(concrete_clusters) * length(unique(demolition$period)),
  all(wood_timeline$expected_material_t >= 0),
  all(concrete_timeline$expected_material_t >= 0)
)

city_totals <- demolition |>
  group_by(period) |>
  summarise(
    wood = sum(expected_wood_t, na.rm = TRUE),
    concrete = sum(expected_concrete_t, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    c(wood, concrete),
    names_to = "material",
    values_to = "city_material_t"
  )

cluster_coverage <- bind_rows(
  wood_timeline |> mutate(material = "wood"),
  concrete_timeline |> mutate(material = "concrete")
) |>
  group_by(material, period) |>
  summarise(
    cluster_material_t = sum(expected_material_t),
    .groups = "drop"
  ) |>
  left_join(city_totals, by = c("material", "period")) |>
  mutate(
    outside_clusters_t = city_material_t - cluster_material_t,
    cluster_share_pct = 100 * cluster_material_t / city_material_t
  ) |>
  arrange(material, period)

stopifnot(
  all(is.finite(cluster_coverage$cluster_share_pct)),
  all(between(cluster_coverage$cluster_share_pct, 0, 100))
)

print(cluster_coverage)

# ==============================================================================
# 5. Save results
# ==============================================================================

write.csv(
  wood_timeline, "data/processed/wood_cluster_timeline.csv",
  row.names = FALSE
)

write.csv(
  concrete_timeline, "data/processed/concrete_cluster_timeline.csv",
  row.names = FALSE
)

write.csv(
  cluster_coverage, "data/processed/cluster_coverage_by_period.csv",
  row.names = FALSE
)

wood_timeline_map <- wood_clusters |>
  select(cluster_id) |>
  left_join(wood_timeline, by = "cluster_id")

concrete_timeline_map <- concrete_clusters |>
  select(cluster_id) |>
  left_join(concrete_timeline, by = "cluster_id")

st_write(
  wood_timeline_map,
  "data/processed/wood_cluster_timeline.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  concrete_timeline_map,
  "data/processed/concrete_cluster_timeline.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ==============================================================================
# 6. Visualise results
# ==============================================================================

# Share of city-wide demolition potential within static clusters
ggplot(
  cluster_coverage,
  aes(period, cluster_share_pct, colour = material, group = material)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Demolition material potential within static clusters",
    x = "Period",
    y = "Share of city-wide potential (%)",
    colour = "Material"
  ) +
  theme_minimal()

# Spatial distribution of expected concrete potential
selected_period <- "2030-2035"

concrete_map <- concrete_timeline_map |>
  filter(period == selected_period)

concrete_breaks <- 10^seq(
  0,
  ceiling(log10(max(concrete_map$expected_material_t) + 1)),
  length.out = 8
) - 1

mapview(
  concrete_map,
  zcol = "expected_material_t",
  at = concrete_breaks,
  col.regions = hcl.colors(8, "Viridis"),
  layer.name = paste("Expected concrete", selected_period, "(tonnes)"),
  map.types = "CartoDB.Positron"
)

