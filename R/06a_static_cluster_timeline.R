# ==============================================================================
# 06a_static_cluster_timeline.R
#
# Estimate period-specific demolition material potential within the fixed
# wood and concrete stock clusters identified by script 02.
#
# The clusters retain their original boundaries and classifications. Buildings
# are assigned using the original cluster ID rasters, not polygon intersections.
# Expected demolition amounts come from the provisional scenario in script 06;
# these are not validated forecasts or confirmed recoverable material flows.
#
# Inputs:
#   data/processed/buildings_material_stock_joensuu.gpkg
#   data/processed/demolition_potential_by_building.csv
#   data/processed/wood_clusters.gpkg
#   data/processed/concrete_clusters.gpkg
#   data/processed/wood_cluster_ids.tif
#   data/processed/concrete_cluster_ids.tif
#
# Outputs:
#   data/processed/wood_cluster_timeline.csv
#   data/processed/concrete_cluster_timeline.csv
#   data/processed/cluster_coverage_by_period.csv
#   data/processed/wood_cluster_timeline.gpkg
#   data/processed/concrete_cluster_timeline.gpkg
#
# Run from the R/ directory. Plots and interactive maps are optional.
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(tidyr)

show_plots <- FALSE
show_maps <- FALSE

# ==============================================================================
# 1. Read and validate input data
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

stopifnot(
  nrow(buildings) > 0,
  nrow(demolition) > 0,
  nrow(wood_clusters) > 0,
  nrow(concrete_clusters) > 0,
  "pysyva_rakennustunnus" %in% names(buildings),
  all(c("pysyva_rakennustunnus", "period", "demolition_probability",
        "expected_wood_t", "expected_concrete_t") %in% names(demolition)),
  all(c("cluster_id", "class", "material_t") %in% names(wood_clusters)),
  all(c("cluster_id", "class", "material_t") %in% names(concrete_clusters)),
  !anyNA(buildings$pysyva_rakennustunnus),
  !anyDuplicated(buildings$pysyva_rakennustunnus),
  !anyNA(demolition$pysyva_rakennustunnus),
  !anyNA(demolition$period),
  !anyDuplicated(demolition[c("pysyva_rakennustunnus", "period")]),
  all(demolition$pysyva_rakennustunnus %in% buildings$pysyva_rakennustunnus),
  !anyNA(wood_clusters$cluster_id),
  !anyNA(concrete_clusters$cluster_id),
  !anyDuplicated(wood_clusters$cluster_id),
  !anyDuplicated(concrete_clusters$cluster_id),
  st_crs(wood_clusters) == st_crs(buildings),
  st_crs(concrete_clusters) == st_crs(buildings),
  same.crs(wood_raster, vect(buildings)),
  same.crs(concrete_raster, vect(buildings)),
  all(is.finite(demolition$demolition_probability)),
  all(between(demolition$demolition_probability, 0, 1)),
  all(demolition$expected_wood_t >= 0, na.rm = TRUE),
  all(demolition$expected_concrete_t >= 0, na.rm = TRUE)
)

cat("\nStatic cluster timeline inputs:\n")
cat("Buildings in stock:", nrow(buildings), "\n")
cat("Buildings in demolition scenario:",
    length(unique(demolition$pysyva_rakennustunnus)), "\n")
cat("Periods:", length(unique(demolition$period)), "\n")
cat("Accepted wood clusters:", nrow(wood_clusters), "\n")
cat("Accepted concrete clusters:", nrow(concrete_clusters), "\n")

# ==============================================================================
# 2. Assign buildings to the original cluster rasters
# ==============================================================================

building_points <- centroids(vect(buildings), inside = TRUE)

# Use terra::extract explicitly to avoid function masking by other packages.
buildings$wood_cluster_id <- terra::extract(
  wood_raster, building_points, ID = FALSE
)[[1]]

buildings$concrete_cluster_id <- terra::extract(
  concrete_raster, building_points, ID = FALSE
)[[1]]

# Each building in script 06 appears once per scenario period.
building_timeline <- buildings |>
  st_drop_geometry() |>
  select(pysyva_rakennustunnus, wood_cluster_id, concrete_cluster_id) |>
  inner_join(demolition, by = "pysyva_rakennustunnus")

stopifnot(nrow(building_timeline) == nrow(demolition))

# ==============================================================================
# 3. Calculate temporal material potential within accepted clusters
# ==============================================================================

calculate_cluster_timeline <- function(
    buildings, clusters, cluster_column, material_column
) {
  # Raster IDs include rejected clusters. Keep only accepted polygon IDs.
  summary <- buildings |>
    mutate(cluster_id = .data[[cluster_column]]) |>
    filter(cluster_id %in% clusters$cluster_id) |>
    group_by(cluster_id, period) |>
    summarise(
      expected_material_t = sum(.data[[material_column]], na.rm = TRUE),
      expected_demolitions = sum(demolition_probability),
      .groups = "drop"
    )
  
  # Include every accepted cluster in every period, even when its potential is 0.
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
# 4. Summarise and validate city-wide cluster coverage
# ==============================================================================

stopifnot(
  nrow(wood_timeline) ==
    nrow(wood_clusters) * length(unique(demolition$period)),
  nrow(concrete_timeline) ==
    nrow(concrete_clusters) * length(unique(demolition$period)),
  all(is.finite(wood_timeline$expected_material_t)),
  all(is.finite(concrete_timeline$expected_material_t)),
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

cat("\nShare of demolition material potential within fixed clusters:\n")
print(cluster_coverage)

# ==============================================================================
# 5. Save cluster timelines and spatial results
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

# Repeated geometries represent the same fixed cluster in different periods.
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

cat("\nStatic cluster timeline outputs saved to data/processed/.\n")

# ==============================================================================
# 6. Optional visualisations
# ==============================================================================

if (show_plots) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to display the timeline plot.")
  }
  library(ggplot2)
  
  print(
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
  )
}

if (show_maps) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("Install mapview to display the interactive map.")
  }
  
  # The second available period matches the original 2030-2035 example.
  periods <- unique(demolition$period)
  selected_period <- periods[min(2L, length(periods))]
  
  concrete_map <- concrete_timeline_map |>
    filter(period == selected_period)
  
  concrete_breaks <- 10^seq(
    0,
    ceiling(log10(max(concrete_map$expected_material_t) + 1)),
    length.out = 8
  ) - 1
  
  print(mapview::mapview(
    concrete_map,
    zcol = "expected_material_t",
    at = concrete_breaks,
    col.regions = hcl.colors(8, "Viridis"),
    layer.name = paste("Expected concrete", selected_period, "(tonnes)"),
    map.types = "CartoDB.Positron"
  ))
}
