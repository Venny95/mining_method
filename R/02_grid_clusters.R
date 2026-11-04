# ==============================================================================
# 02_grid_clusters.R
#
# Identify spatial concentrations of building material stocks (wood, concrete)
# using moving-window sums and dual-threshold connected-region clustering.
# The clustering algorithm is defined in functions/cluster_functions.R and is
# also used by 02b_cluster_sensitivity.R.
#
# Input:
#   data/processed/buildings_material_stock_joensuu.gpkg
#
# Outputs:
#   data/processed/wood_clusters.gpkg
#   data/processed/concrete_clusters.gpkg
#   data/processed/wood_cluster_ids.tif
#   data/processed/concrete_cluster_ids.tif
#
# Run from the R/ directory. Material thresholds are exploratory and are not
# validated limits for local recovery or reuse.
# ==============================================================================

library(terra)
library(sf)

source("functions/cluster_functions.R")


# ==============================================================================
# 1. Define analysis parameters
# ==============================================================================

grid_size_m <- 100
focal_window_cells <- 3
seed_quantile <- 0.90
growth_quantile <- 0.85

wood_cluster_thresholds <- c(
  significant = 500,
  medium = 2000,
  high = 5000
)

concrete_cluster_thresholds <- c(
  significant = 5000,
  medium = 20000,
  high = 50000
)

# Optional interactive maps; disabled for routine source() runs.
show_maps <- FALSE


# ==============================================================================
# 2. Read building data and calculate clusters
# ==============================================================================

buil <- terra::vect(
  "data/processed/buildings_material_stock_joensuu.gpkg"
)

results <- calculate_clusters(
  buil = buil,
  grid_size_m = grid_size_m,
  focal_window_cells = focal_window_cells,
  seed_quantile = seed_quantile,
  growth_quantile = growth_quantile,
  patch_directions = 8,
  grid_offset_m = c(0, 0),
  wood_cluster_thresholds = wood_cluster_thresholds,
  concrete_cluster_thresholds = concrete_cluster_thresholds
)

grid_materials <- results$grid_materials
wood_clusters <- results$wood_clusters
concrete_clusters <- results$concrete_clusters
wood_cluster_summary <- results$wood_cluster_summary
concrete_cluster_summary <- results$concrete_cluster_summary


# ==============================================================================
# 3. Convert cluster rasters to polygons
# ==============================================================================

wood_cluster_map <- terra::as.polygons(
  wood_clusters,
  dissolve = TRUE,
  na.rm = TRUE
)

concrete_cluster_map <- terra::as.polygons(
  concrete_clusters,
  dissolve = TRUE,
  na.rm = TRUE
)

names(wood_cluster_map) <- "cluster_id"
names(concrete_cluster_map) <- "cluster_id"


# ==============================================================================
# 4. Attach material stocks and classifications
# ==============================================================================

wood_match <- match(
  wood_cluster_map$cluster_id,
  wood_cluster_summary$cluster_id
)

concrete_match <- match(
  concrete_cluster_map$cluster_id,
  concrete_cluster_summary$cluster_id
)

stopifnot(!anyNA(wood_match), !anyNA(concrete_match))

wood_cluster_map$material_t <-
  wood_cluster_summary$material_t[wood_match]

concrete_cluster_map$material_t <-
  concrete_cluster_summary$material_t[concrete_match]

wood_cluster_map$map_class <- factor(
  as.character(wood_cluster_summary$class[wood_match]),
  levels = c("below_threshold", "significant", "medium", "high"),
  labels = c("< 500 t", "500–1,999 t", "2,000–4,999 t", "≥ 5,000 t")
)

concrete_cluster_map$map_class <- factor(
  as.character(concrete_cluster_summary$class[concrete_match]),
  levels = c("below_threshold", "significant", "medium", "high"),
  labels = c("< 5,000 t", "5,000–19,999 t", "20,000–49,999 t", "≥ 50,000 t")
)

wood_cluster_map$class <- as.character(
  wood_cluster_summary$class[wood_match]
)

concrete_cluster_map$class <- as.character(
  concrete_cluster_summary$class[concrete_match]
)


# ==============================================================================
# 5. Retain clusters meeting the minimum material stock thresholds
# ==============================================================================

wood_accepted <- wood_cluster_map[
  !is.na(wood_cluster_map$class) &
    wood_cluster_map$class != "below_threshold",
]

concrete_accepted <- concrete_cluster_map[
  !is.na(concrete_cluster_map$class) &
    concrete_cluster_map$class != "below_threshold",
]


# ==============================================================================
# 6. Report clustering parameters and results
# ==============================================================================

cat("\nClustering parameters:\n")
cat("Grid resolution:", grid_size_m, "m\n")
cat("Moving window:", focal_window_cells, "x", focal_window_cells,
    "cells (", grid_size_m * focal_window_cells, "m wide)\n")
cat("Seed quantile:", seed_quantile, "\n")
cat("Growth quantile:", growth_quantile, "\n")
cat("Connectivity: 8 directions\n")

threshold_summary <- data.frame(
  material = c("wood", "concrete"),
  seed_threshold_t = c(
    as.numeric(results$thresholds$wood_seed),
    as.numeric(results$thresholds$concrete_seed)
  ),
  growth_threshold_t = c(
    as.numeric(results$thresholds$wood_growth),
    as.numeric(results$thresholds$concrete_growth)
  )
)

cluster_report <- data.frame(
  material = c("wood", "concrete"),
  all_clusters = c(nrow(wood_cluster_summary), nrow(concrete_cluster_summary)),
  accepted_clusters = c(nrow(wood_accepted), nrow(concrete_accepted)),
  accepted_material_t = c(
    sum(wood_accepted$material_t, na.rm = TRUE),
    sum(concrete_accepted$material_t, na.rm = TRUE)
  )
)

cat("\nFocal-sum thresholds (tonnes):\n")
print(threshold_summary, row.names = FALSE)

cat("\nMaterial clusters:\n")
print(cluster_report, row.names = FALSE)

# Cluster material amounts are summed from original grid-cell values,
# not from overlapping moving-window sums.


# ==============================================================================
# 7. Save clusters and cluster ID rasters
# ==============================================================================

st_write(
  st_as_sf(wood_accepted),
  "data/processed/wood_clusters.gpkg",
  delete_dsn = TRUE
)

st_write(
  st_as_sf(concrete_accepted),
  "data/processed/concrete_clusters.gpkg",
  delete_dsn = TRUE
)

# These rasters retain all seed-connected clusters, including clusters below
# the accepted material thresholds. Downstream scripts match accepted IDs.
writeRaster(
  wood_clusters,
  "data/processed/wood_cluster_ids.tif",
  overwrite = TRUE
)

writeRaster(
  concrete_clusters,
  "data/processed/concrete_cluster_ids.tif",
  overwrite = TRUE
)

cat("\nCluster outputs saved to data/processed/.\n")


# ==============================================================================
# 8. Optional map review
# ==============================================================================

if (show_maps) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("Install the mapview package to display interactive maps.")
  }
  
  cluster_colours <- c("grey70", "#FFD54F", "#FF8C42", "#D73027")
  
  print(mapview::mapview(
    wood_cluster_map,
    zcol = "map_class",
    col.regions = cluster_colours,
    alpha.regions = 0.75,
    layer.name = "Wood clusters",
    map.types = "CartoDB.Positron"
  ))
  
  print(mapview::mapview(
    concrete_cluster_map,
    zcol = "map_class",
    col.regions = cluster_colours,
    alpha.regions = 0.75,
    layer.name = "Concrete clusters",
    map.types = "CartoDB.Positron"
  ))
}

