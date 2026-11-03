
# ==============================================================================
# 02_grid_clusters.R
#
# Identify spatial clusters of building material stocks.
# Clustering calculation is shared with 04b_cluster_sensitivity.R.
# ==============================================================================

library(terra)
library(mapview)
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
# 3. Review focal material sums
# ==============================================================================

wood_map <- grid_materials[[
  c("material_wood_t_sum", "focal_wood_t")
]]

concrete_map <- grid_materials[[
  c("material_concrete_t_sum", "focal_concrete_t")
]]

mapview(wood_map)
mapview(concrete_map)


# ==============================================================================
# 4. Review seed cells
# ==============================================================================

wood_seed_map <- terra::ifel(
  grid_materials[["wood_seed"]] == 1,
  1,
  NA
)

concrete_seed_map <- terra::ifel(
  grid_materials[["concrete_seed"]] == 1,
  1,
  NA
)

names(wood_seed_map) <- "wood_seeds"
names(concrete_seed_map) <- "concrete_seeds"

mapview(
  wood_seed_map,
  col.regions = "red",
  legend = FALSE
)

mapview(
  concrete_seed_map,
  col.regions = "blue",
  legend = FALSE
)


# ==============================================================================
# 5. Review cluster IDs
# ==============================================================================

wood_cluster_map <- wood_clusters
concrete_cluster_map <- concrete_clusters

names(wood_cluster_map) <- "wood_clusters"
names(concrete_cluster_map) <- "concrete_clusters"

mapview(
  wood_cluster_map,
  legend = FALSE
)

mapview(
  concrete_cluster_map,
  legend = FALSE
)


# ==============================================================================
# 6. Convert cluster rasters to polygons
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
# 7. Attach material amounts and classifications
# ==============================================================================

wood_match <- match(
  wood_cluster_map$cluster_id,
  wood_cluster_summary$cluster_id
)

concrete_match <- match(
  concrete_cluster_map$cluster_id,
  concrete_cluster_summary$cluster_id
)

wood_cluster_map$material_t <-
  wood_cluster_summary$material_t[wood_match]

concrete_cluster_map$material_t <-
  concrete_cluster_summary$material_t[concrete_match]

wood_cluster_map$map_class <- factor(
  as.character(wood_cluster_summary$class[wood_match]),
  levels = c(
    "below_threshold",
    "significant",
    "medium",
    "high"
  ),
  labels = c(
    "< 500 t",
    "500–1,999 t",
    "2,000–4,999 t",
    "≥ 5,000 t"
  )
)

concrete_cluster_map$map_class <- factor(
  as.character(concrete_cluster_summary$class[concrete_match]),
  levels = c(
    "below_threshold",
    "significant",
    "medium",
    "high"
  ),
  labels = c(
    "< 5,000 t",
    "5,000–19,999 t",
    "20,000–49,999 t",
    "≥ 50,000 t"
  )
)


# ==============================================================================
# 8. Review classified material clusters
# ==============================================================================

cluster_colours <- c(
  "grey70",
  "#FFD54F",
  "#FF8C42",
  "#D73027"
)

mapview(
  wood_cluster_map,
  zcol = "map_class",
  col.regions = cluster_colours,
  alpha.regions = 0.75,
  layer.name = "Wood clusters",
  map.types = "CartoDB.Positron"
)

mapview(
  concrete_cluster_map,
  zcol = "map_class",
  col.regions = cluster_colours,
  alpha.regions = 0.75,
  layer.name = "Concrete clusters",
  map.types = "CartoDB.Positron"
)



# ==============================================================================
# 9. Save accepted material clusters
# ==============================================================================

# Add original cluster classifications
wood_cluster_map$class <- as.character(
  wood_cluster_summary$class[wood_match]
)

concrete_cluster_map$class <- as.character(
  concrete_cluster_summary$class[concrete_match]
)

# Keep clusters meeting the minimum material threshold
wood_accepted <- wood_cluster_map[
  !is.na(wood_cluster_map$class) &
    wood_cluster_map$class != "below_threshold",
]

concrete_accepted <- concrete_cluster_map[
  !is.na(concrete_cluster_map$class) &
    concrete_cluster_map$class != "below_threshold",
]

# Convert to sf and save as GeoPackages
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

cat("Saved", nrow(wood_accepted), "wood clusters\n")
cat("Saved", nrow(concrete_accepted), "concrete clusters\n")

# Save cluster rasters for building-to-cluster assignment
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

