# ==============================================================================
# Identify spatial clusters of building material stocks
#
# Aggregates building-level wood and concrete stocks to a regular grid and
# identifies spatial material concentrations using focal sums and contiguous
# high-material grid cells.
#
# Input:
#   Building-level material stock data
#   (data/processed/buildings_material_stock_joensuu.gpkg)
#
# Output:
#   Grid-level material stocks and wood and concrete cluster results
# ==============================================================================

library(sf)
library(dplyr)
library(terra)
library(igraph)
library(mapview)


# ==============================================================================
# 1. Define analysis parameters
# ==============================================================================

# Grid-cell width and height in metres
grid_size_m <- 100

# Focal-window width and height in grid cells
# The value must be an odd number: 3, 5, 7, ...
focal_window_cells <- 3

# Upper focal-sum quantile used to identify cluster cores
seed_quantile <- 0.90

# Lower focal-sum quantile used to expand clusters around their cores
growth_quantile <- 0.80

# Cluster-level material thresholds for wood, in tonnes
wood_cluster_thresholds <- c(
  significant = 500,
  medium = 2000,
  high = 5000
)

# Cluster-level material thresholds for concrete, in tonnes
concrete_cluster_thresholds <- c(
  significant = 5000,
  medium = 20000,
  high = 50000
)

# Check parameter values
stopifnot(
  grid_size_m > 0,
  focal_window_cells >= 3,
  focal_window_cells %% 2 == 1,
  seed_quantile > growth_quantile,
  seed_quantile < 1,
  growth_quantile > 0
)

# EPSG:3067 is used because grid dimensions are defined in metres.
# Grid size, focal-window size, grid origin and focal-sum thresholds should
# be tested separately in 04b_cluster_sensitivity.R.

# ==============================================================================
# 2. Read building-level material stock data
# ==============================================================================

buil <- vect("data/processed/buildings_material_stock_joensuu.gpkg")
buil <- project(buil, "EPSG:3067")

# Verify that the required material columns are available
stopifnot(
  "material_wood_t" %in% names(buil),
  "material_concrete_t" %in% names(buil)
)

cat("Buildings:", nrow(buil), "\n")
cat("CRS:", crs(buil, describe = TRUE)$name, "\n")


# ==============================================================================
# 3. Prepare building attributes
# ==============================================================================

# Add a building count variable for raster aggregation
buil$building_count <- 1

# Identify buildings with available material data
buil$buildings_with_wood_data <-
  as.integer(!is.na(buil$material_wood_t))

buil$buildings_with_concrete_data <-
  as.integer(!is.na(buil$material_concrete_t))

# Replace missing material quantities with zero
buil$material_wood_t[is.na(buil$material_wood_t)] <- 0
buil$material_concrete_t[is.na(buil$material_concrete_t)] <- 0

# Use one point per building for assigning buildings to grid cells
buil_points <- centroids(buil, inside = TRUE)

# ==============================================================================
# 4. Aggregate buildings to raster grid
# ==============================================================================

# Create an empty raster grid covering the building dataset
# using the defined grid-cell size and the building-data CRS
grid <- rast(
  ext(buil),
  resolution = grid_size_m,
  crs = crs(buil)
)

# Aggregate building-level information to grid cells
# by summing the values of buildings located within each cell
grid_materials <- rasterize(
  buil_points,
  grid,
  field = c(
    "building_count",
    "buildings_with_wood_data",
    "buildings_with_concrete_data",
    "material_wood_t",
    "material_concrete_t"
  ),
  fun = "sum",
  background = 0
)

# ==============================================================================
# 5. Calculate focal sums
# ==============================================================================

# Create a square moving window for focal calculations
w <- matrix(1, focal_window_cells, focal_window_cells)

# Calculate the total wood and concrete quantities within
# the moving window around each grid cell
focal_materials <- focal(
  grid_materials[[
    c("material_wood_t_sum", "material_concrete_t_sum")
  ]],
  w = w,
  fun = "sum",
  fillvalue = 0
)

# Rename the focal-sum raster layers
names(focal_materials) <- c(
  "focal_wood_t",
  "focal_concrete_t"
)

# Add the focal-sum layers to the grid-level raster data
grid_materials <- c(
  grid_materials,
  focal_materials
)


# ==============================================================================
# 6. Review focal sums
# ==============================================================================

# Wood
wood_map <- grid_materials[[
  c("material_wood_t_sum", "focal_wood_t")
]]

mapview(wood_map)

# Concrete
concrete_map <- grid_materials[[
  c("material_concrete_t_sum", "focal_concrete_t")
]]

mapview(concrete_map)






















# ==============================================================================
# 7. Calculate seed and growth thresholds
# ==============================================================================

# Extract positive focal-sum values
wood_values <- values(
  grid_materials[["focal_wood_t"]],
  na.rm = TRUE
)

concrete_values <- values(
  grid_materials[["focal_concrete_t"]],
  na.rm = TRUE
)

wood_values <- wood_values[wood_values > 0]
concrete_values <- concrete_values[concrete_values > 0]

# Calculate material-specific seed and growth thresholds
wood_seed_threshold <- quantile(
  wood_values,
  seed_quantile
)

concrete_seed_threshold <- quantile(
  concrete_values,
  seed_quantile
)

wood_growth_threshold <- quantile(
  wood_values,
  growth_quantile
)

concrete_growth_threshold <- quantile(
  concrete_values,
  growth_quantile
)

# Identify cells whose focal sums meet or exceed the seed thresholds
wood_seed <- grid_materials[["focal_wood_t"]] >=
  wood_seed_threshold

concrete_seed <- grid_materials[["focal_concrete_t"]] >=
  concrete_seed_threshold

# Rename and add the seed-cell layers
names(wood_seed) <- "wood_seed"
names(concrete_seed) <- "concrete_seed"

grid_materials <- c(
  grid_materials,
  wood_seed,
  concrete_seed
)

# ==============================================================================
# 8. Review seed cells
# ==============================================================================

wood_seed_map <- ifel(
  grid_materials[["wood_seed"]] == 1,
  1,
  NA
)

concrete_seed_map <- ifel(
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

# Nyt klustereita on runsaasti, mutta vielä ei ole arvioitu mitkä niistä ovat materiaalimäärältään merkittäviä!


# ==============================================================================
# 9. Grow clusters from seed cells
# ==============================================================================

# Group connected growth cells
wood_clusters <- patches(
  ifel(
    grid_materials[["focal_wood_t"]] >= wood_growth_threshold,
    1,
    NA
  ),
  directions = 8
)

concrete_clusters <- patches(
  ifel(
    grid_materials[["focal_concrete_t"]] >= concrete_growth_threshold,
    1,
    NA
  ),
  directions = 8
)

# Find cluster IDs containing seed cells
wood_seed_ids <- unique(values(
  mask(
    wood_clusters,
    ifel(grid_materials[["wood_seed"]], 1, NA)
  ),
  na.rm = TRUE
))

concrete_seed_ids <- unique(values(
  mask(
    concrete_clusters,
    ifel(grid_materials[["concrete_seed"]], 1, NA)
  ),
  na.rm = TRUE
))

# Keep only clusters containing seed cells
wood_clusters <- ifel(
  wood_clusters %in% wood_seed_ids,
  wood_clusters,
  NA
)

concrete_clusters <- ifel(
  concrete_clusters %in% concrete_seed_ids,
  concrete_clusters,
  NA
)

names(wood_clusters) <- "wood_cluster"
names(concrete_clusters) <- "concrete_cluster"

grid_materials <- c(
  grid_materials,
  wood_clusters,
  concrete_clusters
)




# ==============================================================================
# 9. Review clusters
# ==============================================================================

wood_cluster_map <- grid_materials[["wood_cluster"]]
concrete_cluster_map <- grid_materials[["concrete_cluster"]]

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
# 10. Calculate cluster material amounts
# ==============================================================================

wood_cluster_summary <- zonal(
  grid_materials[["material_wood_t_sum"]],
  wood_clusters,
  fun = "sum",
  na.rm = TRUE
)

concrete_cluster_summary <- zonal(
  grid_materials[["material_concrete_t_sum"]],
  concrete_clusters,
  fun = "sum",
  na.rm = TRUE
)

names(wood_cluster_summary) <- c(
  "cluster_id",
  "material_t"
)

names(concrete_cluster_summary) <- c(
  "cluster_id",
  "material_t"
)


# ==============================================================================
# 11. Classify clusters
# ==============================================================================

wood_cluster_summary$class <- cut(
  wood_cluster_summary$material_t,
  breaks = c(
    -Inf,
    wood_cluster_thresholds,
    Inf
  ),
  labels = c(
    "below_threshold",
    "significant",
    "medium",
    "high"
  ),
  right = FALSE
)

concrete_cluster_summary$class <- cut(
  concrete_cluster_summary$material_t,
  breaks = c(
    -Inf,
    concrete_cluster_thresholds,
    Inf
  ),
  labels = c(
    "below_threshold",
    "significant",
    "medium",
    "high"
  ),
  right = FALSE
)


# ==============================================================================
# 12. Review cluster classifications
# ==============================================================================

# Convert cluster rasters to polygons
wood_cluster_map <- as.polygons(
  wood_clusters,
  dissolve = TRUE,
  na.rm = TRUE
)

concrete_cluster_map <- as.polygons(
  concrete_clusters,
  dissolve = TRUE,
  na.rm = TRUE
)

names(wood_cluster_map) <- "cluster_id"
names(concrete_cluster_map) <- "cluster_id"

# Match polygon clusters with cluster summaries
wood_match <- match(
  wood_cluster_map$cluster_id,
  wood_cluster_summary$cluster_id
)

concrete_match <- match(
  concrete_cluster_map$cluster_id,
  concrete_cluster_summary$cluster_id
)

# Add material amounts and classification labels
wood_cluster_map$material_t <-
  wood_cluster_summary$material_t[wood_match]

wood_cluster_map$map_class <-
  as.character(wood_cluster_summary$class[wood_match])

concrete_cluster_map$material_t <-
  concrete_cluster_summary$material_t[concrete_match]

concrete_cluster_map$map_class <-
  as.character(concrete_cluster_summary$class[concrete_match])

# Replace class names with material ranges for the legend
wood_cluster_map$map_class <- factor(
  wood_cluster_map$map_class,
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
  concrete_cluster_map$map_class,
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

cluster_colours <- c(
  "grey70",
  "#FFD54F",
  "#FF8C42",
  "#D73027"
)

# Display wood clusters
mapview(
  wood_cluster_map,
  zcol = "map_class",
  col.regions = cluster_colours,
  alpha.regions = 0.75,
  layer.name = "Wood clusters",
  map.types = "CartoDB.Positron"
)

# Display concrete clusters
mapview(
  concrete_cluster_map,
  zcol = "map_class",
  col.regions = cluster_colours,
  alpha.regions = 0.75,
  layer.name = "Concrete clusters",
  map.types = "CartoDB.Positron"
)

