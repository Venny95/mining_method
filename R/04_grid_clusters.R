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

# Focal window width and height in grid cells
# Must be an odd number: 3, 5, 7, ...
focal_window_cells <- 3

# Relative threshold for identifying high focal material sums
# A value of 0.90 selects cells within the highest 10% of positive focal sums
high_material_quantile <- 0.90

# Material-quantity thresholds for classifying wood clusters, in tonnes
wood_cluster_thresholds <- c(
  low = 500,
  medium = 2000,
  high = 5000
)

# Material-quantity thresholds for classifying concrete clusters, in tonnes
concrete_cluster_thresholds <- c(
  low = 5000,
  medium = 20000,
  high = 50000
)

# EPSG:3067 is used because grid dimensions are defined in metres.
# Grid size, focal-window size and grid origin are tested separately in
# 04b_cluster_sensitivity.R to evaluate sensitivity to the MAUP.

# ==============================================================================
# 2. Read building-level material stock data
# ==============================================================================

buil <- st_read(
  "data/processed/buildings_material_stock_joensuu.gpkg",
  quiet = TRUE
) |>
  st_transform(3067)

# Verify that the required material columns are available
stopifnot(
  "material_wood_t" %in% names(buil),
  "material_concrete_t" %in% names(buil)
)

cat("Buildings:", nrow(buil), "\n")
cat("CRS:", st_crs(buil)$input, "\n")

# ==============================================================================
# 3. Create regular grid
# ==============================================================================

# Create a square grid covering the extent of the building dataset
grid <- st_make_grid(
  buil,
  cellsize = grid_size_m,
  square = TRUE
) |>
  st_sf() |>
  mutate(
    grid_id = row_number()
  )

cat("All grid cells:", nrow(grid), "\n")

# ==============================================================================
# 4. Assign buildings to grid cells
# ==============================================================================

# Add a unique row identifier to verify that each building is retained once
buil <- buil |>
  mutate(building_row_id = row_number())

# Assign buildings located strictly within grid cells
buil_grid <- st_join(
  buil,
  grid["grid_id"],
  join = st_within,
  left = TRUE
)

cat(
  "Buildings assigned with st_within:",
  sum(!is.na(buil_grid$grid_id)),
  "\n"
)

cat(
  "Buildings on grid boundaries:",
  sum(is.na(buil_grid$grid_id)),
  "\n"
)

# Identify buildings that were not assigned because they lie exactly on
# grid-cell boundaries
missing_grid <- which(is.na(buil_grid$grid_id))

# Assign boundary buildings to the grid cell with the nearest centroid
if (length(missing_grid) > 0) {
  
  grid_centroids <- st_centroid(
    st_geometry(grid)
  )
  
  nearest_grid <- st_nearest_feature(
    buil_grid[missing_grid, ],
    grid_centroids
  )
  
  buil_grid$grid_id[missing_grid] <-
    grid$grid_id[nearest_grid]
}

# Verify that all buildings were assigned exactly once
stopifnot(
  nrow(buil_grid) == nrow(buil),
  !any(is.na(buil_grid$grid_id)),
  !anyDuplicated(buil_grid$building_row_id)
)

cat(
  "Buildings assigned to grid:",
  nrow(buil_grid),
  "\n"
)
cat(
  "Unassigned buildings:",
  sum(is.na(buil_grid$grid_id)),
  "\n"
)

# ==============================================================================
# 5. Aggregate material stocks by grid cell
# ==============================================================================

# Calculate building counts, data availability and material quantities
# for each occupied grid cell
grid_summary <- buil_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarise(
    building_count = n(),
    
    buildings_with_wood_data =
      sum(!is.na(material_wood_t)),
    
    buildings_with_concrete_data =
      sum(!is.na(material_concrete_t)),
    
    material_wood_t =
      sum(material_wood_t, na.rm = TRUE),
    
    material_concrete_t =
      sum(material_concrete_t, na.rm = TRUE),
    
    .groups = "drop"
  )

# Join the aggregated values to the complete grid
grid_materials <- grid |>
  left_join(
    grid_summary,
    by = "grid_id"
  )

# Assign zero values to grid cells without buildings
grid_materials <- grid_materials |>
  mutate(
    building_count =
      coalesce(building_count, 0L),
    
    buildings_with_wood_data =
      coalesce(buildings_with_wood_data, 0L),
    
    buildings_with_concrete_data =
      coalesce(buildings_with_concrete_data, 0L),
    
    material_wood_t =
      coalesce(material_wood_t, 0),
    
    material_concrete_t =
      coalesce(material_concrete_t, 0)
  )

# Review the distributions of grid-level material quantities
summary(grid_materials$material_wood_t)
summary(grid_materials$material_concrete_t)

# ==============================================================================
# 6. Define function for identifying material clusters
# ==============================================================================

create_focal_material_clusters <- function(
    grid_data,
    material_column,
    material_name,
    cluster_thresholds,
    grid_size,
    focal_window = 3,
    threshold_quantile = 0.90
) {
  
  # Convert the grid polygons and their material quantities to raster format
  grid_vector <- terra::vect(grid_data)
  
  material_raster <- terra::rasterize(
    grid_vector,
    terra::rast(
      terra::ext(grid_vector),
      resolution = grid_size,
      crs = terra::crs(grid_vector)
    ),
    field = material_column,
    background = 0
  )
  
  # Calculate the total material quantity within each moving neighbourhood
  focal_raster <- terra::focal(
    material_raster,
    w = focal_window,
    fun = "sum",
    na.rm = TRUE,
    fillvalue = 0
  )
  
  names(focal_raster) <- "focal_material_t"
  
  # Extract focal sums to the corresponding grid cells
  focal_values <- terra::extract(
    focal_raster,
    terra::vect(
      st_point_on_surface(
        st_geometry(grid_data)
      )
    )
  )
  
  grid_focal <- grid_data |>
    mutate(
      focal_material_t = focal_values$focal_material_t
    )
  
  # Calculate the relative threshold from positive focal sums
  threshold <- quantile(
    grid_focal$focal_material_t[
      grid_focal$focal_material_t > 0
    ],
    probs = threshold_quantile,
    na.rm = TRUE,
    names = FALSE
  )
  
  # Select material-containing cells in high-material neighbourhoods
  high_cells <- grid_focal |>
    filter(
      focal_material_t >= threshold,
      .data[[material_column]] > 0
    )
  
  # Form clusters from cells sharing a common edge
  adjacency <- st_relate(
    high_cells,
    high_cells,
    pattern = "F***1****",
    sparse = FALSE
  )
  
  # Use connected components to combine adjacent cells into clusters
  graph <- igraph::graph_from_adjacency_matrix(
    adjacency,
    mode = "undirected",
    diag = FALSE
  )
  
  high_cells$cluster_id <- paste0(
    material_name,
    "_",
    igraph::components(graph)$membership
  )
  
  # Calculate cluster-level statistics and classify clusters by material amount
  clusters <- high_cells |>
    group_by(cluster_id) |>
    summarise(
      material_type = material_name,
      grid_count = n(),
      building_count = sum(building_count, na.rm = TRUE),
      material_t = sum(
        .data[[material_column]],
        na.rm = TRUE
      ),
      maximum_focal_material_t = max(
        focal_material_t,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      material_scenario = case_when(
        material_t >= cluster_thresholds["high"] ~ "high",
        material_t >= cluster_thresholds["medium"] ~ "medium",
        material_t >= cluster_thresholds["low"] ~ "low",
        TRUE ~ "below threshold"
      ),
      material_scenario = factor(
        material_scenario,
        levels = c(
          "below threshold",
          "low",
          "medium",
          "high"
        ),
        ordered = TRUE
      )
    )
  
  # Return the threshold and intermediate and final spatial results
  list(
    threshold = threshold,
    grid_focal = grid_focal,
    high_cells = high_cells,
    clusters = clusters
  )
}

# ==============================================================================
# 7. Identify wood material clusters
# ==============================================================================

wood_results <- create_focal_material_clusters(
  grid_data = grid_materials,
  material_column = "material_wood_t",
  material_name = "wood",
  cluster_thresholds = wood_cluster_thresholds,
  grid_size = grid_size_m,
  focal_window = focal_window_cells,
  threshold_quantile = high_material_quantile
)

# ==============================================================================
# 8. Identify concrete material clusters
# ==============================================================================

concrete_results <- create_focal_material_clusters(
  grid_data = grid_materials,
  material_column = "material_concrete_t",
  material_name = "concrete",
  cluster_thresholds = concrete_cluster_thresholds,
  grid_size = grid_size_m,
  focal_window = focal_window_cells,
  threshold_quantile = high_material_quantile
)

# ==============================================================================
# 9. Review cluster results
# ==============================================================================

# List wood clusters from the largest to the smallest material quantity
wood_results$clusters |>
  st_drop_geometry() |>
  select(
    cluster_id,
    grid_count,
    material_t,
    material_scenario
  ) |>
  arrange(desc(material_t))

# List concrete clusters from the largest to the smallest material quantity
concrete_results$clusters |>
  st_drop_geometry() |>
  select(
    cluster_id,
    grid_count,
    material_t,
    material_scenario
  ) |>
  arrange(desc(material_t))

# Count clusters by material scenario
table(wood_results$clusters$material_scenario)
table(concrete_results$clusters$material_scenario)

# ==============================================================================
# 10. Visualize cluster results
# ==============================================================================

# Define a common colour palette for material scenarios
scenario_palette <- c(
  "below threshold" = "#D9D9D9",
  "low" = "#FFFFB2",
  "medium" = "#FD8D3C",
  "high" = "#BD0026"
)

# Visualize wood clusters
mapview(
  wood_results$clusters,
  zcol = "material_scenario",
  col.regions = scenario_palette,
  color = "grey30",
  lwd = 1,
  alpha.regions = 0.8,
  legend = TRUE,
  map.types = "CartoDB.Positron",
  layer.name = "Wood material scenario"
)

# Visualize concrete clusters
mapview(
  concrete_results$clusters,
  zcol = "material_scenario",
  col.regions = scenario_palette,
  color = "grey30",
  lwd = 1,
  alpha.regions = 0.8,
  legend = TRUE,
  map.types = "CartoDB.Positron",
  layer.name = "Concrete material scenario"
)

