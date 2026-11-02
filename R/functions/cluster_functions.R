# Shared clustering calculation for 04_grid_clusters.R and 04b_cluster_sensitivity.R.
# The calculation follows the original 04 script without methodological changes.

calculate_clusters <- function(
    buil, grid_size_m = 100, focal_window_cells = 3,
    seed_quantile = 0.90, growth_quantile = 0.80,
    wood_cluster_thresholds = c(significant = 500, medium = 2000, high = 5000),
    concrete_cluster_thresholds = c(significant = 5000, medium = 20000, high = 50000)
) {
  stopifnot(
    grid_size_m > 0,
    focal_window_cells >= 3,
    focal_window_cells %% 2 == 1,
    seed_quantile > growth_quantile,
    seed_quantile < 1,
    growth_quantile > 0
  )
  
  # 1. Prepare building attributes (same as original 04).
  buil <- terra::project(buil, "EPSG:3067")
  stopifnot(
    "material_wood_t" %in% names(buil),
    "material_concrete_t" %in% names(buil)
  )
  buil$building_count <- 1
  buil$buildings_with_wood_data <- as.integer(!is.na(buil$material_wood_t))
  buil$buildings_with_concrete_data <- as.integer(!is.na(buil$material_concrete_t))
  buil$material_wood_t[is.na(buil$material_wood_t)] <- 0
  buil$material_concrete_t[is.na(buil$material_concrete_t)] <- 0
  buil_points <- terra::centroids(buil, inside = TRUE)
  
  # 2. Aggregate building attributes to a grid.
  grid <- terra::rast(
    terra::ext(buil), resolution = grid_size_m, crs = terra::crs(buil)
  )
  grid_materials <- terra::rasterize(
    buil_points, grid,
    field = c(
      "building_count", "buildings_with_wood_data",
      "buildings_with_concrete_data", "material_wood_t", "material_concrete_t"
    ),
    fun = "sum", background = 0
  )
  
  # 3. Focal sums identify local concentrations; do not use them
  # to calculate cluster stock totals (that would double-count material).
  w <- matrix(1, focal_window_cells, focal_window_cells)
  focal_materials <- terra::focal(
    grid_materials[[c("material_wood_t_sum", "material_concrete_t_sum")]],
    w = w, fun = "sum", fillvalue = 0
  )
  names(focal_materials) <- c("focal_wood_t", "focal_concrete_t")
  grid_materials <- c(grid_materials, focal_materials)
  
  # 4. Quantiles are based on positive focal sums only.
  wood_values <- terra::values(grid_materials[["focal_wood_t"]], na.rm = TRUE)
  concrete_values <- terra::values(grid_materials[["focal_concrete_t"]], na.rm = TRUE)
  wood_values <- wood_values[wood_values > 0]
  concrete_values <- concrete_values[concrete_values > 0]
  
  wood_seed_threshold <- stats::quantile(wood_values, seed_quantile)
  concrete_seed_threshold <- stats::quantile(concrete_values, seed_quantile)
  wood_growth_threshold <- stats::quantile(wood_values, growth_quantile)
  concrete_growth_threshold <- stats::quantile(concrete_values, growth_quantile)
  
  # 5. Seed cells mark cluster cores (upper quantile).
  wood_seed <- grid_materials[["focal_wood_t"]] >= wood_seed_threshold
  concrete_seed <- grid_materials[["focal_concrete_t"]] >= concrete_seed_threshold
  names(wood_seed) <- "wood_seed"
  names(concrete_seed) <- "concrete_seed"
  grid_materials <- c(grid_materials, wood_seed, concrete_seed)
  
  # 6. Growth cells form patches with eight-direction connectivity.
  # Retain only patches containing at least one seed cell.
  wood_clusters <- terra::patches(
    terra::ifel(grid_materials[["focal_wood_t"]] >= wood_growth_threshold, 1, NA),
    directions = 8
  )
  concrete_clusters <- terra::patches(
    terra::ifel(grid_materials[["focal_concrete_t"]] >= concrete_growth_threshold, 1, NA),
    directions = 8
  )
  wood_seed_ids <- unique(terra::values(
    terra::mask(wood_clusters, terra::ifel(grid_materials[["wood_seed"]], 1, NA)),
    na.rm = TRUE
  ))
  concrete_seed_ids <- unique(terra::values(
    terra::mask(concrete_clusters, terra::ifel(grid_materials[["concrete_seed"]], 1, NA)),
    na.rm = TRUE
  ))
  wood_clusters <- terra::ifel(wood_clusters %in% wood_seed_ids, wood_clusters, NA)
  concrete_clusters <- terra::ifel(concrete_clusters %in% concrete_seed_ids, concrete_clusters, NA)
  names(wood_clusters) <- "wood_cluster"
  names(concrete_clusters) <- "concrete_cluster"
  grid_materials <- c(grid_materials, wood_clusters, concrete_clusters)
  
  # 7. Sum original grid-cell material stocks by cluster ID.
  wood_cluster_summary <- terra::zonal(
    grid_materials[["material_wood_t_sum"]], wood_clusters,
    fun = "sum", na.rm = TRUE
  )
  concrete_cluster_summary <- terra::zonal(
    grid_materials[["material_concrete_t_sum"]], concrete_clusters,
    fun = "sum", na.rm = TRUE
  )
  names(wood_cluster_summary) <- c("cluster_id", "material_t")
  names(concrete_cluster_summary) <- c("cluster_id", "material_t")
  
  # 8. Classify cluster totals using the original material thresholds.
  wood_cluster_summary$class <- cut(
    wood_cluster_summary$material_t,
    breaks = c(-Inf, wood_cluster_thresholds, Inf),
    labels = c("below_threshold", "significant", "medium", "high"),
    right = FALSE
  )
  concrete_cluster_summary$class <- cut(
    concrete_cluster_summary$material_t,
    breaks = c(-Inf, concrete_cluster_thresholds, Inf),
    labels = c("below_threshold", "significant", "medium", "high"),
    right = FALSE
  )
  
  list(
    grid_materials = grid_materials,
    wood_clusters = wood_clusters,
    concrete_clusters = concrete_clusters,
    wood_cluster_summary = wood_cluster_summary,
    concrete_cluster_summary = concrete_cluster_summary,
    thresholds = list(
      wood_seed = wood_seed_threshold,
      wood_growth = wood_growth_threshold,
      concrete_seed = concrete_seed_threshold,
      concrete_growth = concrete_growth_threshold
    )
  )
}