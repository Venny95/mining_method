
# ==============================================================================
# 04b_cluster_sensitivity.R
#
# Sensitivity of material clusters to grid resolution and neighbourhood size.
# Uses the same clustering method as 04_grid_clusters.R.
# ==============================================================================

library(terra)

source("functions/cluster_functions.R")


# ==============================================================================
# 1. Parameters and building data
# ==============================================================================

seed_quantile <- 0.90
growth_quantile <- 0.80

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

buil <- terra::vect(
  "data/processed/buildings_material_stock_joensuu.gpkg"
)


# ==============================================================================
# 2. Sensitivity scenarios
# ==============================================================================

# Grid resolution:
# Test whether the spatial aggregation of buildings changes the clusters.
# Odd focal windows keep neighbourhood widths reasonably comparable.
#
# Neighbourhood size:
# Test how the spatial scale of local material concentrations affects
# cluster identification while keeping grid resolution fixed.
#
# The baseline reproduces the parameter settings of 04_grid_clusters.R.

scenarios <- data.frame(
  scenario = c(
    "baseline",
    "grid_50m",
    "grid_75m",
    "grid_150m",
    "neighbourhood_500m",
    "neighbourhood_700m"
  ),
  test_type = c(
    "baseline",
    rep("grid resolution", 3),
    rep("neighbourhood size", 2)
  ),
  grid_size_m = c(
    100, 50, 75, 150, 100, 100
  ),
  focal_window_cells = c(
    3, 7, 5, 3, 5, 7
  )
)

scenarios$neighbourhood_width_m <-
  scenarios$grid_size_m * scenarios$focal_window_cells


# ==============================================================================
# 3. Run sensitivity scenarios
# ==============================================================================

run_scenario <- function(i) {
  
  s <- scenarios[i, ]
  
  cat("Running:", s$scenario, "\n")
  
  results <- calculate_clusters(
    buil,
    grid_size_m = s$grid_size_m,
    focal_window_cells = s$focal_window_cells,
    seed_quantile = seed_quantile,
    growth_quantile = growth_quantile,
    wood_cluster_thresholds = wood_cluster_thresholds,
    concrete_cluster_thresholds = concrete_cluster_thresholds
  )
  
  # Summarise both materials using the same cluster classification
  # as the main analysis. Accepted clusters meet the significant
  # material threshold; smaller clusters are retained in the totals.
  
  summaries <- list(
    wood = results$wood_cluster_summary,
    concrete = results$concrete_cluster_summary
  )
  
  do.call(rbind, lapply(names(summaries), function(material) {
    
    x <- summaries[[material]]
    accepted <- x$class != "below_threshold"
    
    data.frame(
      scenario = s$scenario,
      test_type = s$test_type,
      material = material,
      grid_size_m = s$grid_size_m,
      focal_window_cells = s$focal_window_cells,
      neighbourhood_width_m = s$neighbourhood_width_m,
      cluster_count = nrow(x),
      accepted_cluster_count = sum(accepted),
      accepted_material_t = sum(x$material_t[accepted]),
      median_cluster_material_t = median(x$material_t),
      maximum_cluster_material_t = max(x$material_t)
    )
  }))
}


# ==============================================================================
# 4. Run and review results
# ==============================================================================

sensitivity_summary <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), run_scenario)
)

rownames(sensitivity_summary) <- NULL

print(sensitivity_summary)


# ==============================================================================
# 5. Save results (optional)
# ==============================================================================

# write.csv(
#   sensitivity_summary,
#   "data/processed/cluster_sensitivity_summary.csv",
#   row.names = FALSE
# )


# ==============================================================================
# 6. Compare cluster locations across selected scenarios
# ==============================================================================

library(mapview)

# Compare the baseline with a finer grid and a wider neighbourhood.
# The aim is to assess whether major material concentrations remain
# in the same locations or merge into larger spatial clusters.

map_scenarios <- c(
  "baseline",
  "grid_50m",
  "neighbourhood_700m"
)

map_colours <- c(
  baseline = "#D73027",
  grid_50m = "#2C7FB8",
  neighbourhood_700m = "#31A354"
)

cluster_maps <- list(wood = list(), concrete = list())

for (scenario_name in map_scenarios) {
  
  s <- scenarios[scenarios$scenario == scenario_name, ]
  
  results <- calculate_clusters(
    buil,
    grid_size_m = s$grid_size_m,
    focal_window_cells = s$focal_window_cells,
    seed_quantile = seed_quantile,
    growth_quantile = growth_quantile,
    wood_cluster_thresholds = wood_cluster_thresholds,
    concrete_cluster_thresholds = concrete_cluster_thresholds
  )
  
  for (material in c("wood", "concrete")) {
    
    clusters <- results[[paste0(material, "_clusters")]]
    summary <- results[[paste0(material, "_cluster_summary")]]
    
    # Show only clusters meeting the minimum material threshold.
    # Cluster IDs are preserved so individual concentrations remain visible.
    accepted_ids <- summary$cluster_id[
      summary$class != "below_threshold"
    ]
    
    accepted_raster <- terra::ifel(
      clusters %in% accepted_ids,
      clusters,
      NA
    )
    
    names(accepted_raster) <- paste(material, scenario_name)
    
    cluster_maps[[material]][[scenario_name]] <- terra::as.polygons(
      accepted_raster,
      dissolve = TRUE,
      na.rm = TRUE
    )
  }
}

# Overlay the scenarios. Toggle layers to compare cluster boundaries.
make_comparison_map <- function(material) {
  
  layers <- lapply(map_scenarios, function(scenario_name) {
    
    mapview(
      cluster_maps[[material]][[scenario_name]],
      col.regions = map_colours[[scenario_name]],
      alpha.regions = 0.35,
      legend = FALSE,
      layer.name = scenario_name,
      map.types = "CartoDB.Positron"
    )
  })
  
  Reduce(`+`, layers)
}

wood_comparison_map <- make_comparison_map("wood")
concrete_comparison_map <- make_comparison_map("concrete")

wood_comparison_map
concrete_comparison_map

