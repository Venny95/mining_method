
# ==============================================================================
# 04b_cluster_sensitivity.R
#
# Sensitivity analysis of the urban material mining clustering method.
#
# Tests:
# 1. Grid resolution
# 2. Focal neighbourhood size
# 3. Seed quantile
# 4. Growth quantile
# 5. Patch connectivity
# 6. Grid origin offset
#
# Additional analysis:
# 7. Material classification thresholds
#
# All scenarios use the same clustering function.
# ==============================================================================


# ==============================================================================
# 1. Libraries and input data
# ==============================================================================

library(terra)
library(mapview)
library(sf)

source("functions/cluster_functions.R")
source("functions/cluster_metrics.R")

buil <- terra::vect(
  "data/processed/buildings_material_stock_joensuu.gpkg"
)

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
# 2. Define sensitivity scenarios
# ==============================================================================

# The baseline represents the reference method.
#
# Change one parameter at a time, except that the physical focal
# neighbourhood necessarily changes with grid resolution.

scenarios <- data.frame(
  scenario = c(
    "baseline",
    "grid_50m",
    "grid_150m",
    "focal_5",
    "focal_7",
    "seed_085",
    "seed_095",
    "growth_075",
    "growth_085",
    "connectivity_4",
    "offset_50_0",
    "offset_0_50",
    "offset_50_50"
  ),
  
  test_type = c(
    "baseline",
    "grid resolution",
    "grid resolution",
    "focal neighbourhood",
    "focal neighbourhood",
    "seed quantile",
    "seed quantile",
    "growth quantile",
    "growth quantile",
    "connectivity",
    "grid origin",
    "grid origin",
    "grid origin"
  ),
  
  grid_size_m = c(
    100, 50, 150,
    100, 100,
    100, 100,
    100, 100,
    100,
    100, 100, 100
  ),
  
  focal_window_cells = c(
    3, 3, 3,
    5, 7,
    3, 3,
    3, 3,
    3,
    3, 3, 3
  ),
  
  seed_quantile = c(
    0.90, 0.90, 0.90,
    0.90, 0.90,
    0.85, 0.95,
    0.90, 0.90,
    0.90,
    0.90, 0.90, 0.90
  ),
  
  growth_quantile = c(
    0.80, 0.80, 0.80,
    0.80, 0.80,
    0.80, 0.80,
    0.75, 0.85,
    0.80,
    0.80, 0.80, 0.80
  ),
  
  patch_directions = c(
    8, 8, 8,
    8, 8,
    8, 8,
    8, 8,
    4,
    8, 8, 8
  ),
  
  offset_x_m = c(
    0, 0, 0,
    0, 0,
    0, 0,
    0, 0,
    0,
    50, 0, 50
  ),
  
  offset_y_m = c(
    0, 0, 0,
    0, 0,
    0, 0,
    0, 0,
    0,
    0, 50, 50
  )
)

# Calculate physical focal neighbourhood width.
scenarios$focal_width_m <-
  scenarios$grid_size_m * scenarios$focal_window_cells

print(scenarios)

# ==============================================================================
# 2b. Additional grid and focal neighbourhood combinations
# ==============================================================================

# Test smaller grid cells with different physical focal widths.
# Keep seed quantile, connectivity and grid origin fixed.

additional_scenarios <- expand.grid(
  grid_size_m = 50,
  focal_window_cells = c(3, 5, 7),
  growth_quantile = c(0.80, 0.85)
)

additional_scenarios$scenario <- sprintf(
  "grid_%sm_focal_%s_growth_%s",
  additional_scenarios$grid_size_m,
  additional_scenarios$focal_window_cells,
  gsub(
    "\\.",
    "",
    sprintf("%.2f", additional_scenarios$growth_quantile)
  )
)

additional_scenarios$test_type <- "grid-focal combination"
additional_scenarios$seed_quantile <- 0.90
additional_scenarios$patch_directions <- 8
additional_scenarios$offset_x_m <- 0
additional_scenarios$offset_y_m <- 0

additional_scenarios$focal_width_m <-
  additional_scenarios$grid_size_m *
  additional_scenarios$focal_window_cells

# Match the existing scenario table structure.
additional_scenarios <- additional_scenarios[
  , names(scenarios)
]

# Avoid duplicating the existing 50 m / 3x3 / 0.80 scenario.
additional_scenarios <- additional_scenarios[
  !(
    additional_scenarios$grid_size_m == 50 &
      additional_scenarios$focal_window_cells == 3 &
      additional_scenarios$growth_quantile == 0.80
  ),
]

scenarios <- rbind(
  scenarios,
  additional_scenarios
)

stopifnot(!anyDuplicated(scenarios$scenario))

print(scenarios)


# ==============================================================================
# 3. Run all scenarios
# ==============================================================================

# Store full results to allow later spatial comparisons.

scenario_results <- list()

for (i in seq_len(nrow(scenarios))) {
  
  s <- scenarios[i, ]
  
  cat(
    "Running scenario:",
    s$scenario,
    "(", i, "/", nrow(scenarios), ")\n"
  )
  
  scenario_results[[s$scenario]] <- calculate_clusters(
    buil = buil,
    grid_size_m = s$grid_size_m,
    focal_window_cells = s$focal_window_cells,
    seed_quantile = s$seed_quantile,
    growth_quantile = s$growth_quantile,
    patch_directions = s$patch_directions,
    grid_offset_m = c(s$offset_x_m, s$offset_y_m),
    wood_cluster_thresholds = wood_cluster_thresholds,
    concrete_cluster_thresholds = concrete_cluster_thresholds
  )
}


# ==============================================================================
# 4. Calculate cluster summary metrics
# ==============================================================================

# Metrics are calculated separately for wood and concrete.
#
# Cluster count: all seed-connected clusters.
# Accepted clusters: clusters exceeding the minimum stock threshold.
# Area: total area of accepted cluster cells.
# Density: accepted material stock divided by accepted area.
#
# Material density is based on original cell stocks, not focal sums.

calculate_metrics <- function(result, scenario_row, material) {
  
  clusters <- result[[paste0(material, "_clusters")]]
  summary <- result[[paste0(material, "_cluster_summary")]]
  
  accepted_ids <- summary$cluster_id[
    summary$class != "below_threshold"
  ]
  
  accepted_summary <- summary[
    summary$cluster_id %in% accepted_ids,
    ,
    drop = FALSE
  ]
  
  accepted_raster <- terra::ifel(
    clusters %in% accepted_ids,
    1,
    NA
  )
  
  cell_count <- terra::global(
    accepted_raster,
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  # Convert raster-cell count to hectares.
  area_ha <- cell_count *
    scenario_row$grid_size_m^2 / 10000
  
  accepted_material_t <- sum(accepted_summary$material_t)
  
  density_t_ha <- if (area_ha > 0) {
    accepted_material_t / area_ha
  } else {
    NA_real_
  }
  
  # Calculate the largest accepted cluster area.
  cluster_frequency <- terra::freq(clusters)
  
  accepted_frequency <- cluster_frequency[
    cluster_frequency$value %in% accepted_ids,
    ,
    drop = FALSE
  ]
  
  largest_area_ha <- if (nrow(accepted_frequency) > 0) {
    max(accepted_frequency$count) *
      scenario_row$grid_size_m^2 / 10000
  } else {
    NA_real_
  }
  
  # Total city stock, including buildings outside clusters.
  stock_layer <- paste0("material_", material, "_t_sum")
  
  total_material_t <- terra::global(
    result$grid_materials[[stock_layer]],
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  data.frame(
    scenario = scenario_row$scenario,
    test_type = scenario_row$test_type,
    material = material,
    grid_size_m = scenario_row$grid_size_m,
    focal_width_m = scenario_row$focal_width_m,
    cluster_count = nrow(summary),
    accepted_cluster_count = nrow(accepted_summary),
    accepted_area_ha = area_ha,
    accepted_material_t = accepted_material_t,
    material_share_pct = if (total_material_t > 0) {
      100 * accepted_material_t / total_material_t
    } else {
      NA_real_
    },
    density_t_ha = density_t_ha,
    largest_cluster_area_ha = largest_area_ha,
    largest_cluster_material_t = if (nrow(accepted_summary) > 0) {
      max(accepted_summary$material_t)
    } else {
      NA_real_
    }
  )
}

sensitivity_summary <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), function(i) {
    
    s <- scenarios[i, ]
    result <- scenario_results[[s$scenario]]
    
    rbind(
      calculate_metrics(result, s, "wood"),
      calculate_metrics(result, s, "concrete")
    )
  })
)

rownames(sensitivity_summary) <- NULL

print(sensitivity_summary)


# ==============================================================================
# 5. Calculate spatial overlap with baseline
# ==============================================================================

# Jaccard index measures spatial agreement:
#
# 1 = identical accepted-cluster coverage
# 0 = no overlap
#
# All masks are transferred to a common 25 m comparison grid.
# Nearest-neighbour resampling makes comparisons approximate near
# cell boundaries, especially between different grid resolutions.


make_accepted_mask <- function(result, material) {
  
  clusters <- result[[paste0(material, "_clusters")]]
  summary <- result[[paste0(material, "_cluster_summary")]]
  
  accepted_ids <- summary$cluster_id[
    summary$class != "below_threshold"
  ]
  
  terra::ifel(
    !is.na(clusters) & (clusters %in% accepted_ids),
    1,
    0
  )
}


# Create a common comparison grid covering all scenarios.
all_extents <- lapply(
  scenario_results,
  function(x) terra::ext(x$grid_materials)
)

comparison_extent <- terra::ext(
  min(vapply(all_extents, terra::xmin, numeric(1))),
  max(vapply(all_extents, terra::xmax, numeric(1))),
  min(vapply(all_extents, terra::ymin, numeric(1))),
  max(vapply(all_extents, terra::ymax, numeric(1)))
)

comparison_grid <- terra::rast(
  comparison_extent,
  resolution = 25,
  crs = terra::crs(
    scenario_results[["baseline"]]$grid_materials
  )
)

# Rasterize accepted coverage on the comparison grid.
# Missing cells outside the source extent are treated as zero.

align_mask <- function(mask) {
  
  aligned <- terra::resample(
    mask,
    comparison_grid,
    method = "near"
  )
  
  terra::ifel(is.na(aligned), 0, aligned)
}

spatial_overlap <- do.call(
  rbind,
  lapply(c("wood", "concrete"), function(material) {
    
    baseline_mask <- align_mask(
      make_accepted_mask(
        scenario_results[["baseline"]],
        material
      )
    )
    
    do.call(
      rbind,
      lapply(names(scenario_results), function(scenario) {
        
        comparison_mask <- align_mask(
          make_accepted_mask(
            scenario_results[[scenario]],
            material
          )
        )
        
        intersection <- terra::global(
          (baseline_mask == 1) & (comparison_mask == 1),
          "sum",
          na.rm = TRUE
        )[1, 1]
        
        union <- terra::global(
          (baseline_mask == 1) | (comparison_mask == 1),
          "sum",
          na.rm = TRUE
        )[1, 1]
        
        data.frame(
          scenario = scenario,
          material = material,
          jaccard = if (union > 0) {
            intersection / union
          } else {
            NA_real_
          }
        )
      })
    )
  })
)

sensitivity_summary <- merge(
  sensitivity_summary,
  spatial_overlap,
  by = c("scenario", "material"),
  sort = FALSE
)

# Restore the original scenario order.
sensitivity_summary <- sensitivity_summary[
  order(
    match(sensitivity_summary$scenario, scenarios$scenario),
    match(sensitivity_summary$material, c("wood", "concrete"))
  ),
]

rownames(sensitivity_summary) <- NULL

print(sensitivity_summary)


# ==============================================================================
# 6. Compare classification thresholds
# ==============================================================================

# Classification thresholds do not change cluster geometry.
# Test their effect separately using baseline cluster stocks.
#
# 0.75 = thresholds reduced by 25%
# 1.00 = original thresholds
# 1.25 = thresholds increased by 25%

threshold_factors <- c(0.75, 1.00, 1.25)

classification_summary <- do.call(
  rbind,
  lapply(c("wood", "concrete"), function(material) {
    
    x <- scenario_results[["baseline"]][[
      paste0(material, "_cluster_summary")
    ]]
    
    thresholds <- if (material == "wood") {
      wood_cluster_thresholds
    } else {
      concrete_cluster_thresholds
    }
    
    do.call(
      rbind,
      lapply(threshold_factors, function(factor) {
        
        classes <- cut(
          x$material_t,
          breaks = c(-Inf, thresholds * factor, Inf),
          labels = c(
            "below_threshold",
            "significant",
            "medium",
            "high"
          ),
          right = FALSE
        )
        
        data.frame(
          material = material,
          threshold_factor = factor,
          significant_count = sum(classes == "significant"),
          medium_count = sum(classes == "medium"),
          high_count = sum(classes == "high"),
          below_threshold_count = sum(classes == "below_threshold")
        )
      })
    )
  })
)

print(classification_summary)


# ==============================================================================
# 7. Optional map comparisons
# ==============================================================================

# Choose a small number of scenarios for visual inspection.
# Each scenario is displayed as a separate toggleable layer.

map_scenarios <- c(
  "baseline",
  "growth_085",
  "connectivity_4",
  "offset_50_50"
)

map_colours <- c(
  baseline = "#2C7FB8",
  growth_085 = "#D73027",
  connectivity_4 = "#31A354",
  offset_50_50 = "#756BB1"
)

make_cluster_map <- function(material) {
  
  map_layers <- lapply(map_scenarios, function(scenario) {
    
    result <- scenario_results[[scenario]]
    
    clusters <- result[[paste0(material, "_clusters")]]
    summary <- result[[paste0(material, "_cluster_summary")]]
    
    accepted_ids <- summary$cluster_id[
      summary$class != "below_threshold"
    ]
    
    accepted <- terra::ifel(
      clusters %in% accepted_ids,
      clusters,
      NA
    )
    
    polygons <- terra::as.polygons(
      accepted,
      dissolve = TRUE,
      na.rm = TRUE
    )
    
    mapview(
      polygons,
      col.regions = map_colours[[scenario]],
      alpha.regions = 0.35,
      legend = FALSE,
      layer.name = scenario,
      map.types = "CartoDB.Positron"
    )
  })
  
  Reduce(`+`, map_layers)
}

wood_sensitivity_map <- make_cluster_map("wood")
concrete_sensitivity_map <- make_cluster_map("concrete")

wood_sensitivity_map
concrete_sensitivity_map


# ==============================================================================
# 8. Export summary tables (optional)
# ==============================================================================

# Uncomment to save the results.
#
# write.csv(
#   sensitivity_summary,
#   "data/processed/cluster_sensitivity_summary.csv",
#   row.names = FALSE
# )
#
# write.csv(
#   classification_summary,
#   "data/processed/cluster_classification_sensitivity.csv",
#   row.names = FALSE
# )



# ==============================================================================
# 9. Cluster-level quality assessment
# ==============================================================================
#
# Compare selected parameter combinations using cluster-level metrics.
#
# Primary criteria:
# 1. Local spatial extent
# 2. Material density
#
# Supporting criteria:
# 3. Material stock
# 4. Spatial coherence and shape
# 5. Robustness
#
# No new clusters are calculated in this section.
# ==============================================================================


# ==============================================================================
# 9.1. Select scenarios
# ==============================================================================

quality_scenarios <- c(
  "growth_085",
  "grid_50m_focal_3_growth_085",
  "grid_50m_focal_5_growth_085",
  "grid_50m_focal_7_growth_085"
)


# ==============================================================================
# 9.2. Calculate quality metrics for individual clusters
# ==============================================================================

cluster_quality <- do.call(
  rbind,
  lapply(quality_scenarios, function(scenario) {
    
    s <- scenarios[
      scenarios$scenario == scenario,
      ,
      drop = FALSE
    ]
    
    result <- scenario_results[[scenario]]
    
    rbind(
      calculate_cluster_quality(
        result = result,
        scenario_row = s,
        material = "wood"
      ),
      calculate_cluster_quality(
        result = result,
        scenario_row = s,
        material = "concrete"
      )
    )
  })
)

rownames(cluster_quality) <- NULL

print(cluster_quality)


# ==============================================================================
# 9.3. Summarise cluster-level metrics by scenario
# ==============================================================================

quality_summary <- do.call(
  rbind,
  lapply(
    split(
      cluster_quality,
      list(
        cluster_quality$scenario,
        cluster_quality$material
      ),
      drop = TRUE
    ),
    function(x) {
      
      data.frame(
        scenario = x$scenario[1],
        material = x$material[1],
        
        cluster_count = nrow(x),
        
        median_area_ha = median(x$area_ha),
        max_area_ha = max(x$area_ha),
        
        median_density_t_ha = median(x$density_t_ha),
        mean_density_t_ha = mean(x$density_t_ha),
        
        median_extent_m = median(x$max_extent_m),
        max_extent_m = max(x$max_extent_m),
        
        median_compactness = median(x$compactness),
        
        total_material_t = sum(x$material_t)
      )
    }
  )
)

rownames(quality_summary) <- NULL

quality_summary <- quality_summary[
  order(
    match(quality_summary$scenario, quality_scenarios),
    match(quality_summary$material, c("wood", "concrete"))
  ),
]

print(quality_summary)


# ==============================================================================
# 9.4. Inspect the largest clusters
# ==============================================================================

# Identify the five largest clusters by area for each scenario
# and material.

largest_clusters <- do.call(
  rbind,
  lapply(
    split(
      cluster_quality,
      list(
        cluster_quality$scenario,
        cluster_quality$material
      ),
      drop = TRUE
    ),
    function(x) {
      
      x <- x[
        order(x$area_ha, decreasing = TRUE),
        ,
        drop = FALSE
      ]
      
      head(x, 5)
    }
  )
)

rownames(largest_clusters) <- NULL

largest_clusters <- largest_clusters[
  order(
    match(largest_clusters$scenario, quality_scenarios),
    match(largest_clusters$material, c("wood", "concrete")),
    -largest_clusters$area_ha
  ),
]

print(largest_clusters)


# ==============================================================================
# 9.5. Optional exports
# ==============================================================================

# Uncomment to save the results.

# write.csv(
#   cluster_quality,
#   "data/processed/cluster_quality.csv",
#   row.names = FALSE
# )

# write.csv(
#   quality_summary,
#   "data/processed/cluster_quality_summary.csv",
#   row.names = FALSE
# )

# write.csv(
#   largest_clusters,
#   "data/processed/largest_clusters.csv",
#   row.names = FALSE
# )



# ==============================================================================
# 10. Interactive comparison of cluster geometries
# ==============================================================================

library(mapview)

map_scenarios <- c(
  "growth_085",
  "grid_50m_focal_3_growth_085",
  "grid_50m_focal_5_growth_085",
  "grid_50m_focal_7_growth_085"
)

map_colours <- c(
  growth_085 = "#636363",
  grid_50m_focal_3_growth_085 = "#1B9E77",
  grid_50m_focal_5_growth_085 = "#D95F02",
  grid_50m_focal_7_growth_085 = "#7570B3"
)

make_quality_map <- function(material) {
  
  layers <- lapply(map_scenarios, function(scenario) {
    
    result <- scenario_results[[scenario]]
    
    clusters <- result[[paste0(material, "_clusters")]]
    summary <- result[[paste0(material, "_cluster_summary")]]
    
    accepted_ids <- summary$cluster_id[
      !is.na(summary$class) &
        summary$class != "below_threshold"
    ]
    
    accepted <- terra::ifel(
      clusters %in% accepted_ids,
      clusters,
      NA
    )
    
    polygons <- terra::as.polygons(
      accepted,
      dissolve = TRUE,
      na.rm = TRUE
    )
    
    names(polygons)[1] <- "cluster_id"
    
    # Add cluster-level quality metrics to the map.
    metrics <- cluster_quality[
      cluster_quality$scenario == scenario &
        cluster_quality$material == material,
      ,
      drop = FALSE
    ]
    
    idx <- match(polygons$cluster_id, metrics$cluster_id)
    
    polygons$area_ha <- round(metrics$area_ha[idx], 1)
    polygons$material_t <- round(metrics$material_t[idx], 0)
    polygons$density_t_ha <- round(metrics$density_t_ha[idx], 1)
    polygons$max_extent_m <- round(metrics$max_extent_m[idx], 0)
    polygons$compactness <- round(metrics$compactness[idx], 3)
    
    mapview(
      polygons,
      zcol = NULL,
      col.regions = map_colours[[scenario]],
      alpha.regions = 0.45,
      legend = FALSE,
      layer.name = scenario,
      map.types = "CartoDB.Positron"
    )
  })
  
  Reduce(`+`, layers)
}

wood_quality_map <- make_quality_map("wood")
concrete_quality_map <- make_quality_map("concrete")

wood_quality_map
