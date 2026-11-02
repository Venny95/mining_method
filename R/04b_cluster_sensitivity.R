# ==============================================================================
# 04b_cluster_sensitivity.R
#
# PURPOSE
# Test how grid size, focal window, quantiles, connectivity and grid origin
# affect the identification of wood and concrete material clusters.
#
# OUTPUTS (in R memory only; no files are saved):
#   scenarios           - all 18 parameter combinations
#   scenario_results    - full clustering results for each scenario
#   sensitivity_summary - counts, stocks, areas, density and Jaccard overlap
#   cluster_quality     - metrics for individual accepted clusters
#   quality_summary     - scenario-level summaries of cluster quality
#   largest_clusters    - five largest clusters by area in each selected case
#   classification_summary - sensitivity of stock-based class labels
#   wood_quality_map / concrete_quality_map - interactive comparison maps
#
# Cluster quality is assessed using three main criteria:
# 1. Locality: a geographically bounded concentration
# 2. Sufficient material stock: a meaningful material reserve
# 3. Material density: material tonnes relative to cluster area
#
# Run from the R/ directory. The original clustering algorithm is unchanged.
# ==============================================================================

# 1. Libraries, data and material thresholds -----------------------------------

library(terra)
library(sf)
library(mapview)

source("functions/cluster_functions.R")
source("functions/cluster_metrics.R")

buil <- terra::vect("data/processed/buildings_material_stock_joensuu.gpkg")

wood_cluster_thresholds <- c(significant = 500, medium = 2000, high = 5000)
concrete_cluster_thresholds <- c(significant = 5000, medium = 20000, high = 50000)


# 2. Sensitivity scenarios ------------------------------------------------------
# Each row is one test. The original baseline is 100 m / 3x3 / growth 0.80.
# NOTE: 04_grid_clusters.R currently uses growth 0.85 instead.
# Grid resolution tests also change the physical width of the focal window.

scenarios <- data.frame(
  scenario = c(
    "baseline", "grid_50m", "grid_150m", "focal_5", "focal_7",
    "seed_085", "seed_095", "growth_075", "growth_085",
    "connectivity_4", "offset_50_0", "offset_0_50", "offset_50_50",
    "grid_50m_focal_5_growth_080", "grid_50m_focal_7_growth_080",
    "grid_50m_focal_3_growth_085", "grid_50m_focal_5_growth_085",
    "grid_50m_focal_7_growth_085"
  ),
  test_type = c(
    "baseline", rep("grid resolution", 2), rep("focal neighbourhood", 2),
    rep("seed quantile", 2), rep("growth quantile", 2), "connectivity",
    rep("grid origin", 3), rep("grid-focal combination", 5)
  ),
  grid_size_m = c(
    100, 50, 150, 100, 100, 100, 100, 100, 100, 100,
    100, 100, 100, 50, 50, 50, 50, 50
  ),
  focal_window_cells = c(
    3, 3, 3, 5, 7, 3, 3, 3, 3, 3, 3, 3, 3, 5, 7, 3, 5, 7
  ),
  seed_quantile = c(
    0.90, 0.90, 0.90, 0.90, 0.90, 0.85, 0.95, 0.90, 0.90,
    0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90, 0.90
  ),
  growth_quantile = c(
    0.80, 0.80, 0.80, 0.80, 0.80, 0.80, 0.80, 0.75, 0.85,
    0.80, 0.80, 0.80, 0.80, 0.80, 0.80, 0.85, 0.85, 0.85
  ),
  patch_directions = c(rep(8, 9), 4, rep(8, 8)),
  offset_x_m = c(rep(0, 10), 50, 0, 50, rep(0, 5)),
  offset_y_m = c(rep(0, 10), 0, 50, 50, rep(0, 5))
)

scenarios$focal_width_m <- scenarios$grid_size_m * scenarios$focal_window_cells
stopifnot(nrow(scenarios) == 18, !anyDuplicated(scenarios$scenario))


# 3. Run clustering scenarios ---------------------------------------------------
# Full raster results stay in memory so that subsequent sections can compare
# footprints and display maps without running clustering again.

scenario_results <- list()

for (i in seq_len(nrow(scenarios))) {
  s <- scenarios[i, ]
  message("Running ", s$scenario, " (", i, "/", nrow(scenarios), ")")
  
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


# 4. Scenario summary: stock, area and density ----------------------------------
# Only accepted clusters (class != "below_threshold") contribute to the
# reported area, stock and density. Material stock comes from original grid
# values, NOT from overlapping focal sums.

accepted_ids <- function(result, material) {
  x <- result[[paste0(material, "_cluster_summary")]]
  x$cluster_id[!is.na(x$class) & x$class != "below_threshold"]
}

make_accepted_mask <- function(result, material) {
  x <- result[[paste0(material, "_clusters")]]
  terra::ifel(!is.na(x) & x %in% accepted_ids(result, material), 1, 0)
}

summarise_scenario <- function(result, s, material) {
  x <- result[[paste0(material, "_cluster_summary")]]
  ids <- accepted_ids(result, material)
  accepted <- x[x$cluster_id %in% ids, , drop = FALSE]
  
  counts <- terra::freq(result[[paste0(material, "_clusters")]])
  counts <- counts[counts$value %in% ids, , drop = FALSE]
  area_ha <- sum(counts$count) * s$grid_size_m^2 / 10000
  stock_t <- sum(accepted$material_t)
  
  city_stock_t <- terra::global(
    result$grid_materials[[paste0("material_", material, "_t_sum")]],
    "sum", na.rm = TRUE
  )[1, 1]
  
  data.frame(
    scenario = s$scenario, test_type = s$test_type, material = material,
    grid_size_m = s$grid_size_m, focal_width_m = s$focal_width_m,
    cluster_count = nrow(x), accepted_cluster_count = nrow(accepted),
    accepted_area_ha = area_ha, accepted_material_t = stock_t,
    material_share_pct = if (city_stock_t > 0) 100 * stock_t / city_stock_t else NA_real_,
    density_t_ha = if (area_ha > 0) stock_t / area_ha else NA_real_,
    largest_cluster_area_ha = if (nrow(counts)) max(counts$count) * s$grid_size_m^2 / 10000 else NA_real_,
    largest_cluster_material_t = if (nrow(accepted)) max(accepted$material_t) else NA_real_
  )
}

sensitivity_summary <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  result <- scenario_results[[s$scenario]]
  rbind(summarise_scenario(result, s, "wood"),
        summarise_scenario(result, s, "concrete"))
}))
rownames(sensitivity_summary) <- NULL


# 5. Spatial overlap: Jaccard index ---------------------------------------------
# Jaccard = area of intersection / area of union of accepted cluster footprints.
# 1 means identical coverage; 0 means no overlap.
# This does NOT compare cluster IDs or whether individual clusters split.
# Resampling to 25 m uses nearest neighbours, so edges are approximate.

extents <- lapply(scenario_results, function(x) terra::ext(x$grid_materials))
comparison_grid <- terra::rast(
  terra::ext(
    min(vapply(extents, terra::xmin, numeric(1))),
    max(vapply(extents, terra::xmax, numeric(1))),
    min(vapply(extents, terra::ymin, numeric(1))),
    max(vapply(extents, terra::ymax, numeric(1)))
  ),
  resolution = 25,
  crs = terra::crs(scenario_results[["baseline"]]$grid_materials)
)

align_mask <- function(result, material) {
  x <- terra::resample(make_accepted_mask(result, material),
                       comparison_grid, method = "near")
  terra::ifel(is.na(x), 0, x)
}

spatial_overlap <- do.call(rbind, lapply(c("wood", "concrete"), function(material) {
  baseline_mask <- align_mask(scenario_results[["baseline"]], material)
  
  do.call(rbind, lapply(names(scenario_results), function(scenario) {
    other_mask <- align_mask(scenario_results[[scenario]], material)
    intersection <- terra::global((baseline_mask == 1) & (other_mask == 1),
                                  "sum", na.rm = TRUE)[1, 1]
    union <- terra::global((baseline_mask == 1) | (other_mask == 1),
                           "sum", na.rm = TRUE)[1, 1]
    
    data.frame(scenario = scenario, material = material,
               jaccard = if (union > 0) intersection / union else NA_real_)
  }))
}))

sensitivity_summary$jaccard <- spatial_overlap$jaccard[
  match(paste(sensitivity_summary$scenario, sensitivity_summary$material),
        paste(spatial_overlap$scenario, spatial_overlap$material))
]
print(sensitivity_summary)


# 6. Cluster quality: locality, stock and density -------------------------------
# Detailed metrics are calculated for four selected scenarios, not all 18.
# Locality: area, maximum extent and compactness (shape).
# Material stock: actual tonnes per accepted cluster.
# Density: actual tonnes / cluster area (t/ha).
# Shape metrics support interpretation; they do not replace map inspection.

quality_scenarios <- c(
  "growth_085",
  "grid_50m_focal_3_growth_085",
  "grid_50m_focal_5_growth_085",
  "grid_50m_focal_7_growth_085"
)

cluster_quality <- do.call(rbind, lapply(quality_scenarios, function(scenario) {
  s <- scenarios[scenarios$scenario == scenario, , drop = FALSE]
  result <- scenario_results[[scenario]]
  rbind(calculate_cluster_quality(result, s, "wood"),
        calculate_cluster_quality(result, s, "concrete"))
}))
rownames(cluster_quality) <- NULL

groups <- split(cluster_quality,
                list(cluster_quality$scenario, cluster_quality$material),
                drop = TRUE)

quality_summary <- do.call(rbind, lapply(groups, function(x) {
  data.frame(
    scenario = x$scenario[1], material = x$material[1],
    cluster_count = nrow(x),
    median_area_ha = median(x$area_ha), max_area_ha = max(x$area_ha),
    median_density_t_ha = median(x$density_t_ha),
    mean_density_t_ha = mean(x$density_t_ha),
    median_extent_m = median(x$max_extent_m),
    max_extent_m = max(x$max_extent_m),
    median_compactness = median(x$compactness),
    total_material_t = sum(x$material_t)
  )
}))

largest_clusters <- do.call(rbind, lapply(groups, function(x) {
  head(x[order(x$area_ha, decreasing = TRUE), , drop = FALSE], 5)
}))

order_quality <- function(x) {
  x[order(match(x$scenario, quality_scenarios),
          match(x$material, c("wood", "concrete"))), , drop = FALSE]
}
quality_summary <- order_quality(quality_summary)
largest_clusters <- order_quality(largest_clusters)
rownames(quality_summary) <- NULL
rownames(largest_clusters) <- NULL

print(quality_summary)
print(largest_clusters)


# 7. Classification threshold sensitivity --------------------------------------
# Change the stock thresholds by +/-25% without changing cluster geometry.
# This tests the number of clusters in each material class, not their shapes.

threshold_factors <- c(0.75, 1, 1.25)

classification_summary <- do.call(rbind, lapply(c("wood", "concrete"), function(material) {
  x <- scenario_results[["baseline"]][[paste0(material, "_cluster_summary")]]
  thresholds <- if (material == "wood") wood_cluster_thresholds else concrete_cluster_thresholds
  
  do.call(rbind, lapply(threshold_factors, function(factor) {
    classes <- cut(
      x$material_t,
      breaks = c(-Inf, thresholds * factor, Inf),
      labels = c("below_threshold", "significant", "medium", "high"),
      right = FALSE
    )
    data.frame(
      material = material, threshold_factor = factor,
      significant_count = sum(classes == "significant"),
      medium_count = sum(classes == "medium"),
      high_count = sum(classes == "high"),
      below_threshold_count = sum(classes == "below_threshold")
    )
  }))
}))
print(classification_summary)


# 8. Interactive maps -----------------------------------------------------------
# Toggle scenario layers on/off to compare accepted cluster footprints.
# Click a polygon to inspect its area, stock, density, extent and compactness.
# Change map_scenarios to compare other scenarios; detailed pop-up metrics
# are available for scenarios included in quality_scenarios.

map_scenarios <- quality_scenarios
map_colours <- c("#636363", "#1B9E77", "#D95F02", "#7570B3")
names(map_colours) <- map_scenarios

make_quality_map <- function(material) {
  layers <- lapply(map_scenarios, function(scenario) {
    result <- scenario_results[[scenario]]
    clusters <- result[[paste0(material, "_clusters")]]
    ids <- accepted_ids(result, material)
    
    accepted <- terra::ifel(clusters %in% ids, clusters, NA)
    polygons <- terra::as.polygons(accepted, dissolve = TRUE, na.rm = TRUE)
    names(polygons)[1] <- "cluster_id"
    
    metrics <- cluster_quality[
      cluster_quality$scenario == scenario &
        cluster_quality$material == material, , drop = FALSE
    ]
    idx <- match(polygons$cluster_id, metrics$cluster_id)
    polygons$area_ha <- round(metrics$area_ha[idx], 1)
    polygons$material_t <- round(metrics$material_t[idx], 0)
    polygons$density_t_ha <- round(metrics$density_t_ha[idx], 1)
    polygons$max_extent_m <- round(metrics$max_extent_m[idx], 0)
    polygons$compactness <- round(metrics$compactness[idx], 3)
    
    mapview(
      polygons, zcol = NULL, col.regions = map_colours[[scenario]],
      alpha.regions = 0.45, legend = FALSE,
      layer.name = scenario, map.types = "CartoDB.Positron"
    )
  })
  Reduce(`+`, layers)
}

wood_quality_map <- make_quality_map("wood")
concrete_quality_map <- make_quality_map("concrete")

wood_quality_map
concrete_quality_map
