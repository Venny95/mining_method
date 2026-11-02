
# ==============================================================================
# cluster_metrics.R
#
# Calculate cluster-level quality metrics for material stock clusters.
#
# Metrics:
# - Area (ha)
# - Material stock (tonnes)
# - Material density (tonnes/ha)
# - Number of raster cells
# - Maximum spatial extent (m)
# - Shape compactness (4 * pi * area / perimeter^2)
#
# The function uses the original cluster raster and material summaries.
# Focal material sums are not used to calculate material density.
# ==============================================================================


calculate_cluster_quality <- function(result, scenario_row, material) {
  
  stopifnot(material %in% c("wood", "concrete"))
  
  clusters <- result[[paste0(material, "_clusters")]]
  
  cluster_summary <- result[[
    paste0(material, "_cluster_summary")
  ]]
  
  # Retain clusters that meet the minimum material stock threshold.
  accepted_summary <- cluster_summary[
    !is.na(cluster_summary$class) &
      cluster_summary$class != "below_threshold",
    ,
    drop = FALSE
  ]
  
  if (nrow(accepted_summary) == 0) {
    return(data.frame())
  }
  
  accepted_ids <- accepted_summary$cluster_id
  
  # Count raster cells belonging to each cluster.
  cell_frequency <- terra::freq(clusters)
  
  cell_count <- cell_frequency$count[
    match(accepted_ids, cell_frequency$value)
  ]
  
  # Calculate cluster area from the original raster resolution.
  cell_area_ha <- prod(terra::res(clusters)) / 10000
  
  area_ha <- cell_count * cell_area_ha
  
  # Calculate density from original material stocks.
  material_t <- accepted_summary$material_t
  
  density_t_ha <- material_t / area_ha
  
  # Keep accepted cluster IDs in the raster.
  accepted_raster <- terra::ifel(
    clusters %in% accepted_ids,
    clusters,
    NA
  )
  
  # Convert raster clusters to polygons.
  cluster_polygons <- terra::as.polygons(
    accepted_raster,
    dissolve = TRUE,
    na.rm = TRUE
  )
  
  names(cluster_polygons)[1] <- "cluster_id"
  
  # Convert polygons to sf geometries.
  cluster_sf <- sf::st_as_sf(cluster_polygons)
  
  # Match geometry rows to accepted cluster IDs.
  cluster_sf <- cluster_sf[
    match(accepted_ids, cluster_sf$cluster_id),
    ,
    drop = FALSE
  ]
  
  # Calculate area and perimeter in projected metres.
  geometry_area_m2 <- as.numeric(
    sf::st_area(cluster_sf)
  )
  
  perimeter_m <- as.numeric(
    sf::st_length(
      sf::st_boundary(cluster_sf)
    )
  )
  
  # Shape compactness:
  # 1 represents a perfect circle.
  # Lower values indicate more irregular or elongated shapes.
  compactness <- ifelse(
    perimeter_m > 0,
    4 * pi * geometry_area_m2 / perimeter_m^2,
    NA_real_
  )
  
  # Calculate the maximum distance between polygon vertices.
  # This represents the maximum spatial extent of each cluster.
  max_extent_m <- vapply(
    sf::st_geometry(cluster_sf),
    function(geom) {
      
      vertices <- sf::st_coordinates(geom)
      
      xy <- unique(
        vertices[, c("X", "Y"), drop = FALSE]
      )
      
      if (nrow(xy) < 2) {
        return(0)
      }
      
      # The convex hull contains the most distant vertices.
      hull <- grDevices::chull(xy)
      
      hull_xy <- xy[hull, , drop = FALSE]
      
      max(stats::dist(hull_xy))
    },
    numeric(1)
  )
  
  data.frame(
    scenario = scenario_row$scenario,
    material = material,
    cluster_id = accepted_ids,
    class = as.character(accepted_summary$class),
    grid_size_m = scenario_row$grid_size_m,
    focal_width_m = scenario_row$focal_width_m,
    growth_quantile = scenario_row$growth_quantile,
    cell_count = cell_count,
    area_ha = area_ha,
    material_t = material_t,
    density_t_ha = density_t_ha,
    perimeter_m = perimeter_m,
    max_extent_m = max_extent_m,
    compactness = compactness
  )
}
