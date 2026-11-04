# ==============================================================================
# 05_cluster_accessibility.R
#
# Calculate material-weighted accessibility for accepted wood and concrete
# stock clusters using building-level network distances from script 04.
# Buildings are assigned to clusters via the original cluster ID rasters (02),
# not by polygon intersections. Only accepted cluster IDs are summarized.
#
# Inputs:
#   data/processed/buildings_accessibility.gpkg
#   data/processed/wood_clusters.gpkg
#   data/processed/concrete_clusters.gpkg
#   data/processed/wood_cluster_ids.tif
#   data/processed/concrete_cluster_ids.tif
#
# Outputs:
#   data/processed/wood_clusters_accessibility.gpkg
#   data/processed/concrete_clusters_accessibility.gpkg
#   Cluster-level material-weighted road distances (km) and tonne-kilometres.
#
# Run from the R/ directory. Interactive maps are optional.
# ==============================================================================

library(sf)
library(terra)
library(dplyr)

show_maps <- FALSE

# ==============================================================================
# 1. Read and validate input data
# ==============================================================================

buildings <- st_read("data/processed/buildings_accessibility.gpkg")
wood_clusters <- st_read("data/processed/wood_clusters.gpkg")
concrete_clusters <- st_read("data/processed/concrete_clusters.gpkg")

wood_raster <- rast("data/processed/wood_cluster_ids.tif")
concrete_raster <- rast("data/processed/concrete_cluster_ids.tif")

stopifnot(
  nrow(buildings) > 0,
  nrow(wood_clusters) > 0,
  nrow(concrete_clusters) > 0,
  all(c("material_wood_t", "material_concrete_t",
        "dist_wood_km", "dist_conc_km") %in% names(buildings)),
  all(c("cluster_id", "class", "material_t") %in% names(wood_clusters)),
  all(c("cluster_id", "class", "material_t") %in% names(concrete_clusters)),
  !anyNA(wood_clusters$cluster_id),
  !anyNA(concrete_clusters$cluster_id),
  !anyDuplicated(wood_clusters$cluster_id),
  !anyDuplicated(concrete_clusters$cluster_id)
)

buildings <- st_transform(buildings, 3067)
wood_clusters <- st_transform(wood_clusters, 3067)
concrete_clusters <- st_transform(concrete_clusters, 3067)

# ==============================================================================
# 2. Assign buildings to the original raster cluster IDs
# ==============================================================================

# Use the same point representation as the clustering function in 02.
# Specify terra::extract because other loaded packages may mask extract().
building_points <- centroids(vect(buildings), inside = TRUE)

buildings$wood_cluster_id <- terra::extract(
  wood_raster, building_points, ID = FALSE
)[[1]]

buildings$concrete_cluster_id <- terra::extract(
  concrete_raster, building_points, ID = FALSE
)[[1]]

# Raster IDs also include clusters below the minimum material threshold;
# these are excluded from the summaries by matching accepted polygon IDs.

# ==============================================================================
# 3. Calculate cluster-level accessibility
# ==============================================================================

calculate_cluster_accessibility <- function(
    buildings, clusters, cluster_col, material_col, distance_col
) {
  # Keep buildings belonging to accepted clusters.
  matched <- buildings |>
    st_drop_geometry() |>
    mutate(cluster_id = .data[[cluster_col]]) |>
    filter(cluster_id %in% clusters$cluster_id)
  
  # Missing material values contribute zero tonnes. Only positive material
  # with a finite route is included in the weighted distance calculation.
  summary <- matched |>
    mutate(
      material = coalesce(.data[[material_col]], 0),
      distance = .data[[distance_col]],
      routed = material > 0 & is.finite(distance)
    ) |>
    group_by(cluster_id) |>
    summarise(
      building_count = n(),
      matched_material_t = sum(material),
      routed_t = sum(material[routed]),
      unrouted_t = sum(material[!routed]),
      tonne_km = sum(material[routed] * distance[routed]),
      .groups = "drop"
    ) |>
    mutate(
      mean_dist_km = if_else(
        routed_t > 0,
        tonne_km / routed_t,
        NA_real_
      ),
      routed_share_pct = if_else(
        matched_material_t > 0,
        100 * routed_t / matched_material_t,
        NA_real_
      )
    )
  
  # Preserve the accepted cluster geometries and existing output columns.
  output <- clusters |>
    left_join(summary, by = "cluster_id")
  
  # Verify that raster-based building allocation reproduces the material
  # amounts in script 02 (tolerance: 0.01 tonne per cluster).
  differences <- abs(output$material_t - output$matched_material_t)
  
  if (anyNA(differences) || any(differences >= 0.01)) {
    print(
      output |>
        st_drop_geometry() |>
        select(cluster_id, material_t, matched_material_t)
    )
    stop("Cluster material totals do not match the original raster results.")
  }
  
  output
}

# ==============================================================================
# 4. Calculate wood and concrete accessibility
# ==============================================================================

wood_accessibility <- calculate_cluster_accessibility(
  buildings, wood_clusters, "wood_cluster_id", "material_wood_t", "dist_wood_km"
)

concrete_accessibility <- calculate_cluster_accessibility(
  buildings, concrete_clusters, "concrete_cluster_id",
  "material_concrete_t", "dist_conc_km"
)

# ==============================================================================
# 5. Report results
# ==============================================================================

wood_summary <- wood_accessibility |>
  st_drop_geometry() |>
  select(cluster_id, class, material_t, building_count,
         mean_dist_km, tonne_km, unrouted_t, routed_share_pct) |>
  arrange(desc(tonne_km))

concrete_summary <- concrete_accessibility |>
  st_drop_geometry() |>
  select(cluster_id, class, material_t, building_count,
         mean_dist_km, tonne_km, unrouted_t, routed_share_pct) |>
  arrange(desc(tonne_km))

cat("\nCluster accessibility overview:\n")
print(data.frame(
  material = c("wood", "concrete"),
  accepted_clusters = c(nrow(wood_accessibility), nrow(concrete_accessibility)),
  buildings_in_clusters = c(sum(wood_accessibility$building_count),
                            sum(concrete_accessibility$building_count)),
  matched_material_t = c(sum(wood_accessibility$matched_material_t),
                         sum(concrete_accessibility$matched_material_t)),
  unrouted_material_t = c(sum(wood_accessibility$unrouted_t),
                          sum(concrete_accessibility$unrouted_t))
), row.names = FALSE)

cat("\nWood cluster accessibility (highest tonne-km first):\n")
print(wood_summary)
cat("\nConcrete cluster accessibility (highest tonne-km first):\n")
print(concrete_summary)

# ==============================================================================
# 6. Save cluster-level results
# ==============================================================================

st_write(
  wood_accessibility,
  "data/processed/wood_clusters_accessibility.gpkg",
  delete_dsn = TRUE
)

st_write(
  concrete_accessibility,
  "data/processed/concrete_clusters_accessibility.gpkg",
  delete_dsn = TRUE
)

cat("\nCluster accessibility outputs saved to data/processed/.\n")

# ==============================================================================
# 7. Optional interactive maps
# ==============================================================================

if (show_maps) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("Install the mapview package to display interactive maps.")
  }
  library(mapview)
  
  print(mapview(
    wood_accessibility,
    zcol = "mean_dist_km",
    layer.name = "Wood cluster accessibility"
  ))
  
  print(mapview(
    concrete_accessibility,
    zcol = "mean_dist_km",
    layer.name = "Concrete cluster accessibility"
  ))
}

