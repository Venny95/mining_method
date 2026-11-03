
# ==============================================================================
# 05_cluster_accessibility.R
#
# Calculate material-weighted accessibility for accepted material clusters.
# Building-to-cluster assignment uses the original clustering rasters.
#
# Outputs:
#   data/processed/wood_clusters_accessibility.gpkg
#   data/processed/concrete_clusters_accessibility.gpkg
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(mapview)


# ==============================================================================
# 1. Read input data
# ==============================================================================

buildings <- st_read(
  "data/processed/buildings_accessibility.gpkg"
)

wood_clusters <- st_read(
  "data/processed/wood_clusters.gpkg"
)

concrete_clusters <- st_read(
  "data/processed/concrete_clusters.gpkg"
)

wood_raster <- rast("data/processed/wood_cluster_ids.tif")
concrete_raster <- rast("data/processed/concrete_cluster_ids.tif")

buildings <- st_transform(buildings, 3067)
wood_clusters <- st_transform(wood_clusters, 3067)
concrete_clusters <- st_transform(concrete_clusters, 3067)


# ==============================================================================
# 2. Assign buildings to raster clusters
# ==============================================================================

# Use the same point representation as in the original clustering function
building_points <- terra::centroids(
  terra::vect(buildings),
  inside = TRUE
)

# Extract cluster IDs from the original raster cells
buildings$wood_cluster_id <- terra::extract(
  wood_raster, building_points, ID = FALSE
)[[1]]

buildings$concrete_cluster_id <- terra::extract(
  concrete_raster, building_points, ID = FALSE
)[[1]]


# ==============================================================================
# 3. Calculate cluster-level accessibility
# ==============================================================================

calculate_cluster_accessibility <- function(
    buildings, clusters, cluster_col, material_col, distance_col
) {
  
  # Keep buildings belonging to accepted clusters
  matched <- buildings |>
    st_drop_geometry() |>
    mutate(cluster_id = .data[[cluster_col]]) |>
    filter(cluster_id %in% clusters$cluster_id)
  
  # Calculate material-weighted accessibility indicators
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
  
  # Attach indicators to original cluster geometries
  output <- clusters |>
    left_join(summary, by = "cluster_id")
  
  # Verify that material totals match the original clustering results
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
  buildings,
  wood_clusters,
  "wood_cluster_id",
  "material_wood_t",
  "dist_wood_km"
)

concrete_accessibility <- calculate_cluster_accessibility(
  buildings,
  concrete_clusters,
  "concrete_cluster_id",
  "material_concrete_t",
  "dist_conc_km"
)


# ==============================================================================
# 5. Review results
# ==============================================================================

wood_summary <- wood_accessibility |>
  st_drop_geometry() |>
  select(
    cluster_id, class, material_t, building_count,
    mean_dist_km, tonne_km, unrouted_t, routed_share_pct
  ) |>
  arrange(desc(tonne_km))

concrete_summary <- concrete_accessibility |>
  st_drop_geometry() |>
  select(
    cluster_id, class, material_t, building_count,
    mean_dist_km, tonne_km, unrouted_t, routed_share_pct
  ) |>
  arrange(desc(tonne_km))

print(wood_summary)
print(concrete_summary)

mapview(
  wood_accessibility,
  zcol = "mean_dist_km",
  layer.name = "Wood cluster accessibility"
)

mapview(
  concrete_accessibility,
  zcol = "mean_dist_km",
  layer.name = "Concrete cluster accessibility"
)


# ==============================================================================
# 6. Save results
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

cat("Saved", nrow(wood_accessibility), "wood clusters\n")
cat("Saved", nrow(concrete_accessibility), "concrete clusters\n")

