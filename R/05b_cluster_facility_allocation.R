
# ==============================================================================
# 05b_cluster_facility_allocation.R
#
# Analyse how material stocks within accepted clusters are allocated to
# different receiving facilities based on shortest road-network distances.
#
# Outputs:
#   data/processed/wood_facility_allocation.csv
#   data/processed/concrete_facility_allocation.csv
#   data/processed/wood_cluster_facility_summary.csv
#   data/processed/concrete_cluster_facility_summary.csv
# ==============================================================================

library(sf)
library(terra)
library(dplyr)
library(ggplot2)


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


# ==============================================================================
# 2. Assign buildings to accepted clusters
# ==============================================================================

building_points <- terra::centroids(
  terra::vect(buildings),
  inside = TRUE
)

buildings$wood_cluster_id <- terra::extract(
  wood_raster, building_points, ID = FALSE
)[[1]]

buildings$concrete_cluster_id <- terra::extract(
  concrete_raster, building_points, ID = FALSE
)[[1]]


# ==============================================================================
# 3. Calculate facility allocation within clusters
# ==============================================================================

calculate_facility_allocation <- function(
    buildings, clusters, cluster_col, material_col,
    distance_col, facility_col
) {
  
  matched <- buildings |>
    st_drop_geometry() |>
    mutate(
      cluster_id = .data[[cluster_col]],
      material = coalesce(.data[[material_col]], 0),
      distance = .data[[distance_col]],
      facility = .data[[facility_col]]
    ) |>
    filter(cluster_id %in% clusters$cluster_id)
  
  # Check that cluster material totals match the original results
  material_check <- matched |>
    group_by(cluster_id) |>
    summarise(
      matched_material_t = sum(material),
      .groups = "drop"
    )
  
  differences <- clusters |>
    st_drop_geometry() |>
    select(cluster_id, material_t) |>
    left_join(material_check, by = "cluster_id") |>
    mutate(
      difference_t = abs(material_t - matched_material_t)
    )
  
  stopifnot(
    !anyNA(differences$difference_t),
    all(differences$difference_t < 0.01)
  )
  
  # Include only material with a valid facility and route
  allocated <- matched |>
    filter(
      material > 0,
      is.finite(distance),
      !is.na(facility),
      facility != ""
    )
  
  # Summarise material flows by cluster and receiving facility
  allocation <- allocated |>
    group_by(cluster_id, facility) |>
    summarise(
      building_count = n(),
      material_t = sum(material),
      tonne_km = sum(material * distance),
      .groups = "drop"
    ) |>
    group_by(cluster_id) |>
    mutate(
      material_share_pct = 100 * material_t / sum(material_t),
      mean_dist_km = tonne_km / material_t
    ) |>
    ungroup() |>
    arrange(cluster_id, desc(material_t))
  
  # Summarise the distribution of material across facilities
  cluster_summary <- allocation |>
    group_by(cluster_id) |>
    summarise(
      facility_count = n(),
      dominant_facility_share_pct = max(material_share_pct),
      .groups = "drop"
    ) |>
    right_join(
      clusters |>
        st_drop_geometry() |>
        select(cluster_id, material_t, class),
      by = "cluster_id"
    ) |>
    arrange(desc(material_t))
  
  list(
    allocation = allocation,
    cluster_summary = cluster_summary
  )
}


# ==============================================================================
# 4. Calculate wood and concrete allocations
# ==============================================================================

wood_results <- calculate_facility_allocation(
  buildings,
  wood_clusters,
  "wood_cluster_id",
  "material_wood_t",
  "dist_wood_km",
  "company_wood"
)

concrete_results <- calculate_facility_allocation(
  buildings,
  concrete_clusters,
  "concrete_cluster_id",
  "material_concrete_t",
  "dist_conc_km",
  "company_conc"
)

wood_allocation <- wood_results$allocation
concrete_allocation <- concrete_results$allocation

wood_facility_summary <- wood_results$cluster_summary
concrete_facility_summary <- concrete_results$cluster_summary


# ==============================================================================
# 5. Review allocation results
# ==============================================================================

print(wood_facility_summary)
print(concrete_facility_summary)

# Inspect facility allocations for the largest clusters
print(
  wood_allocation |>
    filter(cluster_id == 27)
)

print(
  concrete_allocation |>
    filter(cluster_id == 39)
)


# ==============================================================================
# 6. Visualise facility allocation
# ==============================================================================

# Show the five largest clusters by material stock
wood_top5 <- wood_facility_summary |>
  slice_max(material_t, n = 5) |>
  pull(cluster_id)

concrete_top5 <- concrete_facility_summary |>
  slice_max(material_t, n = 5) |>
  pull(cluster_id)

ggplot(
  wood_allocation |> filter(cluster_id %in% wood_top5),
  aes(
    x = factor(cluster_id),
    y = material_share_pct,
    fill = facility
  )
) +
  geom_col() +
  labs(
    title = "Wood allocation by receiving facility",
    x = "Cluster ID",
    y = "Share of routed material (%)",
    fill = "Facility"
  ) +
  theme_minimal()

ggplot(
  concrete_allocation |> filter(cluster_id %in% concrete_top5),
  aes(
    x = factor(cluster_id),
    y = material_share_pct,
    fill = facility
  )
) +
  geom_col() +
  labs(
    title = "Concrete allocation by receiving facility",
    x = "Cluster ID",
    y = "Share of routed material (%)",
    fill = "Facility"
  ) +
  theme_minimal()


# ==============================================================================
# 7. Save results
# ==============================================================================

write.csv(
  wood_allocation,
  "data/processed/wood_facility_allocation.csv",
  row.names = FALSE
)

write.csv(
  concrete_allocation,
  "data/processed/concrete_facility_allocation.csv",
  row.names = FALSE
)

write.csv(
  wood_facility_summary,
  "data/processed/wood_cluster_facility_summary.csv",
  row.names = FALSE
)

write.csv(
  concrete_facility_summary,
  "data/processed/concrete_cluster_facility_summary.csv",
  row.names = FALSE
)

cat("Facility allocation analysis completed.\n")


# ==============================================================================
# 8. Map facility allocation within the largest wood cluster
# ==============================================================================

# Select the largest wood cluster
cluster_id_to_map <- 27

cluster_map <- wood_clusters |>
  filter(cluster_id == cluster_id_to_map)

# Select buildings assigned to this cluster
buildings_map <- buildings |>
  filter(wood_cluster_id == cluster_id_to_map) |>
  mutate(
    facility = if_else(
      is.na(company_wood) | !is.finite(dist_wood_km),
      "No route",
      company_wood
    )
  )

# Read receiving facilities
facilities <- st_read(
  "data/processed/existing_facilities_joensuu.gpkg"
) |>
  st_transform(3067) |>
  filter(Name %in% buildings_map$company_wood)

# Define colours for receiving facilities
facility_colours <- c(
  "Kuusakoski Oy" = "#258A81",
  "Itä-Suomen Murskauskeskus" = "#E7A54B",
  "No route" = "#888888"
)

# Display cluster boundary, buildings and receiving facilities
allocation_map <-
  mapview(
    cluster_map,
    alpha.regions = 0,
    color = "black",
    lwd = 3,
    legend = FALSE,
    layer.name = "Wood cluster 27",
    map.types = "CartoDB.Positron"
  ) +
  mapview(
    buildings_map,
    zcol = "facility",
    col.regions = facility_colours,
    cex = 2,
    layer.name = "Building allocation"
  ) +
  mapview(
    facilities,
    zcol = "Name",
    cex = 7,
    layer.name = "Receiving facilities"
  )

allocation_map
