# ==============================================================================
# 05b_cluster_facility_allocation.R
#
# Summarise how material stocks within accepted wood and concrete clusters are
# assigned to receiving facilities using the building-level shortest routes
# calculated in 04_building_accessibility.R.
#
# The allocation is distance-based, not a prediction of actual material flows.
# Facility shares refer to material with a valid route and assigned facility;
# material without a route is excluded from those shares.
#
# Inputs:
#   data/processed/buildings_accessibility.gpkg
#   data/processed/wood_clusters.gpkg
#   data/processed/concrete_clusters.gpkg
#   data/processed/wood_cluster_ids.tif
#   data/processed/concrete_cluster_ids.tif
#   data/processed/existing_facilities_joensuu.gpkg (optional map only)
#
# Outputs:
#   data/processed/wood_facility_allocation.csv
#   data/processed/concrete_facility_allocation.csv
#   data/processed/wood_cluster_facility_summary.csv
#   data/processed/concrete_cluster_facility_summary.csv
#
# Run from the R/ directory. Plots and an interactive map are optional.
# ==============================================================================

library(sf)
library(terra)
library(dplyr)

show_plots <- FALSE
show_map <- FALSE

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
        "dist_wood_km", "dist_conc_km",
        "company_wood", "company_conc") %in% names(buildings)),
  all(c("cluster_id", "material_t", "class") %in% names(wood_clusters)),
  all(c("cluster_id", "material_t", "class") %in% names(concrete_clusters)),
  !anyNA(wood_clusters$cluster_id),
  !anyNA(concrete_clusters$cluster_id),
  !anyDuplicated(wood_clusters$cluster_id),
  !anyDuplicated(concrete_clusters$cluster_id)
)

buildings <- st_transform(buildings, 3067)

# ==============================================================================
# 2. Assign buildings to the original raster cluster IDs
# ==============================================================================

# Use the same building-point representation as scripts 02 and 05.
building_points <- centroids(vect(buildings), inside = TRUE)

# Use terra::extract explicitly to avoid conflicts with other extract() methods.
buildings$wood_cluster_id <- terra::extract(
  wood_raster, building_points, ID = FALSE
)[[1]]

buildings$concrete_cluster_id <- terra::extract(
  concrete_raster, building_points, ID = FALSE
)[[1]]

# Raster IDs also contain clusters below the material threshold. Those are
# excluded by matching the accepted polygon IDs inside the function below.

# ==============================================================================
# 3. Calculate allocations by cluster and receiving facility
# ==============================================================================

calculate_facility_allocation <- function(
    buildings, clusters, cluster_col, material_col, distance_col, facility_col
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
  
  # Check that raster-based building allocation reproduces script 02's stocks.
  material_check <- matched |>
    group_by(cluster_id) |>
    summarise(matched_material_t = sum(material), .groups = "drop")
  
  differences <- clusters |>
    st_drop_geometry() |>
    select(cluster_id, material_t) |>
    left_join(material_check, by = "cluster_id") |>
    mutate(difference_t = abs(material_t - matched_material_t))
  
  stopifnot(
    !anyNA(differences$difference_t),
    all(differences$difference_t < 0.01)
  )
  
  # Only positive material stocks with a finite route and known destination
  # are included. Missing distances and facility names remain unallocated.
  allocated <- matched |>
    filter(
      material > 0,
      is.finite(distance),
      !is.na(facility),
      facility != ""
    )
  
  # Material-weighted distance and shares within each accepted cluster.
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
  
  # Retain all accepted clusters, including any with no assigned facility.
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
  
  list(allocation = allocation, cluster_summary = cluster_summary)
}

# ==============================================================================
# 4. Calculate wood and concrete allocations
# ==============================================================================

wood_results <- calculate_facility_allocation(
  buildings, wood_clusters, "wood_cluster_id", "material_wood_t",
  "dist_wood_km", "company_wood"
)

concrete_results <- calculate_facility_allocation(
  buildings, concrete_clusters, "concrete_cluster_id", "material_concrete_t",
  "dist_conc_km", "company_conc"
)

wood_allocation <- wood_results$allocation
concrete_allocation <- concrete_results$allocation
wood_facility_summary <- wood_results$cluster_summary
concrete_facility_summary <- concrete_results$cluster_summary

stopifnot(
  sum(wood_allocation$material_t) <= sum(wood_clusters$material_t) + 0.01,
  sum(concrete_allocation$material_t) <= sum(concrete_clusters$material_t) + 0.01
)

# ==============================================================================
# 5. Report allocation results
# ==============================================================================

allocation_overview <- data.frame(
  material = c("wood", "concrete"),
  accepted_clusters = c(nrow(wood_clusters), nrow(concrete_clusters)),
  allocated_material_t = c(sum(wood_allocation$material_t),
                           sum(concrete_allocation$material_t)),
  unallocated_material_t = c(
    sum(wood_clusters$material_t) - sum(wood_allocation$material_t),
    sum(concrete_clusters$material_t) - sum(concrete_allocation$material_t)
  )
)

cat("\nFacility allocation overview:\n")
print(allocation_overview, row.names = FALSE)

cat("\nWood cluster facility summary:\n")
print(wood_facility_summary, row.names = FALSE)
cat("\nConcrete cluster facility summary:\n")
print(concrete_facility_summary, row.names = FALSE)

# Review allocations within the largest material-stock cluster of each type.
largest_wood_id <- wood_facility_summary$cluster_id[1]
largest_concrete_id <- concrete_facility_summary$cluster_id[1]

cat("\nFacility allocation in the largest wood cluster:", largest_wood_id, "\n")
print(wood_allocation |> filter(cluster_id == largest_wood_id), row.names = FALSE)
cat("\nFacility allocation in the largest concrete cluster:", largest_concrete_id, "\n")
print(concrete_allocation |> filter(cluster_id == largest_concrete_id), row.names = FALSE)

# ==============================================================================
# 6. Save the original four result tables
# ==============================================================================

write.csv(wood_allocation, "data/processed/wood_facility_allocation.csv",
          row.names = FALSE)
write.csv(concrete_allocation, "data/processed/concrete_facility_allocation.csv",
          row.names = FALSE)
write.csv(wood_facility_summary, "data/processed/wood_cluster_facility_summary.csv",
          row.names = FALSE)
write.csv(concrete_facility_summary, "data/processed/concrete_cluster_facility_summary.csv",
          row.names = FALSE)

cat("\nFacility allocation CSV outputs saved to data/processed/.\n")

# ==============================================================================
# 7. Optional charts: five largest clusters by material stock
# ==============================================================================

if (show_plots) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install the ggplot2 package to display allocation charts.")
  }
  library(ggplot2)
  
  wood_top5 <- wood_facility_summary |>
    slice_max(material_t, n = 5) |>
    pull(cluster_id)
  concrete_top5 <- concrete_facility_summary |>
    slice_max(material_t, n = 5) |>
    pull(cluster_id)
  
  print(
    ggplot(wood_allocation |> filter(cluster_id %in% wood_top5),
           aes(x = factor(cluster_id), y = material_share_pct, fill = facility)) +
      geom_col() +
      labs(title = "Wood allocation by receiving facility", x = "Cluster ID",
           y = "Share of routed material (%)", fill = "Facility") +
      theme_minimal()
  )
  
  print(
    ggplot(concrete_allocation |> filter(cluster_id %in% concrete_top5),
           aes(x = factor(cluster_id), y = material_share_pct, fill = facility)) +
      geom_col() +
      labs(title = "Concrete allocation by receiving facility", x = "Cluster ID",
           y = "Share of routed material (%)", fill = "Facility") +
      theme_minimal()
  )
}

# ==============================================================================
# 8. Optional map: facility allocation within the largest wood cluster
# ==============================================================================

if (show_map) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("Install the mapview package to display an allocation map.")
  }
  library(mapview)
  
  cluster_map <- wood_clusters |>
    filter(cluster_id == largest_wood_id)
  
  buildings_map <- buildings |>
    filter(wood_cluster_id == largest_wood_id) |>
    mutate(
      facility = if_else(
        is.na(company_wood) | !is.finite(dist_wood_km),
        "No route", company_wood
      )
    )
  
  facilities <- st_read("data/processed/existing_facilities_joensuu.gpkg") |>
    st_transform(3067) |>
    filter(Name %in% buildings_map$company_wood)
  
  facility_names <- sort(unique(buildings_map$facility))
  facility_colours <- setNames(
    grDevices::hcl.colors(length(facility_names), "Dark 3"), facility_names
  )
  if ("No route" %in% facility_names) facility_colours["No route"] <- "#888888"
  buildings_map$facility <- factor(buildings_map$facility, levels = facility_names)
  
  allocation_map <-
    mapview(
      cluster_map, alpha.regions = 0, color = "black", lwd = 3,
      legend = FALSE, layer.name = paste("Wood cluster", largest_wood_id),
      map.types = "CartoDB.Positron"
    ) +
    mapview(
      buildings_map, zcol = "facility",
      col.regions = unname(facility_colours), cex = 2,
      layer.name = "Building allocation"
    ) +
    mapview(
      facilities, zcol = "Name", cex = 7,
      layer.name = "Receiving facilities"
    )
  
  print(allocation_map)
}

