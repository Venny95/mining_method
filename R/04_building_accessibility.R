# ==============================================================================
# 04_building_accessibility.R
#
# Calculate road-network distances from Joensuu buildings to the nearest
# reachable facilities accepting wood or concrete.
#
# Routes are selected using the HGV-weighted dodgr network prepared in 03.
# Reported distances are the actual lengths of the selected network routes.
# Buildings and facilities are matched to the nearest network vertices;
# connections from the points to those vertices are not included in distances.
#
# Inputs:
#   data/processed/buildings_material_stock_joensuu.gpkg
#   data/processed/existing_facilities_joensuu.gpkg
#   data/processed/network_graph.rds
#   data/processed/network_vertices.rds
#
# Output:
#   data/processed/buildings_accessibility.gpkg
#   Building-level network distances (km), facility names and coordinates,
#   in ETRS-TM35FIN (EPSG:3067).
#
# Run from the R/ directory.
# ==============================================================================

library(sf)
library(dodgr)
library(dplyr)

# ==============================================================================
# 1. Read and validate input data
# ==============================================================================

buil_sf <- st_read("data/processed/buildings_material_stock_joensuu.gpkg")
yri_sf <- st_read("data/processed/existing_facilities_joensuu.gpkg")
graph_main <- readRDS("data/processed/network_graph.rds")
vertices <- readRDS("data/processed/network_vertices.rds")

stopifnot(
  nrow(buil_sf) > 0,
  nrow(yri_sf) > 0,
  all(c("Name", "wood", "concrete") %in% names(yri_sf)),
  "id" %in% names(vertices),
  nrow(graph_main) > 0,
  nrow(vertices) > 0,
  !anyNA(vertices$id),
  !anyDuplicated(vertices$id)
)

# dodgr uses geographic (longitude/latitude) coordinates.
buil_sf <- st_transform(buil_sf, 4326)
yri_sf <- st_transform(yri_sf, 4326)

# ==============================================================================
# 2. Match buildings to network vertices
# ==============================================================================

# match_points_to_verts() returns row positions in the vertices table.
from <- match_points_to_verts(
  vertices,
  st_coordinates(buil_sf),
  connected = TRUE
)
from <- vertices$id[from]

stopifnot(length(from) == nrow(buil_sf), !anyNA(from))

# ==============================================================================
# 3. Identify facilities accepting each material
# ==============================================================================

yri_wood <- yri_sf |> filter(wood == "yes")
yri_conc <- yri_sf |> filter(concrete == "yes")

stopifnot(
  nrow(yri_wood) > 0,
  nrow(yri_conc) > 0,
  !anyNA(yri_wood$Name),
  !anyNA(yri_conc$Name)
)

# ==============================================================================
# 4. Match facilities to network vertices
# ==============================================================================

to_wood <- match_points_to_verts(
  vertices,
  st_coordinates(yri_wood),
  connected = TRUE
)
to_wood <- vertices$id[to_wood]

to_conc <- match_points_to_verts(
  vertices,
  st_coordinates(yri_conc),
  connected = TRUE
)
to_conc <- vertices$id[to_conc]

stopifnot(
  length(to_wood) == nrow(yri_wood),
  length(to_conc) == nrow(yri_conc),
  !anyNA(to_wood),
  !anyNA(to_conc)
)

# ==============================================================================
# 5. Calculate distances to wood facilities
# ==============================================================================

# Each matrix row represents a building; each column represents a facility.
d_wood <- dodgr_dists(graph_main, from, to_wood)
d_wood[is.na(d_wood)] <- Inf

# Unreachable buildings retain NA for their distance and assigned facility.
nearest_wood <- apply(d_wood, 1, function(x) {
  if (all(is.infinite(x))) NA_integer_ else which.min(x)
})
valid_wood <- !is.na(nearest_wood)

dist_wood <- rep(NA_real_, nrow(d_wood))
dist_wood[valid_wood] <- d_wood[
  cbind(which(valid_wood), nearest_wood[valid_wood])
]
dist_wood_km <- dist_wood / 1000

# ==============================================================================
# 6. Calculate distances to concrete facilities
# ==============================================================================

d_conc <- dodgr_dists(graph_main, from, to_conc)
d_conc[is.na(d_conc)] <- Inf

nearest_conc <- apply(d_conc, 1, function(x) {
  if (all(is.infinite(x))) NA_integer_ else which.min(x)
})
valid_conc <- !is.na(nearest_conc)

dist_conc <- rep(NA_real_, nrow(d_conc))
dist_conc[valid_conc] <- d_conc[
  cbind(which(valid_conc), nearest_conc[valid_conc])
]
dist_conc_km <- dist_conc / 1000

# ==============================================================================
# 7. Attach accessibility results to buildings
# ==============================================================================

buil_sf$dist_wood_km <- dist_wood_km
buil_sf$dist_conc_km <- dist_conc_km

wood_coords <- st_coordinates(yri_wood)
conc_coords <- st_coordinates(yri_conc)

# Keep facility information missing for buildings without a route.
buil_sf$wood_company_lon <- NA_real_
buil_sf$wood_company_lat <- NA_real_
buil_sf$company_wood <- NA_character_

buil_sf$wood_company_lon[valid_wood] <- wood_coords[nearest_wood[valid_wood], 1]
buil_sf$wood_company_lat[valid_wood] <- wood_coords[nearest_wood[valid_wood], 2]
buil_sf$company_wood[valid_wood] <- yri_wood$Name[nearest_wood[valid_wood]]

buil_sf$concrete_company_lon <- NA_real_
buil_sf$concrete_company_lat <- NA_real_
buil_sf$company_conc <- NA_character_

buil_sf$concrete_company_lon[valid_conc] <- conc_coords[nearest_conc[valid_conc], 1]
buil_sf$concrete_company_lat[valid_conc] <- conc_coords[nearest_conc[valid_conc], 2]
buil_sf$company_conc[valid_conc] <- yri_conc$Name[nearest_conc[valid_conc]]

# ==============================================================================
# 8. Report and validate accessibility results
# ==============================================================================

# Include no-route cases explicitly; missing distances are not zero distances.
accessibility_summary <- data.frame(
  material = c("wood", "concrete"),
  facilities = c(nrow(yri_wood), nrow(yri_conc)),
  unique_facility_vertices = c(length(unique(to_wood)), length(unique(to_conc))),
  buildings_reached = c(sum(valid_wood), sum(valid_conc)),
  buildings_unreachable = c(sum(!valid_wood), sum(!valid_conc)),
  median_distance_km = c(
    if (any(valid_wood)) median(dist_wood_km[valid_wood]) else NA_real_,
    if (any(valid_conc)) median(dist_conc_km[valid_conc]) else NA_real_
  ),
  max_distance_km = c(
    if (any(valid_wood)) max(dist_wood_km[valid_wood]) else NA_real_,
    if (any(valid_conc)) max(dist_conc_km[valid_conc]) else NA_real_
  )
)

cat("\nBuilding accessibility summary:\n")
cat("Buildings:", nrow(buil_sf), "\n")
cat("Unique building vertices:", length(unique(from)), "\n")
print(accessibility_summary, row.names = FALSE)

cat("\nNearest wood facilities (building counts):\n")
print(table(buil_sf$company_wood, useNA = "ifany"))

cat("\nNearest concrete facilities (building counts):\n")
print(table(buil_sf$company_conc, useNA = "ifany"))

stopifnot(
  nrow(d_wood) == nrow(buil_sf),
  nrow(d_conc) == nrow(buil_sf),
  ncol(d_wood) == nrow(yri_wood),
  ncol(d_conc) == nrow(yri_conc),
  all(is.finite(buil_sf$dist_wood_km[valid_wood])),
  all(is.finite(buil_sf$dist_conc_km[valid_conc])),
  all(is.na(buil_sf$dist_wood_km[!valid_wood])),
  all(is.na(buil_sf$dist_conc_km[!valid_conc]))
)

# ==============================================================================
# 9. Save building-level accessibility results
# ==============================================================================

buil_sf <- st_transform(buil_sf, 3067)
st_write(
  buil_sf,
  "data/processed/buildings_accessibility.gpkg",
  delete_dsn = TRUE
)
cat("\nAccessibility results saved to data/processed/buildings_accessibility.gpkg\n")

