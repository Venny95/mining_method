# ==============================================================================
# Calculate building-level network accessibility
#
# Calculates road-network distances from each building to the nearest wood and
# concrete facility. Adds the distance, facility name, and facility coordinates
# to the building-level material stock dataset.
#
# Inputs:
#   data/processed/buildings_material_stock_joensuu.gpkg
#   Building-level spatial dataset containing estimated wood and concrete
#   quantities and building geometries.
#
#   data/processed/existing_facilities_joensuu.gpkg
#   Point dataset of existing facilities. The dataset must contain facility
#   names and indicators showing whether each facility accepts wood or concrete.
#
#   data/processed/network_graph.rds
#   Weighted HGV road network containing the main connected component.
#
#   data/processed/network_vertices.rds
#   Network vertices with unique IDs and geographic coordinates.
#
# Output:
#   data/processed/buildings_accessibility.gpkg
#   Building-level spatial dataset with network distances to the nearest wood
#   and concrete facilities, facility names, and facility coordinates, in
#   ETRS-TM35FIN (EPSG:3067).
# ==============================================================================


library(sf)
library(dodgr)
library(dplyr)

# ==============================================================================
# 1. Read input data
# ==============================================================================

# Building-level material stock dataset
buil_sf <- st_read(
  "C:/Temp/codes/MINING_METHOD/R/data/processed/buildings_material_stock_joensuu.gpkg"
)

# Existing wood and concrete processing facilities
yri_sf <- st_read(
  "data/processed/existing_facilities_joensuu.gpkg"
)

# Weighted HGV road network
graph_main <- readRDS(
  "data/processed/network_graph.rds"
)

# Network vertices
vertices <- readRDS(
  "data/processed/network_vertices.rds"
)

# Transform spatial datasets to WGS 84 to match the network coordinates
buil_sf <- st_transform(buil_sf, 4326)
yri_sf <- st_transform(yri_sf, 4326)

# ==============================================================================
# 2. Match buildings to network vertices
# ==============================================================================

# Match each building to the nearest connected network vertex
from <- match_points_to_verts(
  vertices,
  st_coordinates(buil_sf),
  connected = TRUE
)

# Replace row positions with network vertex IDs
from <- vertices$id[from]

# ==============================================================================
# 3. Separate facilities by accepted material
# ==============================================================================

# Facilities accepting wood
yri_wood <- yri_sf |>
  filter(wood == "yes")

# Facilities accepting concrete
yri_conc <- yri_sf |>
  filter(concrete == "yes")

# ==============================================================================
# 4. Match facilities to network vertices
# ==============================================================================

# Match wood facilities to the nearest connected network vertices
to_wood <- match_points_to_verts(
  vertices,
  st_coordinates(yri_wood),
  connected = TRUE
)

to_wood <- vertices$id[to_wood]

# Match concrete facilities to the nearest connected network vertices
to_conc <- match_points_to_verts(
  vertices,
  st_coordinates(yri_conc),
  connected = TRUE
)

to_conc <- vertices$id[to_conc]


# ==============================================================================
# 5. Calculate distances to wood facilities
# ==============================================================================

# Calculate network distances from all buildings to all wood facilities
# Rows represent buildings and columns represent facilities
d_wood <- dodgr_dists(
  graph_main,
  from,
  to_wood
)

# Replace unavailable routes with infinite distances
d_wood[is.na(d_wood)] <- Inf

# Identify the nearest reachable wood facility for each building
nearest_wood <- apply(d_wood, 1, function(x) {
  if (all(is.infinite(x))) {
    NA_integer_
  } else {
    which.min(x)
  }
})

# Identify buildings with at least one reachable wood facility
valid_wood <- !is.na(nearest_wood)

# Create an empty distance vector
dist_wood <- rep(NA_real_, nrow(d_wood))

# Extract the distance to the nearest reachable wood facility
dist_wood[valid_wood] <- d_wood[
  cbind(
    which(valid_wood),
    nearest_wood[valid_wood]
  )
]

# Convert distances from metres to kilometres
dist_wood_km <- dist_wood / 1000

# ==============================================================================
# 6. Calculate distances to concrete facilities
# ==============================================================================

# Calculate network distances from all buildings to all concrete facilities
# Rows represent buildings and columns represent facilities
d_conc <- dodgr_dists(
  graph_main,
  from,
  to_conc
)

# Replace unavailable routes with infinite distances
d_conc[is.na(d_conc)] <- Inf

# Identify the nearest reachable concrete facility for each building
nearest_conc <- apply(d_conc, 1, function(x) {
  if (all(is.infinite(x))) {
    NA_integer_
  } else {
    which.min(x)
  }
})

# Identify buildings with at least one reachable concrete facility
valid_conc <- !is.na(nearest_conc)

# Create an empty distance vector
dist_conc <- rep(NA_real_, nrow(d_conc))

# Extract the distance to the nearest reachable concrete facility
dist_conc[valid_conc] <- d_conc[
  cbind(
    which(valid_conc),
    nearest_conc[valid_conc]
  )
]

# Convert distances from metres to kilometres
dist_conc_km <- dist_conc / 1000


# ==============================================================================
# 7. Add accessibility results to building data
# ==============================================================================

# Add network distances to the nearest facilities
buil_sf$dist_wood_km <- dist_wood_km
buil_sf$dist_conc_km <- dist_conc_km

# Extract facility coordinates
wood_coords <- st_coordinates(yri_wood)
conc_coords <- st_coordinates(yri_conc)

# Create empty columns for the nearest wood facility
buil_sf$wood_company_lon <- NA_real_
buil_sf$wood_company_lat <- NA_real_
buil_sf$company_wood <- NA_character_

# Add information on the nearest reachable wood facility
buil_sf$wood_company_lon[valid_wood] <-
  wood_coords[nearest_wood[valid_wood], 1]

buil_sf$wood_company_lat[valid_wood] <-
  wood_coords[nearest_wood[valid_wood], 2]

buil_sf$company_wood[valid_wood] <-
  yri_wood$Name[nearest_wood[valid_wood]]

# Create empty columns for the nearest concrete facility
buil_sf$concrete_company_lon <- NA_real_
buil_sf$concrete_company_lat <- NA_real_
buil_sf$company_conc <- NA_character_

# Add information on the nearest reachable concrete facility
buil_sf$concrete_company_lon[valid_conc] <-
  conc_coords[nearest_conc[valid_conc], 1]

buil_sf$concrete_company_lat[valid_conc] <-
  conc_coords[nearest_conc[valid_conc], 2]

buil_sf$company_conc[valid_conc] <-
  yri_conc$Name[nearest_conc[valid_conc]]

# ==============================================================================
# 8. Check accessibility results
# ==============================================================================

# Inspect selected accessibility attributes
head(
  buil_sf[, c(
    "dist_wood_km",
    "company_wood",
    "dist_conc_km",
    "company_conc"
  )]
)

# Summarize network distances
summary(buil_sf$dist_wood_km)
summary(buil_sf$dist_conc_km)

# Check the allocation of buildings to facilities
table(buil_sf$company_wood, useNA = "ifany")
table(buil_sf$company_conc, useNA = "ifany")

# Check the numbers of unique matched network vertices
length(unique(from))
length(unique(to_wood))
length(unique(to_conc))

# ==============================================================================
# 9. Save accessibility results
# ==============================================================================

# Transform the output to ETRS-TM35FIN
buil_sf <- buil_sf |>
  st_transform(3067)

# Save building-level accessibility data
st_write(
  buil_sf,
  "data/processed/buildings_accessibility.gpkg",
  delete_dsn = TRUE
)

