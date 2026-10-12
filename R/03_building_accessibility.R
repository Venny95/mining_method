# Read the data
# rakennusten ja yritysten sovittaminen verkkoon
# rakennuskohtaiset etäisyydet
# lähimmän yrityksen tunniste


library(sf)
library(dodgr)
library(dplyr)

#=========================
# 1. READ INPUT DATA
#=========================

buil_sf <- st_read(
  "C:/Temp/codes/MINING_METHOD/R/data/processed/buildings_material_stock_joensuu.gpkg"
)

yri_sf <- st_read(
  "data/processed/existing_facilities_joensuu.gpkg"
)

graph_main <- readRDS(
  "data/processed/network_graph.rds"
)

vertices <- readRDS(
  "data/processed/network_vertices.rds"
)

# dodgr network vertices use EPSG:4326 coordinates
buil_sf <- st_transform(buil_sf, 4326)
yri_sf <- st_transform(yri_sf, 4326)

#=======
# 2. Match buildings to network
#=======

from <- match_points_to_verts(
  vertices,
  st_coordinates(buil_sf),
  connected = TRUE
)

from <- vertices$id[from]

#=====
# 3. Split facilities by material
#=====
#Split companies by material
yri_wood <- yri_sf |>
  filter(wood == "yes")

yri_conc <- yri_sf |>
  filter(concrete == "yes")

#=====
# 4. Match facilities to network
#=====

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


# =========================
# 2.ROUTING
# =========================

########################
# WOOD
########################

# Compute the shortest-path distance matrix between all buildings and all companies.
# Rows = buildings ('from')
# Columns = companies ('to')
# Each cell contains the network distance from one building to one company
d_wood <- dodgr_dists(graph_main, from, to_wood)

# Replace missing routes with Inf before finding minimum distances
d_wood[is.na(d_wood)] <- Inf

# Identify the nearest reachable wood facility for each building
nearest_wood <- apply(d_wood, 1, function(x) {
  if (all(is.infinite(x))) {
    NA_integer_
  } else {
    which.min(x)
  }
})

# One result position for each building
dist_wood <- rep(NA_real_, nrow(d_wood))

# Buildings with at least one reachable wood facility
valid_wood <- !is.na(nearest_wood)

# Extract the distance to the nearest reachable facility
dist_wood[valid_wood] <- d_wood[
  cbind(
    which(valid_wood),
    nearest_wood[valid_wood]
  )
]

# Convert metres to kilometres
dist_wood_km <- dist_wood / 1000

head(dist_wood_km)
summary(dist_wood_km)


#################################
# CONCRETE
#################################
d_conc <- dodgr_dists(graph_main, from, to_conc)

d_conc[is.na(d_conc)] <- Inf

nearest_conc <- apply(d_conc, 1, function(x) {
  if (all(is.infinite(x))) {
    NA_integer_
  } else {
    which.min(x)
  }
})

dist_conc <- rep(NA_real_, nrow(d_conc))

valid_conc <- !is.na(nearest_conc)

dist_conc[valid_conc] <- d_conc[
  cbind(
    which(valid_conc),
    nearest_conc[valid_conc]
  )
]

dist_conc_km <- dist_conc / 1000


# =========================
# 3.WRITE RESULTS TO buil_sf
# =========================

# Add distance results to buil_sf
buil_sf$dist_wood_km <- dist_wood_km
buil_sf$dist_conc_km <- dist_conc_km

# Extract company coordinates
wood_coords <- st_coordinates(yri_wood)
conc_coords <- st_coordinates(yri_conc)

# Prepare empty columns for wood company info
buil_sf$wood_company_lon <- NA_real_
buil_sf$wood_company_lat <- NA_real_
buil_sf$company_wood <- NA_character_

# Fill wood company info only for buildings with a valid nearest wood company
buil_sf$wood_company_lon[valid_wood] <- wood_coords[nearest_wood[valid_wood], 1]
buil_sf$wood_company_lat[valid_wood] <- wood_coords[nearest_wood[valid_wood], 2]
buil_sf$company_wood[valid_wood] <- yri_wood$Name[nearest_wood[valid_wood]]

# Prepare empty columns for concrete company info
buil_sf$concrete_company_lon <- NA_real_
buil_sf$concrete_company_lat <- NA_real_
buil_sf$company_conc <- NA_character_

# Fill concrete company info only for buildings with a valid nearest concrete company
buil_sf$concrete_company_lon[valid_conc] <- conc_coords[nearest_conc[valid_conc], 1]
buil_sf$concrete_company_lat[valid_conc] <- conc_coords[nearest_conc[valid_conc], 2]
buil_sf$company_conc[valid_conc] <- yri_conc$Name[nearest_conc[valid_conc]]

# Quick check
head(buil_sf[, c("dist_wood_km", "company_wood",
                 "dist_conc_km", "company_conc")])

summary(buil_sf$dist_wood_km)
summary(buil_sf$dist_conc_km)

table(buil_sf$company_wood, useNA = "ifany")
table(buil_sf$company_conc, useNA = "ifany")

#============
# Save the results
#===============

#Change CRS to 3067
buil_sf <- buil_sf |>
  st_transform(3067)

st_write(
buil_sf,
"data/processed/buildings_accessibility.gpkg",
delete_dsn = TRUE
)


#Check that everything is ok

summary(dist_wood_km)
summary(dist_conc_km)

length(unique(from))
length(unique(to_wood))
length(unique(to_conc))

table(buil_sf$company_wood, useNA = "ifany")
table(buil_sf$company_conc, useNA = "ifany")

