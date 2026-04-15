#This code is for urban mining method
#At this stage, the method is developed for Joensuu area

library(dodgr)
library(DBI)
library(RPostgres)
library(sf)
library(dplyr)
library(osmdata)
library(ggspatial)
library(tidyverse)
library(pacman, qiscoR)
library(mapview)
library(sfnetworks)

# Read datasets directly from GeoPackage files as sf objects

# Company dataset
yri_sf <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/yritys1.gpkg")

# Joensuu building stock dataset, RYHTI data
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/RYHTI_rakennuksen_Joensuun_asemakaava_alue.gpkg")

# OpenStreetMap (OSM) road network dataset (downloaded from Geofabrik)
roads <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/OSM_roads_rajattu2.gpkg")



# Reproject all spatial datasets to WGS84 (EPSG:4326)
buil_sf <- st_transform(buil_sf, 4326)
yri_sf  <- st_transform(yri_sf, 4326)
roads <- st_transform(roads, 4326)

# Visual inspection of spatial layers
mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf, col.regions = "red") +
  mapview(roads, col.regions = "steelblue")

#Create a random sample of 10000 buildings for development
buil_sf <- buil_sf %>%
  slice_sample(n = 800)

# Extract coordinates as matrices (longitude, latitude)
buil_mat <- st_coordinates(buil_sf)
yri_mat  <- st_coordinates(yri_sf)


##########
#Lisätään verteksi rakennuksen lähelle:

#1) Muunna tieaineisto sfnetworkiksi
#net <- as_sfnetwork(streets, directed = FALSE)

# 2) Muodosta pisteaineisto vain geometryllä (varmin tapa)
#b_pts <- st_as_sf(st_geometry(buil_sf))

# 3) Snäppää pisteet tielle ja lisää solmut tieverkkoon
#net_blended <- st_network_blend(net, b_pts)

# 4) Poimi blended‑solmut sf‑tasoksi (tässä ovat nyt uudet verteksit!)
#graph <- net_blended %>%
#  activate("nodes") %>%
#  st_as_sf()
###############



# Convert geometries to LINESTRING format (required for routing/network analysis)
roads <- st_cast(roads, "LINESTRING")

# Count road segments classified as "unknown"
sum(roads$fclass == "unknown", na.rm = TRUE)
# Reclassify "unknown" road segments as "residential"
roads$fclass[roads$fclass == "unknown"] <- "residential"

# Build routing graph from street network using dodgr (weighted for motor vehicles)
graph <- weight_streetnet(roads, wt_profile = "motorcar", type_col = "fclass")


# Compute distance matrix [n_buildings x n_companies]
# Values represent shortest-path distances based on the weighted network
dmat <- dodgr_dists(graph, from = buil_mat, to = yri_mat)
summary(dmat)
is.matrix(dmat)

#Check if there is NA's in the matrix
na_rows <- which(apply(dmat, 1, function(x) any(is.na(x))))
na_rows

# Tunnista rivit, joilla on NA
na_mask <- apply(dmat, 1, function(x) any(is.na(x)))

# Poista kaikki NA-rivit kaikista tarvittavista objekteista
summary(dmat)
dmat <- dmat[!na_mask, , drop = FALSE]
is.matrix(dmat)
dim(dmat)
na_rows <- which(apply(dmat, 1, function(x) any(is.na(x))))
na_rows

#############
#Create a solution that snaps the buildings to the road network!!!
#It can be that this is because downloaded OSM data, maybe try again?
#For development, delete the NA buildings


#Change to sf
#buil_na <- buil_sf[na_rows, ]
#Visualize
#mapview(buil_sf, col.regions = "orange") +
#  mapview(buil_na, col.regions = "red") +
#  mapview(roads, col.regions = "grey")

# Find the nearest road
#nearest_road <- st_nearest_feature(buil_na, roads)

# Distance
#dist_to_road <- st_distance(buil_na, roads[nearest_road, ], by_element = TRUE)
#summary(dist_to_road)
##########



# Find nearest company for each building based on minimum network distance
nearest <- apply(dmat, 1, function(x) which.min(x))
nearest <- unlist(nearest)

# Extract shortest paths to nearest company for each building
paths <- dodgr_paths(graph,
                     from = buil_mat,
                     to   = yri_mat[nearest, , drop = FALSE],
                     vertices = TRUE)

# Check whether the object "paths" exists in the environment
exists("paths")
# Check the length of the "paths" object (number of elements)
length(paths)


# Convert routing vertex sequences into LINESTRING geometries for visualization

# Extract network vertices from the routing graph
verts <- dodgr_vertices(graph)

str(verts)
is.numeric(verts$x)
is.numeric(verts$y)



# Function to convert a sequence of vertex IDs into an sf LINESTRING
make_line <- function(v) {
  # Match vertex IDs to coordinates
  xy <- verts[match(v, verts$id), c("x", "y")]
  # Create LINESTRING geometry from coordinate matrix
  st_linestring(as.matrix(xy))
}

# Apply the function to all routes (each route is a list; take first element)
route_lines <- lapply(paths, function(p) make_line(p[[1]]))

# Combine LINESTRING geometries into an sf sfc object (WGS84 CRS)
routes_sfc  <- st_sfc(route_lines, crs = 4326)

# Create final sf object with attributes and geometry
routes_sf <- st_sf(
  buil_id = buil_sf$id, #Building ID
  nearest_yri = nearest, #Index of nearest company
  length_m = dmat[cbind(seq_len(nrow(buil_sf)), nearest)], #Network distance
  geometry = routes_sfc
)

# Visualize road network, buildings, companies, and computed routes
mapview(roads, color = "steelblue", lwd = 3)+
  mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf,  col.regions = "red") +
  mapview(routes_sf, color = "black", lwd = 3)
  

# Create a dataset to visualize with MapLibre
# Write datasets to geopackage
st_write(buil_sf, "C:/Temp/codes/MAPLIBRE/data/processed/joensuu_mining_method_buildings.gpkg")
st_write(routes_sf, "C:/Temp/codes/MAPLIBRE/data/processed/joensuu_mining_method_routes.gpkg")
st_write(yri_sf, "C:/Temp/codes/MAPLIBRE/data/processed/joensuu_mining_method_companies.gpkg")

