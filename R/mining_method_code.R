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

# Joensuu building stock dataset
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/Joensuu_kantakaupungin_asemakaavoitettu_alue_Joensuun_kaupunki_20251029.gpkg")

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

#Create a random sample of 500 buildings for development
buil_sf <- buil_sf %>%
  slice_sample(n = 500)

# Extract coordinates as matrices (longitude, latitude)
buil_mat <- st_coordinates(buil_sf)
yri_mat  <- st_coordinates(yri_sf)



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
  

# Create a test dataset to validate MapLibre functionality
# Reduce datasets to minimal attributes for testing
names(buil_sf)
testidata <- buil_sf %>% 
  select(id)

names(routes_sf)
testitiet <- routes_sf %>% 
  select(buil_id)


# Write building test dataset to database
st_write(
  obj    = testidata,                                 # sf-object to write
  dsn    = con,                                       # DBI connection
  layer  = Id(schema = "public", table = "rakennukset_testi"),     # target schema + table
  layer_options = c("FID=gid"),                       # optional: name of primary key field
  delete_layer = TRUE                                 # overwrite existing table if it exists
)

st_write(
  obj    = testitiet,
  dsn    = con,
  layer  = Id(schema = "public", table = "tiet_testi"),
  layer_options = c("FID=gid"),
  delete_layer = TRUE
)
