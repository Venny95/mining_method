#This code is for urban mining method
#At this stage, the method is developed for Joensuu area
library(sf)
library(dodgr)
library(dplyr)
library(mapview)

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

#Filter detached houses from buildings where structural material is wood
names(buil_sf)
head(buil_sf)
buil_sf <- buil_sf %>%
  filter(kantavien_rakenteiden_rakennusaine == "Puu",
         paaasiallinen_kayttotarkoitus == "Pientalo")

#head(buil_sf)

# Visual inspection of spatial layers
mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf, col.regions = "red") +
  mapview(roads, col.regions = "steelblue")

# Extract coordinates as matrices (longitude, latitude) for dodgr
buil_mat <- st_coordinates(buil_sf)
yri_mat  <- st_coordinates(yri_sf)


# Convert geometries to LINESTRING format (required for routing/network analysis)
roads <- st_cast(roads, "LINESTRING")

# Count road segments classified as "unknown"
sum(roads$fclass == "unknown", na.rm = TRUE)
# Reclassify "unknown" road segments as "residential"
roads$fclass[roads$fclass == "unknown"] <- "residential"

# Build routing graph from street network using dodgr (weighted for motor vehicles)
graph <- weight_streetnet(roads, wt_profile = "motorcar", type_col = "fclass")


# Compute distance matrix [n_buildings x n_companies]
# Shortest distances from building to company, based on the weighted network
dmat <- dodgr_dists(graph, from = buil_mat, to = yri_mat)
dmat[is.na(dmat)] <- Inf #Prevents missing NA rows
summary(dmat)
is.matrix(dmat)


# Find nearest company for each building based on minimum network distance
nearest <- apply(dmat, 1, function(x) which.min(x))
nearest <- unlist(nearest)

#Statistics

buil_sf$dist_nearest <- dmat[cbind(
  seq_len(nrow(dmat)),
  nearest
)]
buil_sf$dist_nearest[is.infinite(buil_sf$dist_nearest)] <- NA

summary(buil_sf$dist_nearest)
mean(buil_sf$dist_nearest, na.rm = TRUE)
quantile(buil_sf$dist_nearest, c(0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
head(buil_sf)
