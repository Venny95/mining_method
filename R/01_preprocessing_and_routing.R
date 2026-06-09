#This code is for urban mining method
#At this stage, the method is developed for Joensuu area
library(sf)
library(dodgr)
library(dplyr)
library(mapview)

# =========================
# 1.DATA
# =========================

# Read datasets directly from GeoPackage files as sf objects

# Company dataset
yri_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/existing_facilities_joensuu.gpkg")

# Joensuu building stock dataset, RYHTI data
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/ryhti_buildings_joensuu.gpkg")

# OpenStreetMap (OSM) road network dataset (downloaded from Geofabrik)
roads <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/OSM_roads_joensuu_small.gpkg")

# Reproject all spatial datasets to EPSG:3067
buil_sf <- st_transform(buil_sf, 3067)
yri_sf  <- st_transform(yri_sf, 3067)
roads <- st_transform(roads, 3067)

# =========================
# 2.EXTRACT ATTRIBUTES
# =========================

# For MI COEFFICIENT a building decade is needed
# Building year for demo tool information pop-up

#Extracting the building decade (NAs untouched)
sum(is.na(buil_sf$valmistumispaivamaara))

buil_sf <- buil_sf |>
  mutate(
    decade = if_else(
      grepl("^\\d{4}", valmistumispaivamaara),
      as.numeric(substr(valmistumispaivamaara, 1, 3)) * 10,
      NA_real_
    )
  )

#Extracting the building year (NAs untouched)
buil_sf <- buil_sf |>
  mutate(
    year = if_else(
      grepl("^\\d{4}", valmistumispaivamaara),
      as.numeric(substr(valmistumispaivamaara, 1, 4)),
      NA_real_
    )
  )

# =========================
# 3.MATERIAL INTENSITY
# =========================

# MI COEFFICIENT for wood
# gross floor area * MI (kg / m2)
buil_sf <- buil_sf |>
  mutate(
    MI_wood = case_when(
      decade == 1940 ~ kerrosala * 152,
      decade == 1950 ~ kerrosala * 48,
      decade == 1960 ~ kerrosala * 23,
      decade == 1970 ~ kerrosala * 53,
      decade == 1980 ~ kerrosala * 38,
      decade == 1990 ~ kerrosala * 33,
      decade == 2000 ~ kerrosala * 53
    )
  )

head(buil_sf$MI_wood)



# MI COEFFICIENT for concrete
# gross floor area * MI (kg / m2)
buil_sf <- buil_sf |>
  mutate(
    MI_concrete = case_when(
      decade == 1940 ~ kerrosala * 758,
      decade == 1950 ~ kerrosala * 818,
      decade == 1960 ~ kerrosala * 840,
      decade == 1970 ~ kerrosala * 244,
      decade == 1980 ~ kerrosala * 468,
      decade == 1990 ~ kerrosala * 361,
      decade == 2000 ~ kerrosala * 467
    )
  )



# =========================
# 4.ROUTING
# =========================

# Convert geometries to LINESTRING format (required for routing/network analysis)
roads <- st_cast(roads, "LINESTRING")
cat("Edges:", nrow(roads), "edges\n")

# Count road segments classified as "unknown"
sum(roads$fclass == "unknown", na.rm = TRUE)
# Reclassify "unknown" road segments as "residential"
roads$fclass[roads$fclass == "unknown"] <- "residential"

# Build routing graph from street network using dodgr (weighted for motor vehicles)
graph <- weight_streetnet(roads, wt_profile = "hgv", type_col = "fclass")

# Choose only main component
graph_main <- graph [graph$component == 1, ]
cat("Main component edges:", nrow(graph_main), "edges\n")  

#See how dodgr network looks like
head(graph_main, 30)
table (graph_main$component)

#Companies based on material
yri_wood <- yri_sf %>% filter(wood == "yes")
yri_conc <- yri_sf %>% filter(concrete == "yes")

## Extract point coordinates as X/Y matrices for dodgr
buil_mat <- st_coordinates(buil_sf)
yri_wood_mat <- st_coordinates(yri_wood)
yri_conc_mat <- st_coordinates(yri_conc)

# Compute shortest network distance matrix between buildings and companies
dmat_wood <- dodgr_dists(graph_main, from = buil_mat, to = yri_wood_mat)
dmat_conc <- dodgr_dists(graph_main, from = buil_mat, to = yri_conc_mat)

dim (dmat_wood); range (dmat_wood, na.rm = TRUE)
dim (dmat_conc); range (dmat_conc, na.rm = TRUE)

# NA indicates that no valid route exists between a building and a company.
sum(is.na(dmat_wood))
sum(is.na(dmat_conc))

# Replace missing values (NA) with Inf
dmat_wood[is.na(dmat_wood)] <- Inf #Prevents missing NA rows
dmat_conc[is.na(dmat_conc)] <- Inf

# For each building (row), find the index of the closest company based on minimum network distance.
# Check that finite values exist and the building is connected to the graph, otherwise no result
nearest_wood <- apply(dmat_wood, 1, function(x) {
  if (all(is.infinite(x))) NA_integer_ else which.min(x)
})
#nearest_wood <- unlist(nearest_wood) EITARVITA?


nearest_conc <- apply(dmat_conc, 1, function(x) {
  if (all(is.infinite(x))) NA_integer_ else which.min(x)
})
#nearest_conc <- unlist(nearest_conc) EITARVITA?


#=====================================================
# 5.DISTANCES
##====================================================

#Prepare column for distances
buil_sf$dist_wood_km <- NA_real_
valid_wood <- !is.na(nearest_wood)

# Extract distance to nearest wood company
buil_sf$dist_wood_km[valid_wood] <- dmat_wood[
  cbind(which(valid_wood), nearest_wood[valid_wood])
] / 1000

#Prepare column for distances
buil_sf$dist_conc_km <- NA_real_
valid_conc <- !is.na(nearest_conc)

# Extract distance to nearest concrete company
buil_sf$dist_conc_km[valid_conc] <- dmat_conc[
  cbind(which(valid_conc), nearest_conc[valid_conc])
] / 1000

# Add company coordinates to buil_sf
# Company coordinates
wood_coords <- st_coordinates(yri_wood)
conc_coords <- st_coordinates(yri_conc)

# Add selected company coordinates to buil_sf (WOOD)
buil_sf$wood_company_x <- wood_coords[nearest_wood, 1]
buil_sf$wood_company_y <- wood_coords[nearest_wood, 2]

# Add selected company coordinates to buil_sf (CONCRETE)
buil_sf$concrete_company_x <- conc_coords[nearest_conc, 1]
buil_sf$concrete_company_y <- conc_coords[nearest_conc, 2]

# Add company names corresponding to nearest selection
buil_sf$company_wood <- yri_wood$Name[nearest_wood]
buil_sf$company_conc <- yri_conc$Name[nearest_conc]

head(buil_sf)

table(buil_sf$company_wood, useNA = "ifany")
table(buil_sf$company_conc, useNA = "ifany")


names(graph_main)
summary(graph_main$d)
summary(graph_main$d_weighted)
range(graph_main$d, na.rm = TRUE)
range(graph_main$d_weighted, na.rm = TRUE)

st_crs(roads)
st_bbox(roads)
st_crs(buil_sf)
st_bbox(buil_sf)

st_crs(yri_sf)
st_bbox(yri_sf)

#===========================
# 6.CO2 EMISSIONS
# ==========================

# Formula: material tonnes * km * coefficient by truck type (kg CO2e / t km) = xx kg CO2e
# In the coefficient, full load drive from site and empty load drive back to the site is included

# Load factor assumption (Source: Rakennuksen vähähiilisyyden arviointimenetelmä, Ympäristöministeriö):
# - 80% load when departing from site
# - 0% load when returning
# -> 40% load for coefficient

# - Urban and rural road types have different coefficients, urban coefficient used here
# - Vehicle type: dump truck (32 t), payload capacity 19 t ("maansiirtoauto (32 t), kantavuus 19t")

#For wood transportation
buil_sf$wood_emissions <- (buil_sf$MI_wood/1000) * buil_sf$dist_wood_km * 0.17925

#For concrete transportation
buil_sf$conc_emissions <- (buil_sf$MI_concrete/1000) * buil_sf$dist_conc_km * 0.17925

# Total emissions
buil_sf$total_emissions <- buil_sf$wood_emissions + buil_sf$conc_emissions

summary(buil_sf$dist_conc_km, buil_sf$dist_wood_km)
#Save point buildings for location-allocation analysis as geopackage
#st_write(buil_sf, "C:/Temp/codes/MINING_METHOD/R/data/processed/buil_points_processed_joensuu.gpkg", delete_dsn = TRUE)

# =================
# 7.BUILDING POLYGONS
# =================

# Goal is to attach calculated attributes from point features to building polygons for visualization
# And filter to keep only polygons that contains a point, i.e. polygons representing wooden detached buildings

# Read building polygons (OSM data)
buil_polygons <- st_read(
  "C:/Temp/codes/MINING_METHOD/R/data/processed/Joensuu_building_polygons_OSM.gpkg",) |>
  st_transform(3067)

# Join points within 5 m of polygons,
# keep only polygons that get at least one point
buil_polygons <- buil_polygons |>
  st_join(
    buil_sf,
    join = st_is_within_distance,
    dist = 5,
    left = FALSE
  )

# Remove excess attribute fields
buil_polygons <- buil_polygons %>%
  select(-kulttuurihistoriallinen_merkittavyys, -suojeltu, -purkamispaivamaara, -kuntanumero)

# =========
# 8.SAVE THE DATA
# =========

#Change CRS to 4326 for MapLibre
#buil_polygons <- st_transform(buil_polygons, 4326)

#st_write(buil_polygons, "C:/Temp/codes/MINING_METHOD/frontend/data/buil_polygons.geojson", delete_dsn = TRUE)

