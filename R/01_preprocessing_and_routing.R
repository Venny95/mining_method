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

# OpenStreetMap (OSM) road network dataset (downloaded from Geofabrik)
roads <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/OSM_roads_joensuu_small.gpkg")

# Joensuu building stock dataset, RYHTI data
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/ryhti_buildings_joensuu.gpkg")
# Remove excess attribute fields
buil_sf <- buil_sf %>%
  select(-kulttuurihistoriallinen_merkittavyys, -suojeltu, -purkamispaivamaara, -kuntanumero,
         -lammitystapa, -lammitysenergian_lahde, -esteeton, -aanestysaluenumero)
#POISTA KOHTEET JOISSA PURKAMISPAIVAMAARA != NA


# Reproject all spatial datasets to EPSG:4326
buil_sf <- st_transform(buil_sf, 4326)
yri_sf  <- st_transform(yri_sf, 4326)
roads <- st_transform(roads, 4326)


# =========================
# 2.GRAPH & GRAPH VERTICES
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

#Extract graph vertices from the main graph
#Each row in 'v' is one network vertex (node), including:
# - vertex ID
# - lat & lon coordinates (OBS! X/Y in the table!!)
# - component information
v <- dodgr_vertices(graph_main)
head(v)

# =========================
# 2.MATCH POINTS ok
# =========================

#Match buildings to nearest graph vertices
from <- match_points_to_verts(v, st_coordinates(buil_sf), connected = TRUE)
from <- v$id[from]

#Split companies by material
yri_wood <- yri_sf %>% filter(wood == "yes")
yri_conc <- yri_sf %>% filter(concrete == "yes")

# Match wood companies to nearest graph vertices
to_wood <- match_points_to_verts(v, st_coordinates(yri_wood), connected = TRUE)
to_wood <- v$id[to_wood]

# Match concrete companies to nearest graph vertices
to_conc <- match_points_to_verts(v, st_coordinates(yri_conc), connected = TRUE)
to_conc <- v$id[to_conc]


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

# NA indicates that no valid route exists between a building and a company.
sum(is.na(d_wood))

# Replace missing distances (NA) with Inf.
# NA means that no valid network route was found between a building and a company.
# Replacing NA with Inf makes it possible to safely search for the minimum distance
# without accidentally selecting a missing value.
d_wood[is.na(d_wood)] <- Inf

# For each building (each row of the matrix), identify the column index
# of the nearest reachable company.
# If all values in the row are Inf, then no company is reachable by network,
# and the result is set to NA.
nearest_wood <- apply(d_wood, 1, function(x) {
  if (all(is.infinite(x))) NA_integer_ else which.min(x)
})
# Prepare an output vector for the nearest network distance for each building.
# Start with NA for all buildings.
dist_wood <- NA_real_
# Logical vector indicating which buildings have at least one reachable company
valid_wood <- !is.na(nearest_wood)

# Extract the minimum network distance for each valid building.
# cbind(which(valid), nearest[valid]) creates matrix indices:
# - row index = building row in distance matrix
# - column index = nearest company column for that building
dist_wood[valid_wood] <- d_wood[
  cbind(which(valid_wood), nearest_wood[valid_wood])
]
# Convert distances from metres to kilometres
dist_wood_km <- dist_wood / 1000

head(dist_wood_km)
summary(dist_wood_km)

#################################
# CONCRETE
#################################
d_conc <- dodgr_dists(graph_main, from, to_conc)

d_conc[is.na(d_conc)] <- Inf

nearest_conc <- apply(d_conc, 1, function(x) {
  if (all(is.infinite(x))) NA_integer_ else which.min(x)
})

dist_conc <- NA_real_
valid_conc <- !is.na(nearest_conc)

dist_conc[valid_conc] <- d_conc[
  cbind(which(valid_conc), nearest_conc[valid_conc])
]

dist_conc_km <- dist_conc / 1000


# Ne joille ei löydy reittiä:
#bad_conc <- which(!valid_conc)
#length(bad_conc)
#head(bad_conc)
#mapview(roads) + mapview(buil_sf[bad_conc, ], col.regions = "red")




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

#In many cases the gross floor area is '0' and MI is therefore also 0 -> NA
# Some buildings are older than from 1910s

buil_sf <- buil_sf |>
  mutate(
    MI_wood = case_when(
      decade == 1910 ~ kerrosala * 152, #Added same value as 1940s
      decade == 1920 ~ kerrosala * 152, #Added same value as 1940s
      decade == 1930 ~ kerrosala * 152, #Added same value as 1940s
      decade == 1940 ~ kerrosala * 152,
      decade == 1950 ~ kerrosala * 48,
      decade == 1960 ~ kerrosala * 23,
      decade == 1970 ~ kerrosala * 53,
      decade == 1980 ~ kerrosala * 38,
      decade == 1990 ~ kerrosala * 33,
      decade == 2000 ~ kerrosala * 53,
      decade == 2010 ~ kerrosala * 53, #Added same value as 2000s
      decade == 2020 ~ kerrosala * 53  #Added same value as 2000s
    ),
    MI_wood = ifelse(MI_wood == 0, NA_real_, MI_wood) # If 0, change NA
  )



# MI COEFFICIENT for concrete
# gross floor area * MI (kg / m2)
buil_sf <- buil_sf |>
  mutate(
    MI_concrete = case_when(
      decade == 1910 ~ kerrosala * 758, #Added same value as 1940s
      decade == 1920 ~ kerrosala * 758, #Added same value as 1940s
      decade == 1930 ~ kerrosala * 758, #Added same value as 1940s
      decade == 1940 ~ kerrosala * 758,
      decade == 1950 ~ kerrosala * 818,
      decade == 1960 ~ kerrosala * 840,
      decade == 1970 ~ kerrosala * 244,
      decade == 1980 ~ kerrosala * 468,
      decade == 1990 ~ kerrosala * 361,
      decade == 2000 ~ kerrosala * 467,
      decade == 2010 ~ kerrosala * 467, #Added same value as 2000s
      decade == 2020 ~ kerrosala * 467  #Added same value as 2000s
    ),
    MI_concrete = ifelse(MI_concrete == 0, NA_real_, MI_concrete) # If 0, change NA
  )

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

summary(buil_sf$total_emissions)
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



# =========
# 8.SAVE THE DATA
# =========

#Change CRS to 4326 for MapLibre
#buil_polygons <- st_transform(buil_polygons, 4326)

#st_write(buil_polygons, "C:/Temp/codes/MINING_METHOD/frontend/data/buil_polygons.geojson", delete_dsn = TRUE)
