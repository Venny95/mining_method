#This code is for urban mining method
#At this stage, the method is developed for Joensuu area
library(sf)
library(dodgr)
library(dplyr)
library(mapview)

# Read datasets directly from GeoPackage files as sf objects

# Company dataset
yri_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/yritys1.gpkg")

# Joensuu building stock dataset, RYHTI data
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/RYHTI_rakennuksen_Joensuun_asemakaava_alue.gpkg")

# OpenStreetMap (OSM) road network dataset (downloaded from Geofabrik)
roads <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/OSM_roads_rajattu2.gpkg")



# Reproject all spatial datasets to WGS84 (EPSG:4326)
buil_sf <- st_transform(buil_sf, 3067)
yri_sf  <- st_transform(yri_sf, 3067)
roads <- st_transform(roads, 3067)

#At the moment, the MI coefficients calculation method is for wooden detached buildings only
#Filter buildings:
# -where structural material is wood
# -Where number of apartments is 1 or 2. This is because there is a lot of houses that type is more like detached house but there is two apartments
# -where type is "Pientalo" or "Kerrostalo". Kerrostalo is here because there is a lot of errors in data where detached house has mistake in type.

buil_sf <- buil_sf %>%
  filter(kantavien_rakenteiden_rakennusaine == "Puu",
         huoneistojen_lukumaara == 1 | huoneistojen_lukumaara == 2,
         paaasiallinen_kayttotarkoitus == "Pientalo"| paaasiallinen_kayttotarkoitus == "Kerrostalo",
         is.na(purkamispaivamaara)
         )


 ########## MI COEFFICIENTS ##############

# Date on building decade
# For MI COEFFICIENT a building decade is needed

#NOT NEEDED (later NAs will be filtered)?
# #Filtering dates that are incorrect format (not YYY-MM-DD) or NA
#buil_sf <- buil_sf |>
#  filter(!is.na(valmistumispaivamaara) |
#  grepl("^\\d{4}-\\d{2}-\\d{2}$", valmistumispaivamaara))

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



# Visual inspection of spatial layers
#mapview(buil_sf, col.regions = "orange") +
#  mapview(yri_sf, col.regions = "red") +
#  mapview(roads, col.regions = "steelblue")



# Convert geometries to LINESTRING format (required for routing/network analysis)
roads <- st_cast(roads, "LINESTRING")

# Count road segments classified as "unknown"
sum(roads$fclass == "unknown", na.rm = TRUE)
# Reclassify "unknown" road segments as "residential"
roads$fclass[roads$fclass == "unknown"] <- "residential"

# Build routing graph from street network using dodgr (weighted for motor vehicles)
graph <- weight_streetnet(roads, wt_profile = "motorcar", type_col = "fclass")

#Companies based on material
yri_wood <- yri_sf %>% filter(wood == "yes")
yri_conc <- yri_sf %>% filter(concrete == "yes")

# Extract coordinates as matrices (longitude, latitude) for dodgr
buil_mat <- st_coordinates(buil_sf)
yri_wood_mat <- st_coordinates(yri_wood)
yri_conc_mat <- st_coordinates(yri_conc)

# Compute shortest network distance matrix between buildings and companies
dmat_wood <- dodgr_dists(graph, from = buil_mat, to = yri_wood_mat)
dmat_conc <- dodgr_dists(graph, from = buil_mat, to = yri_conc_mat)

# Replace missing values (NA) with Inf
# NA indicates that no valid route exists between a building and a company.
dmat_wood[is.na(dmat_wood)] <- Inf #Prevents missing NA rows
dmat_conc[is.na(dmat_conc)] <- Inf



# For each building (row), find the index of the closest company based on minimum network distance.
nearest_wood <- apply(dmat_wood, 1, which.min)
nearest_wood <- unlist(nearest_wood)

nearest_conc <- apply(dmat_conc, 1, which.min)
nearest_conc <- unlist(nearest_conc)


#=====================================================
#Merge nearest company name and distances into buil_sf
##====================================================

# Extract distance to nearest wood company
buil_sf$dist_wood_km <- (dmat_wood[
  cbind(1:nrow(dmat_wood), nearest_wood)]) / 1000

buil_sf$dist_wood_km[is.infinite(buil_sf$dist_wood_km)] <- NA # Convert infinite values (unreachable routes) to NA for clarity

# Extract distance to nearest concrete company
buil_sf$dist_conc_km <- (dmat_conc[
  cbind(1:nrow(dmat_conc), nearest_conc)]) / 1000

buil_sf$dist_conc_km[is.infinite(buil_sf$dist_conc_km)] <- NA # Convert infinite values to NA

# Add company coordinates to buil_sf
# Company coordinates
wood_coords <- st_coordinates(yri_wood)
conc_coords <- st_coordinates(yri_conc)

# Add selected company coordinates to buil_sf (WOOD)
buil_sf$wood_company_lon <- wood_coords[nearest_wood, 1]
buil_sf$wood_company_lat <- wood_coords[nearest_wood, 2]

# Add selected company coordinates to buil_sf (CONCRETE)
buil_sf$concrete_company_lon <- conc_coords[nearest_conc, 1]
buil_sf$concrete_company_lat <- conc_coords[nearest_conc, 2]

# Add company names corresponding to nearest selection
buil_sf$company_wood <- yri_wood$Name[nearest_wood]
buil_sf$company_conc <- yri_conc$Name[nearest_conc]

head(buil_sf)


#===========================
# CO2 emissions of transport
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

# =================
# Building polygons
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
# Save the data
# =========

#Change CRS to 4326 for MapLibre
buil_polygons <- st_transform(buil_polygons, 4326)

st_write(buil_polygons, "C:/Temp/codes/MINING_METHOD/frontend/data/buil_polygons.geojson", delete_dsn = TRUE)

