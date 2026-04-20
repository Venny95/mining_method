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

#At the moment, the MI coefficients calculation method is for residential detached buildings only and for WOOD only
#Filter detached houses from buildings where structural material is wood
#OBS! "Pientalo" means slightly different than detached house.
names(buil_sf)
head(buil_sf)
buil_sf <- buil_sf %>%
  filter(kantavien_rakenteiden_rakennusaine == "Puu",
         paaasiallinen_kayttotarkoitus == "Pientalo",
         is.na(purkamispaivamaara)
         )

head(buil_sf)


 ########## MI COEFFICIENTS ##############

# Date on building decade
# For MI COEFFICIENT a building decade is needed
# #Filtering dates that are incorrect format (not YYY-MM-DD) or NA
buil_sf <- buil_sf |>
  filter(!is.na(valmistumispaivamaara) |
  grepl("^\\d{4}-\\d{2}-\\d{2}$", valmistumispaivamaara))

#Extracting the building decade (NAs untouched)

buil_sf <- buil_sf |>
  mutate(
    decade = if_else(
      is.na(valmistumispaivamaara),
      NA_character_,
      paste0(substr(valmistumispaivamaara, 1, 3), "0")
    )
  )



# MI COEFFICIENT for wood
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
  ) |>
  filter(!is.na(MI_wood))

head(buil_sf$MI_wood)



# MI COEFFICIENT for concrete
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
  ) |>
  filter(!is.na(MI_concrete))

head(buil_sf$MI_concrete)
head(buil_sf)




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
dmat_wood[is.na(dmat_wood)] <- Inf#Prevents missing NA rows
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
buil_sf$dist_wood <- dmat_wood[
  cbind(1:nrow(dmat_wood), nearest_wood)
]

buil_sf$dist_wood[is.infinite(buil_sf$dist_wood)] <- NA # Convert infinite values (unreachable routes) to NA for clarity

# Extract distance to nearest concrete company
buil_sf$dist_conc <- dmat_conc[
  cbind(1:nrow(dmat_conc), nearest_conc)
]
buil_sf$dist_conc[is.infinite(buil_sf$dist_conc)] <- NA # Convert infinite values to NA

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

# =========
#Statistics
# =========

# Overall distribution summaries
summary(buil_sf$dist_wood)
summary(buil_sf$dist_conc)

# Mean distances (excluding NA values)
mean(buil_sf$dist_wood, na.rm = TRUE)
mean(buil_sf$dist_conc, na.rm = TRUE)

# Quantiles
quantile(buil_sf$dist_wood, c(0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
quantile(buil_sf$dist_conc, c(0.5, 0.75, 0.9, 0.95), na.rm = TRUE)

