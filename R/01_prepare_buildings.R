# 1. RYHTI-aineiston lukeminen
# 2. purettujen rakennusten poistaminen
# 3. rakennusvuosi ja vuosikymmen
# 4. material intensity -kertoimien yhdistäminen
# 5. rakennuskohtaiset materiaalitonnit
# 6. myöhemmin demolition_year ja demolition_period kunhan purkuennuste saadaan
# 7. Save the data

library(sf)
library(dodgr)
library(dplyr)
library(mapview)


#========
# 1
#========

# Joensuu building stock dataset, RYHTI data
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/ryhti_buildings_joensuu.gpkg")

#=======
# 2
#=======

#Remove demolished buildings
buil_sf <- buil_sf %>%
  filter(is.na(purkamispaivamaara))

# Remove excess attribute fields
buil_sf <- buil_sf %>%
  select(-kulttuurihistoriallinen_merkittavyys, -suojeltu, -purkamispaivamaara, -kuntanumero,
         -lammitystapa, -lammitysenergian_lahde, -esteeton, -aanestysaluenumero)


#=======
# 3
#=======


# For MI COEFFICIENT a building decade is needed
# Building year for demo tool information pop-up

#Extracting the building decade (NAs untouched)
sum(is.na(buil_sf$valmistumispaivamaara))

buil_sf <- buil_sf |>
  mutate(
    building_decade = if_else(
      grepl("^\\d{4}", valmistumispaivamaara),
      as.numeric(substr(valmistumispaivamaara, 1, 3)) * 10,
      NA_real_
    )
  )

#Extracting the building year (NAs untouched)
buil_sf <- buil_sf |>
  mutate(
    building_year = if_else(
      grepl("^\\d{4}", valmistumispaivamaara),
      as.numeric(substr(valmistumispaivamaara, 1, 4)),
      NA_real_
    )
  )

# =========================
# 4.MATERIAL INTENSITY
# =========================

# MI COEFFICIENT for wood
# gross floor area * MI (kg / m2)

#In many cases the gross floor area is '0' and MI is therefore also 0 -> NA
# Some buildings are older than from 1910s

buil_sf <- buil_sf |>
  mutate(
    MI_wood_kg = case_when(
      building_decade == 1910 ~ kerrosala * 152, #Added same value as 1940s
      building_decade == 1920 ~ kerrosala * 152, #Added same value as 1940s
      building_decade == 1930 ~ kerrosala * 152, #Added same value as 1940s
      building_decade == 1940 ~ kerrosala * 152,
      building_decade == 1950 ~ kerrosala * 48,
      building_decade == 1960 ~ kerrosala * 23,
      building_decade == 1970 ~ kerrosala * 53,
      building_decade == 1980 ~ kerrosala * 38,
      building_decade == 1990 ~ kerrosala * 33,
      building_decade == 2000 ~ kerrosala * 53,
      building_decade == 2010 ~ kerrosala * 53, #Added same value as 2000s
      building_decade == 2020 ~ kerrosala * 53  #Added same value as 2000s
    ),
    MI_wood_kg = ifelse(MI_wood_kg == 0, NA_real_, MI_wood_kg) # If 0, change NA
  )



# MI COEFFICIENT for concrete
# gross floor area * MI (kg / m2)
buil_sf <- buil_sf |>
  mutate(
    MI_concrete_kg = case_when(
      building_decade == 1910 ~ kerrosala * 758, #Added same value as 1940s
      building_decade == 1920 ~ kerrosala * 758, #Added same value as 1940s
      building_decade == 1930 ~ kerrosala * 758, #Added same value as 1940s
      building_decade == 1940 ~ kerrosala * 758,
      building_decade == 1950 ~ kerrosala * 818,
      building_decade == 1960 ~ kerrosala * 840,
      building_decade == 1970 ~ kerrosala * 244,
      building_decade == 1980 ~ kerrosala * 468,
      building_decade == 1990 ~ kerrosala * 361,
      building_decade == 2000 ~ kerrosala * 467,
      building_decade == 2010 ~ kerrosala * 467, #Added same value as 2000s
      building_decade == 2020 ~ kerrosala * 467  #Added same value as 2000s
    ),
    MI_concrete_kg = ifelse(MI_concrete_kg == 0, NA_real_, MI_concrete_kg) # If 0, change NA
  )

# Convert material quantities from kg to tonnes
buil_sf <- buil_sf |>
  mutate(
    material_wood_t = MI_wood_kg / 1000,
    material_concrete_t = MI_concrete_kg / 1000
  )

#=======
# 7
#=======

# Use ETRS-TM35FIN for distance- and area-based analysis
buil_analysis <- buil_sf |>
  st_transform(3067)

st_write(
  buil_analysis,
  "data/processed/buildings_material_stock_joensuu.gpkg",
  delete_dsn = TRUE
)
