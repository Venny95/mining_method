# ==============================================================================
# Prepare building-level material stock data
#
# Prepares the Joensuu RYHTI building dataset for material mining analysis.
# Estimates wood and concrete quantities using decade-specific material
# intensity (MI) coefficients and gross floor area.
#
# Material intensity coefficients are based on:
# Nasiri, F., Kaasalainen, T., Hughes, M. (2023).
# Estimating the material intensity of wooden residential houses in Finland.
# Resources, Conservation and Recycling, 198, 107142.
#
# Input:
#   data/processed/ryhti_buildings_joensuu.gpkg
#   Building-level spatial dataset containing completion date, gross floor area,
#   demolition information, and building geometry.
#
# Output:
#   data/processed/buildings_material_stock_joensuu.gpkg
#   Building-level spatial dataset with estimated wood and concrete quantities,
#   construction year and decade, in ETRS-TM35FIN (EPSG:3067).
# ==============================================================================

library(sf)
library(dplyr)


# ==============================================================================
# 1. Read building stock data
# ==============================================================================

# Joensuu building stock dataset (RYHTI)
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/ryhti_buildings_joensuu.gpkg")

# ==============================================================================
# 2. Filter and clean building data
# ==============================================================================

# Remove buildings recorded as demolished
buil_sf <- buil_sf %>%
  filter(is.na(purkamispaivamaara))

# Remove excess attribute fields
buil_sf <- buil_sf %>%
  select(-kulttuurihistoriallinen_merkittavyys, -suojeltu, -purkamispaivamaara, -kuntanumero,
         -lammitystapa, -lammitysenergian_lahde, -esteeton, -aanestysaluenumero)


# ==============================================================================
# 3. Derive construction year and decade
# ==============================================================================

# Building decade is required for assigning material intensity coefficients.
# Building year is retained for building-level information and visualization.

# Check the number of missing completion dates
sum(is.na(buil_sf$valmistumispaivamaara))

# Extract construction decade; missing or invalid dates remain NA
buil_sf <- buil_sf |>
  mutate(
    building_decade = if_else(
      grepl("^\\d{4}", valmistumispaivamaara),
      as.numeric(substr(valmistumispaivamaara, 1, 3)) * 10,
      NA_real_
    )
  )

# Extract construction year; missing or invalid dates remain NA
buil_sf <- buil_sf |>
  mutate(
    building_year = if_else(
      grepl("^\\d{4}", valmistumispaivamaara),
      as.numeric(substr(valmistumispaivamaara, 1, 4)),
      NA_real_
    )
  )

# ==============================================================================
# 4. Estimate building-level material quantities
# ==============================================================================

# Material quantities are estimated by multiplying gross floor area
# (kerrosala) by decade-specific material intensity coefficients (kg/m2).
#
# For decades without separate MI coefficients, coefficients from the nearest
# available reference decade are used as specified below.

# In many cases the gross floor area is '0' and MI is therefore also 0 -> NA
# Some buildings are older than from 1910s

# Wood
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



# Concrete

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

# ==============================================================================
# 5. Convert material quantities to tonnes
# ==============================================================================

buil_sf <- buil_sf |>
  mutate(
    material_wood_t = MI_wood_kg / 1000,
    material_concrete_t = MI_concrete_kg / 1000
  )

# ==============================================================================
# 6. Transform coordinate reference system
# ==============================================================================

# Use ETRS-TM35FIN for distance- and area-based analysis
buil_analysis <- buil_sf |>
  st_transform(3067)

# ==============================================================================
# 7. Save processed data
# ==============================================================================

st_write(
  buil_analysis,
  "data/processed/buildings_material_stock_joensuu.gpkg",
  delete_dsn = TRUE
)
