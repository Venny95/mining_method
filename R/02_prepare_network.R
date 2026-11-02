# ==============================================================================
# Prepare road network for routing analysis
#
# Prepares the Joensuu OpenStreetMap road network for routing and accessibility
# analysis. Creates a weighted network graph for heavy goods vehicles (HGV)
# and extracts the vertices of the main connected network component.
#
# Input:
#   data/processed/OSM_roads_joensuu_small.gpkg
#   OpenStreetMap road network dataset containing road geometries and road
#   classes (fclass) used for network weighting.
#
# Outputs:
#   data/processed/network_graph.rds
#   Weighted HGV road network containing the main connected component.
#
#   data/processed/network_vertices.rds
#   Network vertices with unique IDs and geographic coordinates.
# ==============================================================================


library(sf)
library(dodgr)
library(dplyr)

# ==============================================================================
# 1. Read road network data
# ==============================================================================

# OpenStreetMap road network dataset downloaded from Geofabrik
roads <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/OSM_roads_joensuu_small.gpkg")

# ==============================================================================
# 2. Prepare road network geometries and classes
# ==============================================================================

# Transform to WGS 84 and convert geometries to LINESTRING format
# for routing and network analysis
roads <- roads |>
  st_transform(4326) |>
  st_cast("LINESTRING")

# Check the number of road segments
cat("Edges:", nrow(roads), "edges\n")

# Check the number of road segments classified as "unknown"
sum(roads$fclass == "unknown", na.rm = TRUE)

# Reclassify "unknown" road segments as "residential"
roads$fclass[roads$fclass == "unknown"] <- "residential"

# ==============================================================================
# 3. Create weighted road network
# ==============================================================================

# Build routing graph using the dodgr HGV weighting profile (weighted for motor vehicles)
graph <- weight_streetnet(
  roads,
  wt_profile = "hgv",
  type_col = "fclass"
)

# ==============================================================================
# 4. Select main connected component
# ==============================================================================

# Retain the main connected component of the road network
graph_main <- graph [graph$component == 1, ]

# Check the number of edges in the main component
cat("Main component edges:", nrow(graph_main), "edges\n")  

# ==============================================================================
# 5. Extract network vertices
# ==============================================================================

# Extract vertices from the main network component
# Each row represents one network vertex with its ID and coordinates
v <- dodgr_vertices(graph_main)

head(v)

# ==============================================================================
# 6. Save processed network data
# ==============================================================================

saveRDS(
  graph_main,
  "data/processed/network_graph.rds"
)

saveRDS(
  v,
  "data/processed/network_vertices.rds"
)
