# tieverkon lukeminen
# tieluokkien korjaaminen
# weight_streetnet()
# pääkomponentin valinta
# verkon solmujen muodostaminen
# Tallennus


library(sf)
library(dodgr)
library(dplyr)

#=====
# 1
#=====
# OpenStreetMap (OSM) road network dataset (downloaded from Geofabrik)
roads <- st_read("C:/Temp/codes/MINING_METHOD/R/data/processed/OSM_roads_joensuu_small.gpkg")

# =========================
# 2
# =========================

# Convert geometries to LINESTRING format (required for routing/network analysis)
# dodgr network coordinates
roads <- roads |>
  st_transform(4326) |>
  st_cast("LINESTRING")

cat("Edges:", nrow(roads), "edges\n")

# Count road segments classified as "unknown"
sum(roads$fclass == "unknown", na.rm = TRUE)
# Reclassify "unknown" road segments as "residential"
roads$fclass[roads$fclass == "unknown"] <- "residential"

#============
# 3
#=============

# Build routing graph from street network using dodgr (weighted for motor vehicles)
graph <- weight_streetnet(roads, wt_profile = "hgv", type_col = "fclass")

#=========
# 4
#=========

# Choose only main component
graph_main <- graph [graph$component == 1, ]
cat("Main component edges:", nrow(graph_main), "edges\n")  

#======
# 5
#======

#Extract graph vertices from the main graph
#Each row in 'v' is one network vertex (node), including:
# - vertex ID
# - lat & lon coordinates (OBS! X/Y in the table!!)
# - component information
v <- dodgr_vertices(graph_main)
head(v)

#======
# 6
#======

saveRDS(
  graph_main,
  "data/processed/network_graph.rds"
)

saveRDS(
  v,
  "data/processed/network_vertices.rds"
)
