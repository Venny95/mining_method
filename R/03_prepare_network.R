# ==============================================================================
# 03_prepare_network.R
#
# Prepare the Joensuu OpenStreetMap road network for routing and accessibility
# analysis using the dodgr heavy goods vehicle (HGV) weighting profile.
#
# Input:
#   data/processed/OSM_roads_joensuu_small.gpkg
#   Road geometries and OSM road classes (fclass).
#
# Outputs:
#   data/processed/network_graph.rds
#   Weighted road network restricted to its largest connected component.
#
#   data/processed/network_vertices.rds
#   Vertices of that component, including IDs and WGS 84 coordinates.
#
# Run from the R/ directory.
# ==============================================================================

library(sf)
library(dodgr)

# ==============================================================================
# 1. Read and prepare road data
# ==============================================================================

# Local OpenStreetMap extract (Geofabrik).
roads <- st_read("data/processed/OSM_roads_joensuu_small.gpkg")
stopifnot("fclass" %in% names(roads))

# Convert road geometries to WGS 84 LINESTRING features.
roads <- roads |>
  st_transform(4326) |>
  st_cast("LINESTRING")

# Use the residential road class for segments labelled "unknown".
# This is a provisional classification assumption of the current workflow.
unknown_count <- sum(roads$fclass == "unknown", na.rm = TRUE)
roads$fclass[!is.na(roads$fclass) & roads$fclass == "unknown"] <- "residential"

cat("\nRoad network input:\n")
cat("Road features after geometry conversion:", nrow(roads), "\n")
cat("Road features reclassified from unknown:", unknown_count, "\n")
cat("Missing road classes:", sum(is.na(roads$fclass)), "\n")

# ==============================================================================
# 2. Build weighted network graph
# ==============================================================================

graph <- weight_streetnet(
  roads,
  wt_profile = "hgv",
  type_col = "fclass"
)

stopifnot("component" %in% names(graph))

# dodgr numbers connected components by decreasing size (1 = largest).
graph_main <- graph[graph$component == 1, ]
stopifnot(nrow(graph_main) > 0)

cat("\nWeighted network graph:\n")
cat("Total directed graph edges:", nrow(graph), "\n")
cat("Connected components:", length(unique(graph$component)), "\n")
cat("Main component edges:", nrow(graph_main), "\n")
cat("Main component share of edges:",
    round(100 * nrow(graph_main) / nrow(graph), 1), "%\n")

# ==============================================================================
# 3. Extract and check network vertices
# ==============================================================================

vertices <- dodgr_vertices(graph_main)
stopifnot(
  all(c("id", "x", "y") %in% names(vertices)),
  !anyNA(vertices$id),
  !anyDuplicated(vertices$id)
)

cat("Main component vertices:", nrow(vertices), "\n")

# ==============================================================================
# 4. Save processed network data
# ==============================================================================

saveRDS(graph_main, "data/processed/network_graph.rds")
saveRDS(vertices, "data/processed/network_vertices.rds")

cat("\nNetwork outputs saved to data/processed/.\n")

