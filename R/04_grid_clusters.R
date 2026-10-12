# rakennusten liittäminen ruutuihin
# materiaalimäärät aikajaksoittain
# korkean materiaalimäärän ruutujen valinta
# vierekkäisten ruutujen yhdistäminen klustereiksi
# klusterien tunnusluvut

# Ruudukko pitää tehdä EPSG 3067:ssa, koska ruudukko määritellään metreinä ja 3067 ymmärtää metrit
# EPSG:4326 taas sopii dodgr:iin ja MapLibreen

library(sf)
library(dplyr)
library(terra)
library(igraph)


# =========================
# 1. PARAMETERS
# =========================

# Grid-cell width and height in metres
grid_size_m <- 150

# Focal window width and height in grid cells
# Must be an odd number: 3, 5, 7, ...
focal_window_cells <- 3

# Preliminary threshold for identifying high-material cells
# 0.90 means cells in the highest 10% of non-zero material quantities
high_material_quantile <- 0.90

# =========================
# 2. READ BUILDING DATA
# =========================

buil <- st_read(
  "data/processed/buildings_material_stock_joensuu.gpkg",
  quiet = TRUE
) |>
  st_transform(3067)

# Check that the required material columns exist
stopifnot(
  "material_wood_t" %in% names(buil),
  "material_concrete_t" %in% names(buil)
)

cat("Buildings:", nrow(buil), "\n")
cat("CRS:", st_crs(buil)$input, "\n")

# =========================
# 3. CREATE GRID
# =========================

grid <- st_make_grid(
  buil,
  cellsize = grid_size_m,
  square = TRUE
) |>
  st_sf() |>
  mutate(
    grid_id = row_number()
  )

cat("All grid cells:", nrow(grid), "\n")

# =========================
# 4. ASSIGN BUILDINGS TO GRID
# =========================

# Add a unique row identifier for each building
buil <- buil |>
  mutate(
    building_row_id = row_number()
  )

# Assign buildings located strictly within grid cells
buil_grid <- st_join(
  buil,
  grid["grid_id"],
  join = st_within,
  left = TRUE
)

cat(
  "Buildings assigned with st_within:",
  sum(!is.na(buil_grid$grid_id)),
  "\n"
)

cat(
  "Buildings on grid boundaries:",
  sum(is.na(buil_grid$grid_id)),
  "\n"
)

# Identify buildings not assigned because they lie exactly
# on grid-cell boundaries
missing_grid <- which(is.na(buil_grid$grid_id))

if (length(missing_grid) > 0) {
  
  grid_centroids <- st_centroid(grid)
  
  nearest_grid <- st_nearest_feature(
    buil_grid[missing_grid, ],
    grid_centroids
  )
  
  buil_grid$grid_id[missing_grid] <-
    grid$grid_id[nearest_grid]
}

# Final checks
stopifnot(
  nrow(buil_grid) == nrow(buil),
  !any(is.na(buil_grid$grid_id)),
  !anyDuplicated(buil_grid$building_row_id)
)

cat(
  "Buildings assigned to grid:",
  nrow(buil_grid),
  "\n"
)
cat(
  "Buildings on grid boundaries:",
  sum(is.na(buil_grid$grid_id)),
  "\n"
)

# =========================
# 5. AGGREGATE MATERIALS BY GRID
# =========================

grid_summary <- buil_grid |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  summarise(
    building_count = n(),
    
    buildings_with_wood_data =
      sum(!is.na(material_wood_t)),
    
    buildings_with_concrete_data =
      sum(!is.na(material_concrete_t)),
    
    material_wood_t =
      sum(material_wood_t, na.rm = TRUE),
    
    material_concrete_t =
      sum(material_concrete_t, na.rm = TRUE),
    
    .groups = "drop"
  )

# Join calculated values back to grid geometries
grid_materials <- grid |>
  left_join(
    grid_summary,
    by = "grid_id"
  )

# Replace missing aggregate values with zero
grid_materials <- grid_materials |>
  mutate(
    building_count =
      coalesce(building_count, 0L),
    
    buildings_with_wood_data =
      coalesce(buildings_with_wood_data, 0L),
    
    buildings_with_concrete_data =
      coalesce(buildings_with_concrete_data, 0L),
    
    material_wood_t =
      coalesce(material_wood_t, 0),
    
    material_concrete_t =
      coalesce(material_concrete_t, 0)
  )

summary(grid_materials$material_wood_t)
summary(grid_materials$material_concrete_t)

# ==========================================
# 6. CREATE FOCAL MATERIAL CLUSTERS
# ==========================================

create_focal_material_clusters <- function(
    grid_data,
    material_column,
    material_name,
    focal_window = 3,
    threshold_quantile = 0.90
) {
  
  # Convert the material grid to raster
  grid_vector <- vect(grid_data)
  
  material_raster <- rasterize(
    grid_vector,
    rast(
      ext(grid_vector),
      resolution = grid_size_m,
      crs = crs(grid_vector)
    ),
    field = material_column,
    background = 0
  )
  
  # Calculate material quantity in the moving window
  focal_raster <- focal(
    material_raster,
    w = focal_window,
    fun = "sum",
    na.rm = TRUE,
    fillvalue = 0
  )
  
  names(focal_raster) <- "focal_material_t"
  
  # Extract focal values to the original grid
  focal_values <- extract(
    focal_raster,
    vect(
      st_point_on_surface(
        st_geometry(grid_data)
      )
    )
  )
  
  grid_focal <- grid_data |>
    mutate(
      focal_material_t =
        focal_values$focal_material_t
    )
  
  # Calculate the threshold from positive focal sums
  threshold <- quantile(
    grid_focal$focal_material_t[
      grid_focal$focal_material_t > 0
    ],
    probs = threshold_quantile,
    na.rm = TRUE,
    names = FALSE
  )
  
  # Select material-containing cells located in
  # high focal-material neighbourhoods
  high_cells <- grid_focal |>
    filter(
      focal_material_t >= threshold,
      .data[[material_column]] > 0
    )
  
  # Identify cells sharing a common edge
  adjacency <- st_relate(
    high_cells,
    high_cells,
    pattern = "F***1****",
    sparse = FALSE
  )
  
  # Form clusters from connected cells
  graph <- graph_from_adjacency_matrix(
    adjacency,
    mode = "undirected",
    diag = FALSE
  )
  
  high_cells$cluster_id <- paste0(
    material_name,
    "_",
    components(graph)$membership
  )
  
  # Combine adjacent cells into cluster polygons
  clusters <- high_cells |>
    group_by(cluster_id) |>
    summarise(
      material_type = material_name,
      
      grid_count = n(),
      
      building_count = sum(
        building_count,
        na.rm = TRUE
      ),
      
      material_t = sum(
        .data[[material_column]],
        na.rm = TRUE
      ),
      
      maximum_focal_material_t = max(
        focal_material_t,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  list(
    threshold = threshold,
    grid_focal = grid_focal,
    high_cells = high_cells,
    clusters = clusters
  )
}

# ===================
# 7. CLUSTERS FOR WOOD
# ===================

wood_results <- create_focal_material_clusters(
  grid_data = grid_materials,
  material_column = "material_wood_t",
  material_name = "wood",
  focal_window = focal_window_cells,
  threshold_quantile = high_material_quantile
)

# =========================
# 8. CLUSTERS FOR CONCRETE
# =========================

concrete_results <- create_focal_material_clusters(
  grid_data = grid_materials,
  material_column = "material_concrete_t",
  material_name = "concrete",
  focal_window = focal_window_cells,
  threshold_quantile = high_material_quantile
)



wood_results$clusters |>
  st_drop_geometry() |>
  arrange(desc(material_t)) |>
  print(n = Inf)

concrete_results$clusters |>
  st_drop_geometry() |>
  arrange(desc(material_t)) |>
  print(n = Inf)

table(wood_results$clusters$grid_count)
table(concrete_results$clusters$grid_count)

stopifnot(
  sum(wood_results$clusters$grid_count) ==
    nrow(wood_results$high_cells),
  
  sum(concrete_results$clusters$grid_count) ==
    nrow(concrete_results$high_cells)
)

wood_results$grid_focal
wood_results$high_cells
wood_results$clusters

concrete_results$grid_focal
concrete_results$high_cells
concrete_results$clusters

#Tarkastellaan kartalla

library(mapview)

palette <- function(n) {
  rev(hcl.colors(n, "YlOrRd"))
}

mapview(
  wood_results$high_cells,
  zcol = "material_wood_t",
  col.regions = palette,
  legend = TRUE,
  alpha.regions = 0.8,
  map.types = "CartoDB.Positron",
  layer.name = "Wood material (t)"
)



mapview(
  concrete_results$high_cells,
  zcol = "material_concrete_t",
  col.regions = palette,
  legend = TRUE,
  alpha.regions = 0.8,
  map.types = "CartoDB.Positron",
  layer.name = "Concrete material (t)"
)


