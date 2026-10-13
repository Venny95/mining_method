# rakennusten liittäminen ruutuihin
# materiaalimäärät aikajaksoittain
# korkean materiaalimäärän ruutujen valinta
# vierekkäisten ruutujen yhdistäminen klustereiksi
# klusterien tunnusluvut

# Ruudukko pitää tehdä EPSG 3067:ssa, koska ruudukko määritellään metreinä ja 3067 ymmärtää metrit
# EPSG:4326 taas sopii dodgr:iin ja MapLibreen

# modifiable areal unit problem (MAUP), sensitivity analysis needed?

#lisää lopulliseen klusteritaulukkoon absoluuttiseen kokonaismäärään perustuvat skenaarioluokitukset
#anna yhden ruudun klusterin läpäistä sama absoluuttinen klusteriraja
#testaa ruutukoot ja focal-ikkunat niin, että pystyt erottamaan ruututarkkuuden vaikutuksen naapuruston fyysisen koon vaikutuksesta
#testaa myös ruudukon siirtoa, koska rajojen sijainti voi vaikuttaa klustereihin

library(sf)
library(dplyr)
library(terra)
library(igraph)


# =========================
# 1. PARAMETERS
# =========================

# Grid-cell width and height in metres
grid_size_m <- 100

# Focal window width and height in grid cells
# Must be an odd number: 3, 5, 7, ...
focal_window_cells <- 3

# Preliminary relative threshold for identifying high focal sums
# 0.90 selects cells in the highest 10% of positive focal material sums
high_material_quantile <- 0.90

# Preliminary absolute material-quantity scenarios in tonnes
wood_cluster_thresholds <- c(
  low = 500,
  medium = 2000,
  high = 5000
)

concrete_cluster_thresholds <- c(
  low = 5000,
  medium = 20000,
  high = 50000
)

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
    cluster_thresholds,
    focal_window = 3,
    threshold_quantile = 0.90
) {
  
  # Convert material quantities from grid polygons to raster
  grid_vector <- terra::vect(grid_data)
  
  material_raster <- terra::rasterize(
    grid_vector,
    terra::rast(
      terra::ext(grid_vector),
      resolution = grid_size_m,
      crs = terra::crs(grid_vector)
    ),
    field = material_column,
    background = 0
  )
  
  # Calculate material quantity in each moving neighbourhood
  focal_raster <- terra::focal(
    material_raster,
    w = focal_window,
    fun = "sum",
    na.rm = TRUE,
    fillvalue = 0
  )
  
  names(focal_raster) <- "focal_material_t"
  
  # Add focal sums to the original grid
  focal_values <- terra::extract(
    focal_raster,
    terra::vect(
      st_point_on_surface(
        st_geometry(grid_data)
      )
    )
  )
  
  grid_focal <- grid_data |>
    mutate(
      focal_material_t = focal_values$focal_material_t
    )
  
  # Calculate the relative threshold from positive focal sums
  threshold <- quantile(
    grid_focal$focal_material_t[
      grid_focal$focal_material_t > 0
    ],
    probs = threshold_quantile,
    na.rm = TRUE,
    names = FALSE
  )
  
  # Select material-containing cells in high-material neighbourhoods
  high_cells <- grid_focal |>
    filter(
      focal_material_t >= threshold,
      .data[[material_column]] > 0
    )
  
  # Form clusters from cells sharing a common edge
  adjacency <- st_relate(
    high_cells,
    high_cells,
    pattern = "F***1****",
    sparse = FALSE
  )
  
  graph <- igraph::graph_from_adjacency_matrix(
    adjacency,
    mode = "undirected",
    diag = FALSE
  )
  
  high_cells$cluster_id <- paste0(
    material_name,
    "_",
    igraph::components(graph)$membership
  )
  
  # Calculate cluster statistics and assign material scenarios
  clusters <- high_cells |>
    group_by(cluster_id) |>
    summarise(
      material_type = material_name,
      grid_count = n(),
      building_count = sum(building_count, na.rm = TRUE),
      material_t = sum(
        .data[[material_column]],
        na.rm = TRUE
      ),
      maximum_focal_material_t = max(
        focal_material_t,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(
      material_scenario = case_when(
        material_t >= cluster_thresholds["high"] ~ "high",
        material_t >= cluster_thresholds["medium"] ~ "medium",
        material_t >= cluster_thresholds["low"] ~ "low",
        TRUE ~ "below threshold"
      ),
      material_scenario = factor(
        material_scenario,
        levels = c(
          "below threshold",
          "low",
          "medium",
          "high"
        ),
        ordered = TRUE
      )
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
  cluster_thresholds = wood_cluster_thresholds,
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
  cluster_thresholds = concrete_cluster_thresholds,
  focal_window = focal_window_cells,
  threshold_quantile = high_material_quantile
)



wood_results$clusters |>
  st_drop_geometry() |>
  select(
    cluster_id,
    grid_count,
    material_t,
    material_scenario
  ) |>
  arrange(desc(material_t))

concrete_results$clusters |>
  st_drop_geometry() |>
  select(
    cluster_id,
    grid_count,
    material_t,
    material_scenario
  ) |>
  arrange(desc(material_t))

table(wood_results$clusters$material_scenario)
table(concrete_results$clusters$material_scenario)




#Tarkastellaan kartalla

library(mapview)

scenario_palette <- c(
  "below threshold" = "#D9D9D9",
  "low" = "#FFFFB2",
  "medium" = "#FD8D3C",
  "high" = "#BD0026"
)

mapview(
  wood_results$clusters,
  zcol = "material_scenario",
  col.regions = scenario_palette,
  color = "grey30",
  lwd = 1,
  alpha.regions = 0.8,
  legend = TRUE,
  map.types = "CartoDB.Positron",
  layer.name = "Wood material scenario"
)


mapview(
  concrete_results$clusters,
  zcol = "material_scenario",
  col.regions = scenario_palette,
  color = "grey30",
  lwd = 1,
  alpha.regions = 0.8,
  legend = TRUE,
  map.types = "CartoDB.Positron",
  layer.name = "Concrete material scenario"
)

