#This code is for urban mining method
#At this stage, the method is developed for Joensuu area

library(dodgr)
library(DBI)
library(RPostgres)
library(sf)
library(dplyr)
library(osmdata)
library(ggspatial)
library(tidyverse)
library(pacman, qiscoR)
library(mapview)
library(sfnetworks)

#file.edit("~/.Renviron")
#dbDisconnect(con)

#Avataan tietokantayhteys (.Renviron)
con <- dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD"),
  sslmode  = Sys.getenv("PG_SSLMODE") # esim. "require" tuotannossa
)

# Tarkistetaan yhteys
dbIsValid(con)
dbGetQuery(con, "SELECT current_database(), current_user, current_timestamp;")
dbListTables(con)

#Luetaan aineistot tietokannasta suoraan sf-muotoon
buil_sf <- st_read(con, query = "SELECT * FROM public.joensuun_rakennukset")
yri_sf <- st_read(con, query = "SELECT * FROM public.yritys")


#Dodgr-paketille täytyy olla 4326
buil_sf <- st_transform(buil_sf, 4326)
yri_sf  <- st_transform(yri_sf, 4326)
mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf, col.regions = "red")

#Otetaan rakennuksista näyte, jossa 20 kohdetta
buil_sf <- buil_sf %>%
  slice_sample(n = 20)

#OSM-datan tietojen tarkastus:
#available_features()
#available_tags("highway")

#BBoxin luominen rakennusten perusteella ei toimi oikein toimi oikein:
#bbox <- st_bbox(st_union(satun_buil, yri_sf))
#Joten käytetään osmdata-paketin kaupunkitoimintoa:
kysely <- opq("Joensuu") |>
  add_osm_feature(key ="highway", value= c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential"))
streetnet <- osmdata_sf(kysely)
streets <- streetnet$osm_lines
#class(streets)

#Plotataan tarkastelua varten
mapview(streets, color = "steelblue", lwd = 3) +
  mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf, col.regions = "red")

#Lisätään verteksi rakennuksen lähelle:





#1) Muunna tieaineisto sfnetworkiksi
#net <- as_sfnetwork(streets, directed = FALSE)

# 2) Muodosta pisteaineisto vain geometryllä (varmin tapa)
#b_pts <- st_as_sf(st_geometry(buil_sf))

# 3) Snäppää pisteet tielle ja lisää solmut tieverkkoon
#net_blended <- st_network_blend(net, b_pts)

# 4) Poimi blended‑solmut sf‑tasoksi (tässä ovat nyt uudet verteksit!)
#graph <- net_blended %>%
#  activate("nodes") %>%
#  st_as_sf()















#Painota verkko reititystä varten (luo dodgr-graafin)
graph <- weight_streetnet(streets, wt_profile = "motorcar")

# Koordinaatit matriisina (lon, lat)
buil_mat <- st_coordinates(buil_sf)
yri_mat  <- st_coordinates(yri_sf)

# dmat: matriisi [n_buil x n_yri], arvot metreinä (tai profiilin muk. painotus)
dmat <- dodgr::dodgr_dists(graph, from = buil_mat, to = yri_mat)


#Etsi lähin yritys kustakin rakennuksesta (myöhemmin tätä on muokattava)
nearest <- apply(dmat, 1, which.min)

#Hae reitit (vain valittuun lähimpään kohteeseen)
paths <- dodgr_paths(graph,
                     from = buil_mat,
                     to   = yri_mat[nearest, , drop = FALSE],
                     vertices = TRUE)

#Muunna reittiverteksistä LINESTRINGit
verts <- dodgr_vertices(graph)

make_line <- function(v) {
  xy <- verts[match(v, verts$id), c("x", "y")]
  st_linestring(as.matrix(xy))
}

route_lines <- lapply(paths, function(p) make_line(p[[1]]))
routes_sfc  <- st_sfc(route_lines, crs = 4326)

# sf-viivataso
routes_sf <- st_sf(
  buil_id = buil_sf$id,
  nearest_yri = nearest,
  length_m = dmat[cbind(seq_len(nrow(buil_sf)), nearest)],
  geometry = routes_sfc
)


mapview(streets, color = "steelblue", lwd = 3)+
  mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf,  col.regions = "red") +
  mapview(routes_sf, color = "black", lwd = 3)
  

#Tehdään testiaineisto, jolla voidaan testata MapLibren toimintaa
#Poistetaan muuttujia
names(buil_sf)
testidata <- buil_sf %>% 
  select(id)

names(routes_sf)
testitiet <- routes_sf %>% 
  select(buil_id)


# Varmista geometriakentän nimi, jos haluat nimen "geom"
# (oletusnimi säilyy, ellet muuta)
# st_geometry(buil) <- "geom"  # vain jos haluat nimetä sen nimenomaan "geom"

st_write(
  obj    = testidata,                                 # sf-objekti
  dsn    = con,                                       # DBI-yhteys
  layer  = Id(schema = "public", table = "rakennukset_testi"),     # kohdeschema+taulu
  layer_options = c("FID=gid"),                       # valinnainen: pk-kentän nimi
  delete_layer = TRUE                                 # ylikirjoita olemassa oleva taulu
)

st_write(
  obj    = testitiet,                                 # sf-objekti
  dsn    = con,                                       # DBI-yhteys
  layer  = Id(schema = "public", table = "tiet_testi"),     # kohdeschema+taulu
  layer_options = c("FID=gid"),                       # valinnainen: pk-kentän nimi
  delete_layer = TRUE                                 # ylikirjoita olemassa oleva taulu
)
