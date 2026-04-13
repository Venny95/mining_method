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

#Luetaan aineistot tietokannasta suoraan sf-muotoon
yri_sf <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/yritys1.gpkg")

#Read Joensuu area buildings
buil_sf <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/Joensuu_kantakaupungin_asemakaavoitettu_alue_Joensuun_kaupunki_20251029.gpkg")

#Read OSM data
roads <- st_read("C:/Temp/codes/MINING_METHOD/data/processed/OSM_roads_rajattu2.gpkg")


#Dodgr-paketille täytyy olla 4326
buil_sf <- st_transform(buil_sf, 4326)
yri_sf  <- st_transform(yri_sf, 4326)
roads <- st_transform(roads, 4326)
mapview(buil_sf, col.regions = "orange") +
  mapview(yri_sf, col.regions = "red") +
  mapview(roads, col.regions = "steelblue")

#Otetaan rakennuksista näyte, jossa 500 kohdetta
buil_sf <- buil_sf %>%
  slice_sample(n = 500)



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


names(roads)


roads <- st_cast(roads, "LINESTRING")

#Aineistossa on tiepätkiä, jotka luokiteltu "unknown"
roads$fclass[roads$fclass == "unknown"] <- "residential"

#Painota verkko reititystä varten (luo dodgr-graafin)
graph <- weight_streetnet(roads, wt_profile = "motorcar", type_col = "fclass")

# Koordinaatit matriisina (lon, lat)
buil_mat <- st_coordinates(buil_sf)
yri_mat  <- st_coordinates(yri_sf)

# dmat: matriisi [n_buil x n_yri], arvot metreinä (tai profiilin muk. painotus)
dmat <- dodgr_dists(graph, from = buil_mat, to = yri_mat)
summary(dmat)

#Etsi lähin yritys kustakin rakennuksesta (myöhemmin tätä on muokattava)
nearest <- apply(dmat, 1, function(x) which.min(x))
nearest <- unlist(nearest)

#Hae reitit (vain valittuun lähimpään kohteeseen)
paths <- dodgr_paths(graph,
                     from = buil_mat,
                     to   = yri_mat[nearest, , drop = FALSE],
                     vertices = TRUE)


exists("paths")
length(paths)
str(paths)

#Muunna reittiverteksistä LINESTRINGit

#Haetaan verkon solmut
verts <- dodgr_vertices(graph)


make_line <- function(v) {  #Solmuista viivat
  xy <- verts[match(v, verts$id), c("x", "y")]   #Etsitään solmujen koordinaatit
  st_linestring(as.matrix(xy)) #Tehdään linestring
}

#Käytetään yllä olevaa finktiota kaikkiin reitteihin
route_lines <- lapply(paths, function(p) make_line(p[[1]]))

#Yhdistetään viivat sf-objektiksi
routes_sfc  <- st_sfc(route_lines, crs = 4326)

# sf-viivataso
routes_sf <- st_sf(
  buil_id = buil_sf$id,
  nearest_yri = nearest,
  length_m = dmat[cbind(seq_len(nrow(buil_sf)), nearest)],
  geometry = routes_sfc
)


mapview(roads, color = "steelblue", lwd = 3)+
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
