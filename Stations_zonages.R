#############################
# création des zones ratp puis calcul des indicateurs

# référentiel stations

# json
library(jsonlite)
library(janitor)
library(dplyr)
library(tidyr)

# source https://www.data.gouv.fr/fr/datasets/stations-et-gares-de-metro-rer-et-tramway-de-la-region-ile-de-france/
src <- "http://api.openstreetmap.fr/oapi/interpreter?data=[out:json];node[%22type:RATP%22~%22metro|rer|tram%22];out;way[%22type:RATP%22~%22metro|rer|tram%22];out;%3E;out%20skel;"

json_stations <- jsonlite::fromJSON(src)

REF_stations <- 
  as.data.frame(json_stations$elements) %>%
  select(id, lat,lon) %>%
  cbind(as.data.frame(json_stations$elements$tags) %>%
          clean_names() %>%
          select(name,station, type_ratp,wikipedia)) %>%
  mutate(nom_station = gsub("\\s*\\([^\\)]+\\)", "", name, perl=TRUE)) %>%
  distinct(name,.keep_all = TRUE) %>%
  as.data.frame()

# récuparation des coordonnées des stations non renseignées
library(rvest)

REF_stations_ageo <-
  REF_stations %>%
  filter(is.na(lat)) %>%
  mutate(wikiurl = paste0("http://fr.wikipedia.org/wiki/",wikipedia)) %>%
  mutate(wikiurl = gsub(" ", "_", wikiurl, perl=TRUE)) 

# liste des url à traiter
REF_stations_ageoo <- as.vector(REF_stations_ageo$wikiurl)

#récupération des coordonnées
ExtractGeoCoords <- function(url) {
  url %>%
    read_html() %>%
    html_nodes("#coordinates") %>%
    html_text()
}

# boucle sur tous les liens
geos.coords <- sapply(1:length(REF_stations_ageoo), function(x) try(ExtractGeoCoords(REF_stations_ageoo[x]), silent = T))

# nettoyage des coordonnées
geos.clean.coords <- sapply(1:length(geos.coords), function(x) geos.coords[[x]][1])

REF_stations_ageo2 <- REF_stations_ageo %>% cbind(geos.clean.coords) %>%
  mutate(geos.clean.coords = gsub("nord","N", geos.clean.coords),
         geos.clean.coords = gsub("sud","S", geos.clean.coords),
         geos.clean.coords = gsub("est","E", geos.clean.coords),
         geos.clean.coords = gsub("ouest","O", geos.clean.coords)) %>%
  separate(geos.clean.coords, c("geos.clean.lat", "geos.clean.long"), ",") 
  

# conversion des coordonnées en WGS 84 decimales
library(sp)

REF_stations_ageoo <-
  REF_stations_ageo2 %>% 
  as.data.frame() %>%
  filter(!is.na(geos.clean.lat))

cleanlatlong <- function(x) {
  x1 <- as.character(x)
  x2 <- sub('°', 'd', x1)
  x3 <- gsub("\\s", "", x2)
  x4 <- sub("′", "'", x3)
  x5 <- sub('″', '\" ', x4)
  x6 <- as.numeric(char2dms(x5))
  print(x6)
}

REF_stations_ageoo <- REF_stations_ageoo %>%
  mutate(lat.wiki = cleanlatlong(geos.clean.lat),
         lon.wiki = cleanlatlong(geos.clean.long)) 

# référentiel exhaustif des stations et info en plus sur une station non renseignée
REF_stations_full <- REF_stations %>%
  left_join(REF_stations_ageoo %>%
              dplyr::select(id, lat.wiki, lon.wiki) ,
by = c("id" = "id")) %>%
  mutate(lat = ifelse(is.na(lat), lat.wiki, lat),
         lon = ifelse(is.na(lon), lon.wiki, lon)) %>%
  mutate(lat = ifelse(id %in% '417349794', 48.725036, lat),
         lon = ifelse(id %in% '417349794', 2.259882, lon) ) %>%
  dplyr::select(-c(lat.wiki, lon.wiki)) %>%
  mutate(nom_station = ifelse(id %in% '80405300','Antony' ,nom_station)) %>%
  select(id, lat, lon, nom_station)

#  ajout ref transilien
ref_transilien <- read.csv2("https://ressources.data.sncf.com/explore/dataset/osm-mapping-idf/download?format=csv&timezone=Europe/Berlin&use_labels_for_header=true") %>%
  filter(!wikipedia %in% '') %>%
  distinct(Relation.ref.FR.uic8,.keep_all  = TRUE) %>%
  separate(Geo.Point, into = c("lat", "lon"), sep = "\\,") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) 

# réferentiel ratp + sncf exhaustif
REF_stations_fulll <- 
  REF_stations_full %>%
  select(id, nom_station, lat, lon) %>%
  rbind(ref_transilien %>%
          as.data.frame() %>%
          select(id, gare, lat , lon) %>%
          rename(nom_station = gare)) %>%
  distinct(id,.keep_all = TRUE) %>%
  distinct(nom_station,.keep_all = TRUE)
  
##############
# nettoyage du référentiel (principalement élimination des doublons) 

library(spdplyr)

REF_stations.geo <- SpatialPointsDataFrame(coords = subset(REF_stations_fulll, select = c(lon, lat)),
                                           data = REF_stations_fulll,
                                           proj4string = CRS("+init=epsg:4326"))    
REF_stations.geo <- spTransform(REF_stations.geo, CRS("+init=epsg:2154"))  %>% mutate(id = as.character(id))

# exclusion des stations trop proches 

mat_dist <- gDistance(REF_stations.geo, byid=T)
min.dist <- apply(mat_dist, 1, function(x) order(x, decreasing=F)[2])
stations_proches <- cbind(REF_stations_fulll, REF_stations_fulll[min.dist,], apply(mat_dist, 1, function(x) sort(x, decreasing=F)[2]))
View(stations_proches)

# on les élimine à la mano pour conserver le meilleur libellé de station

REF_stations.geo <- REF_stations.geo %>%
  filter(!id %in% c('292422757','3533463459','4069972808','80481421','3497428293', '3441085434','70166455', '80405300','2354904582','1308998006', '255687197','2799009872','1309031698','3146958062','1731763794','3268094343','3190767995','2489972624','1731763792','329974472','3574677130','264508125','329974475','3268094344','2799009836','267619085','270191809','423570860','1088638583','27371889', '3414068431','417844901', '3491814695','3497428295', '3419908160','319204791','3493829093','145097439','417349794','3497428793'))

# voronoi
library(deldir)
voronoipolygons = function(layer) {
  crds = layer@coords
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                         y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                      function(x) slot(x, 'ID'))))
}

# récupération des infos IRIS
REF_stations.geoo <- over(REF_stations.geo, irisnew.sp)

REF_stations.geo@data <- REF_stations.geo@data %>% cbind(REF_stations.geoo %>% dplyr::select(INSEE_COM, NOM_COM))

REF_stations.Z <- voronoipolygons(REF_stations.geo )
proj4string(REF_stations.Z) <- CRS("+init=epsg:2154")
REF_stations.Z <- gBuffer(REF_stations.Z, byid=TRUE, width=0)

REF_stations.Z@data <-cbind(REF_stations.Z@data,REF_stations.geo@data) 
REF_stations.Z <- REF_stations.Z %>% dplyr::select(-c(x,y))


#buffer sur 700 metres
REF_stations.zt <- gBuffer(REF_stations.geo, width = 700, byid = F)
REF_stations.ZT <- raster::crop(REF_stations.Z,REF_stations.zt)
REF_stations.ZT.wgs84 <- spTransform(REF_stations.ZT, CRS("+init=epsg:4326"))

###########
# passage IRIS
#sf pour gagner en rapidité
REF_stations.ZT.sf <- st_as_sf(REF_stations.ZT)

# table de correspondance
intersect.iris.sta <- st_intersection(irisnew.IDF.sf,REF_stations.ZT.sf )

passage_IRIS.sta <- intersect.iris.sta %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>%
  dplyr::select(id, CODE_IRIS, area) %>%
  mutate(id = as.character(id), CODE_IRIS = as.character(CODE_IRIS)) %>%
  group_by(CODE_IRIS, id ) %>%
  summarise (area.intersect = sum(area)) %>%
  mutate(ratio_id_IRISnew = area.intersect / sum(area.intersect))

# table des variables par station
STATIONS_data <-
  passage_IRIS.sta %>%
  as.data.frame() %>%
  dplyr::select(id, CODE_IRIS, ratio_id_IRISnew) %>%
  left_join(IRISnew_RP1990.idf , by = c("CODE_IRIS" = "CODE_IRIS")) %>%
  left_join(IRISnew_RP2013.idf, by = c("CODE_IRIS" = "CODE_IRIS")) %>%
  mutate_each(funs( ratio_id_IRISnew * .), -c(id, CODE_IRIS,ratio_id_IRISnew)) %>%
  group_by(id) %>%
  summarise_if(is.numeric, funs(sum) ) %>%
  dplyr::select(-ratio_id_IRISnew)

# df pour ggplot également
STATIONS_data_indics.f <-
  REF_stations.geo@data %>%
  left_join(STATIONS_data %>%
            mutate(pct_P13_F65P_P13_POP = P13_F65P / P13_POP,
                   DP90F65P = DP90F65 + DP90F70 + DP90F75 + DP90F80 + DP90F85 + DP90F90 + DP90F95,
                   pct_DP90F65P_DP90T = DP90F65P / DP90T,
                   diff_pct_P13_F65P_P13_POP_pct_DP90F65P_DP90T = pct_P13_F65P_P13_POP - pct_DP90F65P_DP90T,
                   DA90T0_5 = DA90H000 + DA90H001 + DA90H002 + DA90H003 + DA90H004 + DA90H005 + DA90F000 + DA90F001 + DA90F002 + DA90F003 + DA90F004 + DA90F005,
                   pct_DA90T0_5_DP90T = DA90T0_5 / DP90T,
                   P13_POP0_5 = P13_POP0002 + P13_POP0305,
                   pct_P13_POP0_5_P13_POP = P13_POP0_5 / P13_POP,
                   diff_pct_P13_POP0_5_P13_POP_pct_DA90T0_5_DP90T = pct_P13_POP0_5_P13_POP - pct_DA90T0_5_DP90T,
                   pct_P13_NSCOL15P_SUP = P13_NSCOL15P_SUP / P13_NSCOL15P,
                   pct_AF90TSUP = (AF90TBA2 + AF90TSUP) / AF90T15P)  ,
            by =c("id" = "id")) %>%
  filter(!substr(INSEE_COM,1,2) %in% '60') %>%
  as.data.frame()

# régression sur diplomés
# regression locale https://fr.wikipedia.org/wiki/R%C3%A9gression_locale

fit <- loess(pct_P13_NSCOL15P_SUP ~ log(pct_AF90TSUP), data = STATIONS_data_indics.f)

STATIONS_data_indics.f <- 
  STATIONS_data_indics.f %>%
  mutate(res_log_dipl = residuals(fit))

# suppression des fichiers temporaires
rm(intersect.iris, intersect.iris.sta, mat_dist,min.dist, passage_IRIS, passage_IRIS.sta, stations_proches)
rm(REF_stations.zt,REF_stations.Z ,REF_stations_full, REF_stations_ageoo, REF_stations_ageo2, REF_stations_ageo, json_stations)
rm(list=ls(pattern="geos"))
