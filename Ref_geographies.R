rm(list = ls(all.names = TRUE))

#############################
# géographie des IRIS 2000 et IRIS en cours / table de correspondance

# données 1990 -> géographie 2000
# données 2013 -> géographie 2015 (geofla 2016)

library(maptools)
library(rgeos)
library(sf)
library(dplyr)

# reférentiel GEOFLA janvier 2017 IGN
#https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2017-01-18$ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/file/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z

comm <- readShapeSpatial("./data/geo/COMMUNE" ,proj4string=CRS("+init=epsg:2154"))
#comm.sf <- st_read("./data/geo/COMMUNE.shp" ) %>% st_transform(crs = "+init=epsg:2154") 

# nettoyage des géometries
comm <- gBuffer(comm, byid = T)

# maille département
dep <- gUnaryUnion(comm, comm$INSEE_DEP)
dep$id <- row.names(dep)

#sf
comm.sf <- st_as_sf(comm)

# shapes IRIS
irisnew <- st_read("./data/geo/CONTOURS-IRIS.shp" ) %>% st_transform(crs = "+init=epsg:2154") 
iris2000 <- st_read("./data/geo/IRIS2000_RGF93.shp") %>% st_transform(crs = "+init=epsg:2154") 

#en format sp également 
irisnew.sp <- readShapeSpatial("./data/geo/CONTOURS-IRIS" ,proj4string=CRS("+init=epsg:2154"))


# nettoyage des geometries et selection IDF
irisnew.IDF <- irisnew %>% filter(substr(CODE_IRIS, 1, 2) %in% c('75', '77','78','91','92','93','94','95')) %>% st_buffer(dist = 0, nQuadSegs = 30)
iris2000.IDF <- iris2000 %>% filter(substr(IRIS2000, 1, 2) %in% c('75', '77','78','91','92','93','94','95')) %>% st_buffer(dist = 0, nQuadSegs = 30)

# verifications de l'allure
plot(subset(dep, id %in% '75'))
plot(st_geometry(subset(iris2000.IDF,IRIS2000 %in% '751010101')))
plot(st_geometry(irisnew.IDF), add=T, border = "red")

# table de correspondance entre IRIS 2000 et nouveaux IRIS
intersect.iris <- st_intersection(irisnew.IDF,iris2000.IDF )

passage_IRIS <- intersect.iris %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>%
  dplyr::select(IRIS2000, CODE_IRIS, area) %>%
  mutate(IRIS2000 = as.character(IRIS2000), CODE_IRIS = as.character(CODE_IRIS)) %>%
  group_by(IRIS2000, CODE_IRIS ) %>%
  summarise (area.intersect = sum(area)) %>%
  mutate(ratio_IRIS2000_IRISnew = area.intersect / sum(area.intersect))


#####
# possible avec une source de population localisée 
# si source disponible

devtools::install_github("joelgombin/spReapportion")
library(spReapportion)
  
 
