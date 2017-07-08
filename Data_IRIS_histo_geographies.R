rm(list = ls(all.names = TRUE))


#############################
# géographie des IRIS 2000 et IRIS en cours / table de correspondance

# données 1990 -> géographie 2000
# données 2013 -> géographie 2015 (geofla 2016)

library(maptools)
library(rgeos)
library(sf)
library(dplyr)
library(spdplyr)
library(rgdal)
options(java.parameters = "-Xmx8g")
library(XLConnect)

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

# shape IRIS 2013
tmp <- tempdir()
url_comm <- "https://wxs-telechargement.ign.fr/1yhlj2ehpqf3q6dt6a2y7b64/telechargement/inspire/CONTOURS-IRIS-2015-01-01$CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/file/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10.7z"
download.file(url_comm, destfile = "/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10.7z")
system("7z x -o/tmp /tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10.7z")

# importer shape des iris métropole + DOM
IRIS_FRMET <- readOGR( dsn = "/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2015",  "CONTOURS-IRIS") 
IRIS_FR971 <- readOGR( dsn = "/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_UTM20W84GUAD_D971-2015",  "CONTOURS-IRIS")
IRIS_FR972 <- readOGR( dsn = "/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_UTM20W84MART_D972-2015",  "CONTOURS-IRIS")
IRIS_FR973 <- readOGR( dsn = "/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_UTM22RGFG95_D973-2015",  "CONTOURS-IRIS")
IRIS_FR974 <- readOGR( dsn = "/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_RGR92UTM40S_D974-2015",  "CONTOURS-IRIS")
 
# liste des IRIS d'apres le fonds carto
# france métropolitaine et DOM
IRIS_FR_df <-
  IRIS_FRMET %>% as.data.frame() %>%
  rbind(IRIS_FR971 %>% as.data.frame()) %>%
  rbind(IRIS_FR972 %>% as.data.frame()) %>%
  rbind(IRIS_FR973 %>% as.data.frame()) %>%
  rbind(IRIS_FR974 %>% as.data.frame()) 

irisnew <- st_read("/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2015/CONTOURS-IRIS.shp") %>% st_transform(crs = "+init=epsg:2154") 
# shapes IRIS 2000
iris2000 <- st_read("./data/geo/IRIS2000_RGF93.shp") %>% st_transform(crs = "+init=epsg:2154") 

#en format sp également 
irisnew.sp <- readOGR("/tmp/CONTOURS-IRIS_2-1__SHP_LAMB93_FXX_2016-11-10/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2015/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2015/", "CONTOURS-IRIS") %>% spTransform( CRS("+init=epsg:2154"))
iris2000.sp <- readOGR("./data/geo/", "IRIS2000_RGF93") %>% spTransform( CRS("+init=epsg:2154"))



# générer couche des cantons 2017
library(tmap)
library(tmaptools)
library(rgeos)
comm_supra <- append_data(comm, table_supracom, key.shp = "INSEE_COM", key.data = "CODGEO")
CV_spdf <- gUnaryUnion(comm_supra, comm_supra$CV)
CV_spdf$id <- row.names(CV_spdf)

# simplifier les geometries
library(rmapshaper)
CV_spdf.s <- ms_simplify(CV_spdf, keep = 0.1)
dep.s <- ms_simplify(dep, keep = 0.1)

# cantons 2015
# communes 2015
url_comm <- "https://wxs-telechargement.ign.fr/oikr5jryiph0iwhw36053ptm/telechargement/inspire/GEOFLA_THEME-COMMUNE_2015_2$GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01/file/GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01.7z"
download.file(url_comm, destfile = "/tmp/GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01.7z")
system("7z x -o/tmp /tmp/GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01.7z")
COMM2015_FRMET <- readOGR( dsn = "/tmp/GEOFLA_2-1_COMMUNE_SHP_LAMB93_FXX_2015-12-01/GEOFLA/1_DONNEES_LIVRAISON_2015/GEOFLA_2-1_SHP_LAMB93_FR-ED152/COMMUNE",  "COMMUNE") 
COMM2015_FRMET@data <-COMM2015_FRMET@data %>% mutate(DEPCOM = ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                       ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                              ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',as.character(INSEE_COM)))) ) 

url_data <- "https://www.insee.fr/fr/statistiques/fichier/2028028/table-appartenance-geo-communes-15.zip"
download.file(url_data, destfile = "/tmp/table-appartenance-geo-communes-14.zip")
system("7z x -o/tmp /tmp/table-appartenance-geo-communes-15.zip")

COMM_APPARTENANCEGEO_2015 <- loadWorkbook("/tmp/table-appartenance-geo-communes-15.xls") %>%
  readWorksheet( "COM", header = T, startRow = 6) %>%
# ou quelques modifs pour que ça colle
  mutate(can = ifelse(CV %in% '69ZZ', '6999',
                      ifelse(CV %in% '75ZZ', '7599',
                             ifelse(CV %in% '972ZZ', '97299',
                                    ifelse(CV %in% '973ZZ', '97399', as.character(CV))))))

comm2015_supra <- append_data(COMM2015_FRMET, COMM_APPARTENANCEGEO_2015, key.shp = "DEPCOM", key.data = "codgeo")
CV2015_spdf <- gUnaryUnion(comm2015_supra, comm2015_supra$can)
CV2015_spdf$id <- row.names(CV2015_spdf)
CV2015_spdf.s <- ms_simplify(CV2015_spdf, keep = 0.1)


#############################
# données du RP sur IRIS  1990 et en cours / ventilation sur nouvelle géographie des IRIS

# récupération depuis sync
# https://ln.sync.com/dl/276cf3df0/vbvn3m2a-yea7wms7-kj2hmbz9-7jpyy24k


# data IRIS RP 2013
# source : https://insee.fr/fr/information/2409289

IRIS_RP2013_POP <- loadWorkbook("./data/stat/base-ic-evol-struct-pop-2013.xls") %>%
  readWorksheet( "IRIS", header = T, startRow = 6) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

IRIS_RP2013_ACT <- loadWorkbook("./data/stat/base-ic-activite-residents-2013.xls") %>%
  readWorksheet( "IRIS", header = T, startRow = 6) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

IRIS_RP2013_FORM <- loadWorkbook("./data/stat/base-ic-diplomes-formation-2013.xls") %>%
  readWorksheet( "IRIS", header = T, startRow = 6) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

IRIS_RP2013_FAM <- loadWorkbook("./data/stat/base-ic-couples-familles-menages-2013.xls") %>%
  readWorksheet( "IRIS", header = T, startRow = 6) %>%
  mutate_each(funs(replace(.,is.na(.),0)))


# table totale indicateurs RP 2013
IRISnew_RP2013 <-
  irisnew %>%
  as.data.frame() %>%
  dplyr::select(CODE_IRIS) %>%
  left_join(IRIS_RP2013_POP %>% dplyr::select(-c(REG, REG2016, DEP, UU2010, COM, LIBCOM, TRIRIS, GRD_QUART, LIBIRIS, TYP_IRIS, MODIF_IRIS, LAB_IRIS)) , by = c("CODE_IRIS" = "IRIS")) %>%
  left_join(IRIS_RP2013_ACT %>% dplyr::select(-c(REG, REG2016, DEP, UU2010, COM, LIBCOM, TRIRIS, GRD_QUART, LIBIRIS, TYP_IRIS, MODIF_IRIS, LAB_IRIS)), by = c("CODE_IRIS" = "IRIS")) %>%
  left_join(IRIS_RP2013_FORM %>% dplyr::select(-c(REG, REG2016, DEP, UU2010, COM, LIBCOM, TRIRIS, GRD_QUART, LIBIRIS, TYP_IRIS, MODIF_IRIS, LAB_IRIS)), by = c("CODE_IRIS" = "IRIS")) %>%
  left_join(IRIS_RP2013_FAM %>% dplyr::select(-c(REG, REG2016, DEP, UU2010, COM, LIBCOM, TRIRIS, GRD_QUART, LIBIRIS, TYP_IRIS, MODIF_IRIS, LAB_IRIS)), by = c("CODE_IRIS" = "IRIS")) %>%
  mutate_each(funs(replace(.,is.na(.),0)))


# data IRIS RGP 1990

# RF 
IRIS_RP1990_REF <- loadWorkbook("./data/stat/IrisReference/RF90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisReference/RF90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

RF90X <- loadWorkbook("./data/stat/IrisReference/RF90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisReference/RF90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# AO 
AO90X <- loadWorkbook("./data/stat/IrisProfils_activite1/AO90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_activite1/AO90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# AT
AT90X <- loadWorkbook("./data/stat/IrisProfils_activite1/AT90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_activite1/AT90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# AF
AF90X <- loadWorkbook("./data/stat/IrisProfils_activite2/AF90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_activite2/AF90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# DP 
DP90X <- loadWorkbook("./data/stat/IrisProfils_demographie1/DP90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_demographie1/DP90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# DA
DA90X <- loadWorkbook("./data/stat/IrisProfils_demographie1/DA90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_demographie1/DA90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# DM
DM90X <- loadWorkbook("./data/stat/IrisProfils_demographie2/DM90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_demographie2/DM90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))


# MN
MN90X <- loadWorkbook("./data/stat/IrisProfils_Migrations/MN90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_Migrations/MN90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS)) %>%
  mutate_each(funs(replace(.,is.na(.),0)))


#######
## contours IRIS 2000 modifié à cause de données RP1990 non irisées
# filtre sur france métropolitaine

iris2000.sp.met <- iris2000.sp %>%
  filter(!substr(IRIS2000,1,2) %in% c('9A','9B','9C','9D')) %>%
  gBuffer( byid = T, width = 0) %>%
  left_join(IRIS_RP1990_REF %>% select(DCOMIRIS, RD90POP), by = c( "IRIS2000"= "DCOMIRIS")) %>%
  mutate(IRIS2000_b = ifelse(is.na(RD90POP), paste0(substr(IRIS2000,1,5),"0000"),  substr(IRIS2000,1,9)) ) 

iris2000.sp.met2 <-   gUnaryUnion(iris2000.sp.met, iris2000.sp.met$IRIS2000_b)
iris2000.sp.met2$id <- row.names(iris2000.sp.met2)

# filtre sur france métropolitaine pour IRIS nouveaux  également
irisnew.sp <-
  irisnew.sp %>% filter(!substr(CODE_IRIS,1,2) %in% c('9A','9B','9C','9D')) %>%
  gBuffer( byid = T, width = 0)


# table IDF indicateurs RP 1990
IRIS_RP1990 <-
  iris2000.sp.met2 %>%
  as.data.frame() %>%
  dplyr::select(id) %>%
  left_join(RF90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(AO90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(AT90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(AF90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(DP90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(DA90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(DM90X, by = c("id" = "DCOMIRIS")) %>%
  left_join(MN90X, by = c("id" = "DCOMIRIS")) %>%
  mutate_each(funs(replace(.,is.na(.),0)))


#  filtre sur IDF pour croisement
irisnew.IDF <- irisnew.sp %>% filter(substr(CODE_IRIS, 1, 2) %in% c('75', '77','78','91','92','93','94','95')) %>% gBuffer(byid=T , width = 0)
iris2000.IDF <- iris2000.sp.met2 %>% filter(substr(id, 1, 2) %in% c('75', '77','78','91','92','93','94','95')) %>% gBuffer(byid=T , width = 0)

# verifications de l'allure
plot(subset(dep, id %in% '75'))
plot(st_geometry(subset(iris2000.IDF,IRIS2000 %in% '751010101')))
plot(st_geometry(irisnew.IDF), add=T, border = "red")

# table de correspondance entre IRIS 2000 et nouveaux IRIS
# avec sf pour gagner en rapidité

irisnew.IDF.sf <- st_as_sf(irisnew.IDF)
iris2000.IDF.sf <- st_as_sf(iris2000.IDF)

intersect.iris <- st_intersection(irisnew.IDF.sf,iris2000.IDF.sf )

passage_IRIS <- intersect.iris %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>%
  dplyr::select(id, CODE_IRIS, area) %>%
  mutate(id = as.character(id), CODE_IRIS = as.character(CODE_IRIS)) %>%
  group_by(id, CODE_IRIS ) %>%
  summarise (area.intersect = sum(area)) %>%
  mutate(ratio_IRIS2000_IRISnew = area.intersect / sum(area.intersect))


#####
# possible avec une source de population localisée 
# si source disponible

#devtools::install_github("joelgombin/spReapportion")
#library(spReapportion)



# ventilation sur table de correspondance IRIS
# et somme par IRIS new

IRISnew_RP1990.idf <-
  passage_IRIS %>%
  as.data.frame() %>%
  dplyr::select(id, CODE_IRIS, ratio_IRIS2000_IRISnew) %>%
  filter(substr(CODE_IRIS,1,2) %in% c('75','77','78','91','92','93','94','95')) %>%
  left_join(IRIS_RP1990, by = c("id" = "id")) %>%
  mutate_each(funs( ratio_IRIS2000_IRISnew * .), -c(id, CODE_IRIS,ratio_IRIS2000_IRISnew)) %>%
  group_by(CODE_IRIS) %>%
  summarise_if(is.numeric, funs(sum) ) %>%
  dplyr::select(-ratio_IRIS2000_IRISnew)

IRISnew_RP2013.idf <-
  irisnew.sp %>%
  as.data.frame() %>%
  dplyr::select(CODE_IRIS) %>%
  left_join(IRISnew_RP2013, by = c("CODE_IRIS" = "CODE_IRIS")) 

# suppression des fichiers temporaires
rm(IRIS_RP2013_POP, IRIS_RP2013_ACT, IRIS_RP2013_FAM, IRIS_RP2013_FORM)
rm(RF90X,AO90X, AT90X, AF90X, DP90X, DA90X , DM90X, MN90X)


