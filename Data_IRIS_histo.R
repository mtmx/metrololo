rm(list = ls(all.names = TRUE))

#############################
# données du RP sur IRIS  1990 et en cours / ventilation sur nouvelle géographie des IRIS

# récupération depuis sync
# https://ln.sync.com/dl/276cf3df0/vbvn3m2a-yea7wms7-kj2hmbz9-7jpyy24k


# data IRIS RP 2013
# source : https://insee.fr/fr/information/2409289
options(java.parameters = "-Xmx8g")
library(XLConnect)

IRIS_RP2013_POP <- loadWorkbook("./data/stat/base-ic-evol-struct-pop-2013.xls") %>%
 readWorksheet( "IRIS", header = T, startRow = 6)

IRIS_RP2013_ACT <- loadWorkbook("./data/stat/base-ic-activite-residents-2013.xls") %>%
  readWorksheet( "IRIS", header = T, startRow = 6)

IRIS_RP2013_FORM <- loadWorkbook("./data/stat/base-ic-diplomes-formation-2013.xls") %>%
 readWorksheet( "IRIS", header = T, startRow = 6)

IRIS_RP2013_FAM <- loadWorkbook("./data/stat/base-ic-couples-familles-menages-2013.xls") %>%
  readWorksheet( "IRIS", header = T, startRow = 6)


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
RF90X <- loadWorkbook("./data/stat/IrisReference/RF90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisReference/RF90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# AO 
AO90X <- loadWorkbook("./data/stat/IrisProfils_activite1/AO90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_activite1/AO90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# AT
AT90X <- loadWorkbook("./data/stat/IrisProfils_activite1/AT90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_activite1/AT90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# AF
AF90X <- loadWorkbook("./data/stat/IrisProfils_activite2/AF90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_activite2/AF90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# DP 
DP90X <- loadWorkbook("./data/stat/IrisProfils_demographie1/DP90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_demographie1/DP90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# DA
DA90X <- loadWorkbook("./data/stat/IrisProfils_demographie1/DA90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_demographie1/DA90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# DM
DM90X <- loadWorkbook("./data/stat/IrisProfils_demographie2/DM90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_demographie2/DM90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))


# MN
MN90X <- loadWorkbook("./data/stat/IrisProfils_Migrations/MN90XIDC.xls") %>%
  readWorksheet( "feuille1", header = T, startRow = 1) %>%
  rbind(loadWorkbook("./data/stat/IrisProfils_Migrations/MN90XIMC.xls") %>%
          readWorksheet( "feuille1", header = T, startRow = 1)) %>%
  select(-c(REG,DEP,TYP_IRIS,INDIC,INFRA,COMP9099,DEPCOM, NOM_COM, DCIRISLI, IRIS, NOM_IRIS))

# table totale indicateurs RP 1990
IRIS_RP1990 <-
  iris2000 %>%
  as.data.frame() %>%
  dplyr::select(IRIS2000) %>%
  left_join(RF90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(AO90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(AT90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(AF90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(DP90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(DA90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(DM90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  left_join(MN90X, by = c("IRIS2000" = "DCOMIRIS")) %>%
  mutate_each(funs(replace(.,is.na(.),0)))

# ventilation sur table de correspondance IRIS
# et somme par IRIS new

IRISnew_RP1990 <-
  passage_IRIS %>%
  as.data.frame() %>%
  dplyr::select(IRIS2000, CODE_IRIS, ratio_IRIS2000_IRISnew) %>%
  left_join(IRIS_RP1990, by = c("IRIS2000" = "IRIS2000")) %>%
  mutate_each(funs( ratio_IRIS2000_IRISnew * .), -c(IRIS2000, CODE_IRIS,ratio_IRIS2000_IRISnew)) %>%
  group_by(CODE_IRIS) %>%
  summarise_if(is.numeric, funs(sum) ) %>%
  dplyr::select(-ratio_IRIS2000_IRISnew)

# suppression des fichiers temporaires
rm(IRIS_RP2013_POP, IRIS_RP2013_ACT, IRIS_RP2013_FAM, IRIS_RP2013_FORM)
rm(RF90X,AO90X, AT90X, AF90X, DP90X, DA90X , DM90X, MN90X)
  
