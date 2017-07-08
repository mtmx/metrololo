library(dplyr)

# chiffres totaux france
FR_stats <-
  IRISnew_RP2013 %>%
  summarise_if(is.numeric, funs(sum) ) %>%
  cbind(
    IRIS_RP1990 %>%
      summarise_if(is.numeric, funs(sum) ) ) %>%
  mutate(pct_P13_POPF = P13_POPF / P13_POP,
         pct_P13_NSCOL15P_SUP = P13_NSCOL15P_SUP / P13_NSCOL15P,
         pct_AF90TSUP = (AF90TBA2 + AF90TSUP) / AF90T15P,
         pct_AT90TOU = AT90TOU / AT90TOU,
         pct_AT90TCA = AT90TCA / AT90TA,
         pct_C13_ACT1564_CS3 = C13_ACT1564_CS3 / C13_ACT1564 ,
         pct_P13_F65P_P13_POP = P13_F65P / P13_POP,
         DP90F65P = DP90F65 + DP90F70 + DP90F75 + DP90F80 + DP90F85 + DP90F90 + DP90F95,
         pct_DP90F65P_DP90T = DP90F65P / DP90T,
         DA90T0_5 = DA90H000 + DA90H001 + DA90H002 + DA90H003 + DA90H004 + DA90H005 + DA90F000 + DA90F001 + DA90F002 + DA90F003 + DA90F004 + DA90F005,
         pct_DA90T0_5_DP90T = DA90T0_5 / DP90T,
         P13_POP0_5 = P13_POP0002 + P13_POP0305,
         pct_P13_POP0_5_P13_POP = P13_POP0_5 / P13_POP,
         diff_pct_P13_F65P_P13_POP_pct_DP90F65P_DP90T = pct_P13_F65P_P13_POP - pct_DP90F65P_DP90T,
         diff_pct_P13_POP0_5_P13_POP_pct_DA90T0_5_DP90T = pct_P13_POP0_5_P13_POP - pct_DA90T0_5_DP90T
         #pct_P13_NA17_JZ_TOT = NA17_JZ / NA17_TOT
  ) %>%
  as.data.frame() %>%
  select(pct_P13_F65P_P13_POP, P13_F65P , P13_POP, pct_DP90F65P_DP90T , DP90F65P , DP90T, pct_P13_NSCOL15P_SUP,pct_AF90TSUP , P13_NSCOL15P_SUP, P13_NSCOL15P,DA90T0_5, pct_DA90T0_5_DP90T, P13_POP0_5, pct_P13_POP0_5_P13_POP) %>%
  View()


# table data RP 2013 par commune

COMM_RP2013 <-
  IRISnew_RP2013 %>%
  mutate(depcom = substr(CODE_IRIS,1,5)) %>%
  mutate(P13_POP0_5 = P13_POP0002 + P13_POP0305) %>%
  group_by(depcom) %>%
  summarise_if(is.numeric, funs(sum) ) %>%
  #select(depcom, P13_F65P , P13_POP) %>%
  as.data.frame()

# table data RP 1990 par commune

COMM_RP1990 <-
  IRIS_RP1990 %>%
  mutate(depcom = substr(id,1,5)) %>%
  group_by(depcom) %>%
  summarise_if(is.numeric, funs(sum) ) %>%
  mutate(DP90F65P = DP90F65 + DP90F70 + DP90F75 + DP90F80 + DP90F85 + DP90F90 + DP90F95) %>%
  mutate(DA90T0_5 = DA90H000 + DA90H001 + DA90H002 + DA90H003 + DA90H004 + DA90H005 + DA90F000 + DA90F001 + DA90F002 + DA90F003 + DA90F004 + DA90F005) %>%
  #select(depcom, DP90F65P , DP90T) %>%
  as.data.frame()


# aggrégation au niveau supra
#install.packages("antuki/COGugaison")
library(COGugaison)
#verifier millésime COG
COG_akinator(COMM_RP2013$depcom, donnees_insee = T)
COG_akinator(COMM_RP1990$depcom, donnees_insee = T)

# data RP 2013 : enlever arrondissements Paris Lyon Marseille
COMM_RP2013_sansPLM <-enlever_PLM(table_entree=COMM_RP2013,codgeo_entree = "depcom",libgeo=NULL,agregation = T)

# data RP 1990 : enlever arrondissements Paris Lyon Marseille et convertir en COG 2015
COMM_RP1990_sansPLM <-enlever_PLM(table_entree=COMM_RP1990,codgeo_entree = "depcom",libgeo=NULL,agregation = T) 
COMM_RP1990_COG2015 <- changement_COG_varNum(table_entree=COMM_RP1990_sansPLM,annees=c(1999:2015),agregation=T,libgeo=T,donnees_insee=T)

# data RP 1990 et 2013 en COG 2015
COMM_RP1990_2013 <- COMM_RP2013_sansPLM %>% 
  left_join(COMM_RP1990_COG2015, by = c("depcom" = "depcom"))

# aggréger données par canton
CV_RP1990_2013 <- nivsupra(table_entree = COMM_RP1990_2013, codgeo_entree = "depcom",nivsupra="CV",agregation=T)  %>% 
  mutate( pct_P13_F65P_P13_POP = (P13_F65P / P13_POP) * 100 ,
          pct_DP90F65P_DP90T = (DP90F65P / DP90T) * 100,
          pct_DA90T0_5_DP90T = (DA90T0_5 / DP90T) * 100,
          pct_P13_POP0_5_P13_POP = (P13_POP0_5 / P13_POP) * 100,
          pct_P13_NSCOL15P_SUP = (P13_NSCOL15P_SUP / P13_NSCOL15P) * 100,
          pct_AF90TSUP = ( (AF90TBA2 + AF90TSUP)  / AF90T15P) * 100) %>%
  as.data.frame()


##### carto

library(cartography)
library(rgeos)

# Set a custom color palette
#cols <- carto.pal(pal1 = "green.pal", n1 = 6)
#bks <- c(0, 0.06, 0.08, 0.12, 0.14, 1)
bks <- c(0, 0.04, 0.06, 0.08, 0.1,0.12, 1)
cols <- carto.pal(pal1 = "pink.pal", n1 = 8)
opar <- par(mar = c(0,0,1.2,0))
plot(dep.s, col = "grey60",border = "white", lwd=0.4, add=F)
#text(x = 1017863, y = 7051189, labels = "1990", cex = 1.8, adj = 0,col = "grey40")

# choroplèthe
choroLayer(spdf = CV_spdf.s, #CV_spdf.s, 
           df = CV_RP2013_COLOCATION, #CV_RP1990_2013, 
           spdfid = "id", 
           dfid = "CANTVILLE", #"CV", 
           var = "LPRM_3_pct_POP",
           col = cols,
           #breaks = bks * 100, 
           method = "quantile", nclass = 8,
           border = NA,  
           lwd = 0.2, 
           legend.pos = "right",
           legend.values.rnd = 0, 
           legend.title.txt = "%", 
           add = TRUE) 

propSymbolsChoroLayer(spdf = CV_spdf, 
           df = CV_RP1990_2013, 
           spdfid = "id", 
           dfid = "CV", 
           var = "P13_F65P",
           var2 = "pct_P13_F65P_P13_POP",
           col = cols,
           breaks = bks * 100, 
           lwd = 0.2, 
inches = 0.3, fixmax = NULL, symbols = "circle", border = "grey20",
 legend.title.cex = 0.8,
legend.values.cex = 0.6, legend.var.pos = "right",
legend.var.title.txt = "nb", legend.var.values.rnd = 0,
legend.var.style = "c", legend.var.frame = FALSE,
legend.var2.pos = "topright", legend.var2.title.txt = "part",
legend.var2.values.rnd = -2,
legend.var2.frame = FALSE, 
add = TRUE) 

plot(dep.s, col = NA,border = "grey80", lwd=0.8, add=T)

titre <- "Part des mamies dans la population"

layoutLayer(title = titre,
            col = "#949494",
            sources = "Insee, RP 2013",
            author = "",
            scale = NULL,
            frame = T, south = F, north = F)

CV_RP1990_2013 %>% select(CV,LIBGEO, pct_AF90TSUP,  pct_P13_NSCOL15P_SUP ) %>% View()
# cartogram ?

# TROP LOURD
####### carto ggplot + ggiraph

library(ggplot2)
library(ggiraph)
library(scales)
library(ggthemes)

CV_spdf.f <- fortify(CV_spdf.s, region = "id") %>%
 left_join(CV_RP1990_2013, by = c("id" = "CV"))

dep.f <- fortify(dep.s, region = "id") 

tip <- paste("<strong>", conv_accents(CV_spdf.f$nom_station),"</strong><br />", 
                  "1990 :", sprintf("%1.1f%%", 100*CV_spdf.f$pct_DP90F65P_DP90T), "de mamies<br />",
                  "2013 :", sprintf("%1.1f%%", 100*CV_spdf.f$pct_P13_F65P_P13_POP), "de mamies<br />")


gg <- ggplot(CV_spdf.f) + 
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = pct_P13_F65P_P13_POP), 
    color = "grey"
  ) + 
  geom_path(
    data = dep.f, aes(x = long, y = lat, group = group), color = "black"
  ) +
  coord_equal() + 
  scale_fill_gradient2(low = "green",mid="white", high = "blue") +
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank()
  ) + 
  ggtitle("Naissances en 2014")



### ggiraph
gg <- ggplot(CV_spdf.f) + 
  geom_polygon_interactive(
    aes(x = long, y = lat, group = group, fill = pct_P13_F65P_P13_POP,
        tooltip = tip, data_id = id), 
    color = "grey"
  ) + 
  geom_path(
    data = dep.f, aes(x = long, y = lat, group = group), color = "black"
  ) +
  coord_equal() + 
  scale_color_gradient2(low = "green",mid="white", high = "blue") +
 theme(
    axis.text = element_blank(), 
    axis.title = element_blank()
  ) + 
  ggtitle("Naissances en 2014")


ggiraph(code = {print(gg)}, width = 14, height = 10, hover_css = "{fill:orange;}")
