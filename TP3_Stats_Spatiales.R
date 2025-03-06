# Chargement des packages
library(openxlsx)
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)


communes_fm<-st_read("fonds/commune_francemetro_2021.gpkg") %>% 
  select(code,libelle,surf)

pop_com_2019<-openxlsx::read.xlsx("data/Pop_legales_2019.xlsx")
pop_com_2019<-pop_com_2019 %>% 
  mutate(COM=if_else(substr(COM,1,3)=="751","75056",COM)) %>% 
  group_by(code=COM) %>% 
  summarise(pop=sum(PMUN19))

communes_fm<-communes_fm %>% 
  left_join(pop_com_2019,
            by="code") %>% 
  mutate(densite=pop/surf)

summary(communes_fm$densite)
hist(communes_fm$densite)
plot(communes_fm["densite"],border=FALSE)
plot(communes_fm["densite"],breaks="quantile",main="quantile",border=FALSE)
plot(communes_fm["densite"],breaks="sd",main="sd",border=FALSE)
plot(communes_fm["densite"],breaks="fisher",main="fisher",border=FALSE)
plot(communes_fm["densite"],breaks="pretty",main="pretty",border=FALSE)

denspop_quant<-classIntervals(
  communes_fm$densite,
  style="quantile",
  n=5
)

str(denspop_quant)
denspop_quant$brks

quantile(communes_fm$densite,probs=seq(0,1,0.1))
summary(communes_fm$densite)

denspop_man_brks5<-c(0,80,320,1200,3600,28000)
popcomfm_sf<-communes_fm %>% 
  mutate(
    densite_c=cut(
      densite,
      breaks=denspop_man_brks5,
      labels=paste0(denspop_man_brks5[1:5],"-",denspop_man_brks5[2:6]),
      include.lowest=TRUE,
      right=FALSE,
      ordered_result=TRUE))
table(popcomfm_sf$densite_c)

pal1<-RColorBrewer::brewer.pal(
  n=5,
  name="Spectral")

plot(popcomfm_sf["densite_c"],pal=rev(pal1),main="Densite de population",border=FALSE)


#Exercice 2
tx_pauvrete<-read.xlsx("data/Taux_pauvrete_2018.xlsx")
dep_francemetro_2021<-st_read("fonds/dep_francemetro_2021.gpkg")
mer<-st_read("fonds/merf_2021.gpkg")

dep_francemetro_2018_pauv<-dep_francemetro_2021 %>% 
  left_join(tx_pauvrete %>% select(-Dept),by=c("code"="Code"))

mf_map(x=dep_francemetro_2018_pauv,
       var="Tx_pauvrete",
       type="choro",
       nbreaks=4,
       breaks="jenks")

couleur<-rev(mf_get_pal(4,"Mint"))
mf_map(mer)

mf_map(x=dep_francemetro_2018_pauv,
       var="Tx_pauvrete",
       type="choro",
       breaks=c(0,13,17,25,max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       pal=couleur,
       leg_pos = NA,add=TRUE)

mf_inset_on(x=dep_francemetro_2018_pauv,pos="topright",cex=.2)
mf_init(dep_francemetro_2018_pauv %>% 
    filter(code %in% c("75","92","93","94")))
          


mf_map(x=dep_francemetro_2018_pauv%>% 
         filter(code %in% c("75","92","93","94")),
       var="Tx_pauvrete",
       type="choro",
       breaks=c(0,13,17,25,max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       pal=couleur,
       leg_pos = NA,add=TRUE)

mf_inset_off()


mf_layout(title="Taux de pauvreté par département en 2018",
          credits="Source: Insee")

#Exercice 4
communes_fm<-st_read("fonds/commune_francemetro_2021.gpkg")
bpe20<-read.csv2("bpe20_sport_loisir_xy.csv",sep=";")

