library(sf)
library(dplyr)
Francemetro<-st_read("~/work/stats_spatiales_files/fonds/commune_francemetro_2021.gpkg")
summary(Francemetro)
head(Francemetro,10)
st_crs(Francemetro)

# Question 5 :
communes_Bretagne<- Francemetro %>% 
  filter(reg =="53")
communes_Bretagne<-communes_Bretagne %>% 
  select(code, libelle, epc, dep, surf)
#Non la variable geom apparaît également

#Question 6 :
str(communes_Bretagne)
#C'est bien un sf

#Question 7 :
plot(communes_Bretagne,lwd=0.1,st_geometry=TRUE)

#Question 9 et 10: 
communes_Bretagne$surf2=st_area(communes_Bretagne$geom)
communes_Bretagne$surf2=units::set_units(communes_Bretagne$surf2,km*km)

#Question 11 :
str(communes_Bretagne$surf2)
mean(as.numeric(communes_Bretagne$surf2))==mean(communes_Bretagne$surf)

# Question 12 :
dept_bretagne<-communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(surf=sum(surf))
plot(dept_bretagne)

L’objectif est de créer une table départementale “dept_bretagne” sans doublons. Cette table devra
contenir le code departement et la superficie du département. Représenter le nouveau fond sur une
carte avec la fonction plot
