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

#Question 13 :
dept_bretagne2<-communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(st_union(geom))
plot(dept_bretagne2)

#Question 14 :
centroid_dept_bret<-st_centroid(dept_bretagne)
str(centroid_dept_bret)
plot(st_geometry(dept_bretagne))
plot(st_geometry(centroid_dept_bret),add=TRUE)
dept_lib=data.frame(
  dep=c("22","29","35","56"),
  dep_lib=c("Côte d'Armor","Finistère","Ille-et-Vilaine","Morbihan")
)
centroid_dept_bret<-centroid_dept_bret %>% 
  left_join(dept_lib,
            by="dep")

centroid_coord<-st_coordinates(centroid_dept_bret)
centroid_coord %>% str()

centroid_coord<-centroid_coord %>% 
  bind_cols(... = centroid_dept_bret %>% 
              select(dep,dep_lib) %>% 
              st_drop_geometry()
            )
centroid_coord %>% str()

plot(st_geometry(dept_bretagne))
plot(st_geometry(centroid_dept_bret),pch=16,col='orangered',add=TRUE)
text(
  x=centroid_coord$X,
  y=centroid_coord$Y,
  labels=centroid_coord$dep_lib,
  pos=3,
  cex=0.8,
  col="coral"
)
commune_centroid_bret<-st_intersection(communes_Bretagne,centroid_dept_bret)
typeof(commune_centroid_bret)
