#30DayMapChallenge
#


##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
#Esto se debe de cambiar en cada computadora
setwd("D:/Documentos/GitHub/30DayMapChallenge")



# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, sf, leaflet)


#Descarga de archivos====
url<-"https://sniiv.sedatu.gob.mx/doc/capas/pcu/2018/PCU_2018_SHP.zip"

temp <- tempfile()
download.file(url,
              mode = "wb",
              destfile = temp)
unzip(temp) 
unlink(temp)



#Leer capa de PCUs
pcu<-st_read("PCUS_2018.shp")%>%
  janitor::clean_names()

##Colores para cada perímetro
pal <- colorFactor(
  palette = c('red', 'green', 'blue'),
  domain = pcu$calif
)


#Mapa
leaflet(pcu,options=leafletOptions(zoomControl = F))%>%
  #Vista y zoom inicial del mapa (Zona Metropolitana del Valle de México)9
  setView(lng = -99.2211882, lat =19.5057864, zoom = 9)%>%
  addTiles()%>%
  addPolygons(color = ~pal(calif))
