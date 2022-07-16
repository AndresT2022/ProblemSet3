#Problem set 3
#Limpiar memoria y colocar el WD------
rm(list=ls()) ## Limpiar el entorno de trabajo
getwd()

#librerias----
require(pacman) # Llamar pacman (contiene la función p_load)

# Llamar/instalar-llamar las librerías-----
p_load(tidyverse, # manipular/limpiar conjuntos de datos.
       rio, # función import/export: leer/escribir archivos desde diferentes formatos. 
       sf, # Leer/escribir/manipular datos espaciales
       leaflet,
       tmaptools, # geocode_OSM()
       osmdata) # Get OSM's data) # Visualizaciones dinámicas 

#1 Obtener datos-------
## descomprimir archivo
unzip(zipfile="dataPS3.zip",
      exdir=".")
glimpse(test)
glimpse(train)

available_features() %>% head(20)
available_tags("amenity") %>% head(20)

## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Bogotá Chapinero Colombia"))
## objeto osm
osm = opq(bbox = getbb("Bogotá Chapinero Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station
## Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red")

view(osm_sf)




