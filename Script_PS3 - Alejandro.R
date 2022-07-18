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
       osmdata,
       sf,rgdal,raster,rgeos) # Get OSM's data) # Visualizaciones dinámicas 
select <- dplyr::select


#1 Obtener datos-------
## descomprimir archivo

test <- readRDS("test.Rds")
train <- readRDS("train.Rds")

glimpse(test)
glimpse(train)

table(test$title)

## Filtrar por Chapinero y El Poblado -----
test$localidad_filtrada_chapinero <- grepl("chapinero", test$title, ignore.case= T)
test$localidad_filtrada_poblado <- grepl("el poblado|poblado", test$title, ignore.case= T)

test$localidad <- ifelse(test$localidad_filtrada_chapinero == TRUE, "CHAPINERO",
                         ifelse(test$localidad_filtrada_poblado == TRUE, "POBLADO","NO APLICA"))

##
train$localidad_filtrada_chapinero <- grepl("chapinero", train$title, ignore.case= T)
train$localidad_filtrada_poblado <- grepl("el poblado|poblado", train$title, ignore.case= T)

train$localidad <- ifelse(train$localidad_filtrada_chapinero == TRUE, "CHAPINERO",
                          ifelse(train$localidad_filtrada_poblado == TRUE, "POBLADO","NO APLICA"))

table(test$localidad)
table(train$localidad)

test_final <- subset(test, localidad == "CHAPINERO"|localidad== "POBLADO")
train_final <- subset(train, localidad == "CHAPINERO"|localidad== "POBLADO") 


available_features() %>% head(20)
available_tags("amenity") %>% head(20)


test_final_chapinero <- subset(test_final, localidad == "CHAPINERO") %>%
  mutate(surface_total = ifelse(is.na(surface_total) == TRUE,surface_covered,surface_total ))

train_final_chapinero <- subset(train_final, localidad == "CHAPINERO")%>%
  mutate(surface_total = ifelse(is.na(surface_total) == TRUE,surface_covered,surface_total ))

test_final_poblado <- subset(test_final, localidad== "POBLADO")%>%
  mutate(surface_total = ifelse(is.na(surface_total) == TRUE,surface_covered,surface_total ))

train_final_poblado <- subset(train_final, localidad== "POBLADO") %>%
  mutate(surface_total = ifelse(is.na(surface_total) == TRUE,surface_covered,surface_total ))






###-Datos Shapefile Manzana Urbano DANE MGN -------


polygons_mzn <- st_read("localidades.shp")

polygon <- polygons_mzn %>% 
  subset(., NOMBRE == "CHAPINERO") %>%
  select (., NOMBRE, SHAPE_AREA,SHAPE_LEN, geometry)

polygon_trans <- sf::st_transform(polygon, 4326)
polygon_trans


## convertir en sf
housing_ch <- st_as_sf(x=train_final_chapinero,coords=c("lon","lat"),crs=4326)
housing_chapinero <-  st_intersection(polygon_trans, housing_ch)
class(housing_chapinero)



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Bogotá Colombia"))
## objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station


bus_station_chapinero <- st_intersection(polygon_trans, bus_station)



## Pintar las estaciones de autobus, la localidad y los aptos
map <- leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = polygon_trans, color = "blue")%>% 
  addCircleMarkers(data=bus_station_chapinero , col="red")%>% 
  addCircleMarkers(data=housing_chapinero , col="green" , label=housing_chapinero$title, radius= 0.25)
  addLayersControl(
    baseGroups = c("Open Street", "World Imagery")
  )
map







leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% addPolygons(data=polygon) %>% 
  addCircleMarkers(data=housing_chapinero , col="red" , label=housing_chapinero$property_id)


