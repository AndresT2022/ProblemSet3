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


#polygons_mzn <- st_read("localidades.shp")

#polygon <- polygons_mzn %>% 
#  subset(., NOMBRE == "CHAPINERO") %>%
#  select (., NOMBRE, SHAPE_AREA,SHAPE_LEN, geometry)

#polygon_trans <- sf::st_transform(polygon, 4326)
#polygon_trans 

polygon_trans <- getbb(place_name = "UPZ Chapinero, Bogota", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") %>% .$multipolygon


## convertir en sf
housing_ch <- st_as_sf(x=train_final_chapinero,coords=c("lon","lat"),crs=4326)
housing_chapinero <-  st_intersection(polygon_trans, housing_ch)
class(housing_chapinero)



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("UPZ Chapinero, Bogota"))
## objeto osm
osm = opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station


## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("UPZ Chapinero, Bogota"))
## objeto osm
osm2 = opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
  add_osm_feature(key="leisure" , value="park") 
class(osm2)
## extraer Simple Features Collection
osm_sf2 = osm2 %>% osmdata_sf()
View(osm_sf2)

## Obtener un objeto sf
parks = osm_sf2$osm_polygons %>% select(osm_id,leisure)
parks



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("UPZ Chapinero, Bogota"))
## objeto osm
osm3 = opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
  add_osm_feature(key="amenity" , value="food_court") 
class(osm2)
## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3

## Obtener un objeto sf
food_court = osm_sf3$osm_points %>% select(osm_id,amenity)
food_court



bus_station_chapinero <- st_intersection(polygon_trans, bus_station)
parks_chapinero <- st_intersection(polygon_trans, parks)
food_court_chapinero <- st_intersection(polygon_trans, food_court)



## Pintar las estaciones de autobus, la localidad y los aptos
map <- leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = polygon_trans, color = "blue")%>% 
  addCircleMarkers(data=bus_station_chapinero , col="red")%>%
  addCircleMarkers(data=food_court_chapinero , col="brown")%>%
  addPolygons(data=parks_chapinero , col="green")%>% 
  addCircleMarkers(data=housing_chapinero , col="yellow" , label=housing_chapinero$title, radius= 0.25)
  addLayersControl(
    baseGroups = c("Open Street", "World Imagery")
  )
map




poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
class(poblado)

## convertir en sf
housing_pob <- st_as_sf(x=train_final_poblado,coords=c("lon","lat"),crs=4326)
housing_poblado <-  st_intersection(poblado, housing_pob)
class(housing_poblado)



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Comuna 14 - El Poblado"))
## objeto osm
osm = opq(bbox = getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station_medellin = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station_medellin


## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Comuna 14 - El Poblado"))
## objeto osm
osm2 = opq(bbox = getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="leisure" , value="park") 
class(osm2)
## extraer Simple Features Collection
osm_sf2 = osm2 %>% osmdata_sf()
osm_sf2

## Obtener un objeto sf
parks_medellin = osm_sf2$osm_polygons %>% select(osm_id,leisure)
parks_medellin



## obtener la caja de coordenada que contiene el polígono de El Poblado
opq(bbox = getbb("Comuna 14 - El Poblado"))
## objeto osm
osm3 = opq(bbox = getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key="amenity" , value="food_court") 
class(osm2)
## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3

## Obtener un objeto sf
food_court_medellin = osm_sf3$osm_points %>% select(osm_id,amenity)
food_court_medellin

class(poblado)

bus_station_poblado <- st_intersection(poblado, bus_station_medellin)
parks_poblado <- st_intersection(poblado, parks_medellin)
food_court_poblado <- st_intersection(poblado, food_court_medellin)



## Pintar las estaciones de autobus, la localidad y los aptos
map_poblado <- leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = poblado, color = "blue")%>% 
  addCircleMarkers(data=bus_station_poblado , col="red")%>%
  addCircleMarkers(data=food_court_poblado , col="brown")%>%
  addPolygons(data=parks_poblado , col="green")%>% 
  addCircleMarkers(data=housing_poblado , col="yellow" , label=housing_poblado$title, radius= 0.25)
addLayersControl(
  baseGroups = c("Open Street", "World Imagery")
)
map_poblado


