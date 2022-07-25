#Problem set 3
#Limpiar memoria y colocar el WD------
rm(list=ls()) ## Limpiar el entorno de trabajo
getwd()

# solucion sf para mac

# Para instalar rgeos
install.packages ("rgeos", repos="http://R-Forge.R-project.org", type="source")
# Para instalar rgdal
install.packages ("rgdal", repos="http://R-Forge.R-project.org", type="source")
# Llamamos devtools para instalar la version de Github de sf
library (devtools)
# Instalamos sf desde su version de <github 
install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")

#librerias----
require(pacman) # Llamar pacman (contiene la función p_load)

# Llamar/instalar-llamar las librerías-----
p_load(tidyverse, # manipular/limpiar conjuntos de datos.
       rio, # función import/export: leer/escribir archivos desde diferentes formatos. 
       leaflet,
       tmaptools, # geocode_OSM()
       osmdata,
       class,skimr,
       rgdal,raster,rgeos,
       sf) # Get OSM's data) # Visualizaciones dinámicas 

select <- dplyr::select


#1 Obtener datos-------
## descomprimir archivo

test <- readRDS("test.Rds")
train <- readRDS("train.Rds")

glimpse(test)
glimpse(train)

table(test$title)

#2 Filtrar por Chapinero y El Poblado -----
test$localidad_filtrada_chapinero <- grepl("chapinero", test$title, ignore.case= T)
test$localidad_filtrada_poblado <- grepl("el poblado|poblado", test$title, ignore.case= T)

test$localidad <- ifelse(test$localidad_filtrada_chapinero == TRUE, "CHAPINERO",
                         ifelse(test$localidad_filtrada_poblado == TRUE, "POBLADO","NO APLICA"))

##
train$localidad_filtrada_chapinero <- grepl("chapinero", train$title, ignore.case= T)
train$localidad_filtrada_poblado <- grepl("el poblado|poblado", train$title, ignore.case= T)

train$localidad <- ifelse(train$localidad_filtrada_chapinero == TRUE, "CHAPINERO",
                          ifelse(train$localidad_filtrada_poblado == TRUE, "POBLADO","NO APLICA"))

test_final <- subset(test, localidad == "CHAPINERO"|localidad== "POBLADO")
train_final <- subset(train, localidad == "CHAPINERO"|localidad== "POBLADO")

table(is.na(test_final$surface_total))
#3 Limpieza de datos

#Imputar cubierta a superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_covered,
                                surface_total))

train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_covered,
                                surface_total))
table(is.na(test_final$surface_total))
## convertir en sf
test_final <- st_as_sf(x=test_final,coords=c("lon","lat"),crs=4326)
train_final <- st_as_sf(x=train_final,coords=c("lon","lat"),crs=4326)

#poner minusculas descripcion
for (i in 1:nrow(test_final)) {
  test_final$description[i]<-str_to_lower(string = test_final$description[i])
}

## Usar descripcion para hallar metros cuadrados

x = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2" ## pattern

test_final = test_final %>% 
  mutate(new_surface = str_extract(string=test_final$description , pattern= x))
sum(table(test_final$new_surface))
## another pattern
y = "[:space:]+[:digit:]+[:space:]+metros"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= y),
                              new_surface))
sum(table(test_final$new_surface))
z = "[:space:]+[:digit:]+[:space:]+mts"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= z),
                              new_surface))
sum(table(test_final$new_surface))

## more patterns
h = "[:digit:]+[:space:]+m2"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= h),
                              new_surface))
sum(table(test_final$new_surface))

l = "[:digit:]+[:space:]+mts"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= l),
                              new_surface))
sum(table(test_final$new_surface))

k = "[:digit:]+[:space:]+metros"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= k),
                              new_surface))
sum(table(test_final$new_surface))

m = "[:digit:]+mts"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= m),
                              new_surface))
sum(table(test_final$new_surface))

n = "[:digit:]+m2"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= n),
                              new_surface))
sum(table(test_final$new_surface))

r  = "[:digit:]+mt2"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= r),
                              new_surface))
sum(table(test_final$new_surface))

t = "[:digit:]+[:space:]+m"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= t),
                              new_surface))
sum(table(test_final$new_surface))

q = "[:digit:]+m"
test_final = test_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=test_final$description , pattern= q),
                              new_surface))
sum(table(test_final$new_surface))

#---- otras descripciones ----
remodelar = "para remodelar"
test_final = test_final %>% 
  mutate(remodelar = str_extract(string=test_final$description, pattern= remodelar),
                              remodelar)


#volver new_surface a numeric

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "[:space:]+m2" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "[:space:]+metros" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "[:space:]+mts" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "," , replacement = ".")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "[:space:]+m" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "[:space:]+metros" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "mts" , replacement = "")
}

test_final$new_surface <- as.numeric(test_final$new_surface)
class(test_final$new_surface)

# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                new_surface,
                                surface_total))
table(is.na(test_final$surface_total))
## make buffer 1
house_buf <- st_buffer(test_final,dist=35)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_3=mean(surface_total.y,na.rm=T))

test_final <- left_join(test_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_3,
                                surface_total))

table(is.na(test_final$surface_total))

## make buffer 1
house_buf <- st_buffer(test_final,dist=100)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_4=mean(surface_total.y,na.rm=T))

test_final <- left_join(test_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_4,
                                surface_total))

table(is.na(test_final$surface_total))

###-Datos Shapefile Manzana Urbano DANE MGN -------
# Caracteristicas disponibles
available_features() %>% head(20)
available_tags("amenity") %>% head(20) # lugares

polygons_mzn <- st_read("localidades.shp")

polygon <- polygons_mzn %>% 
  subset(., NOMBRE == "CHAPINERO") %>%
  select (., NOMBRE, SHAPE_AREA,SHAPE_LEN, geometry)

polygon_trans <- sf::st_transform(polygon, 4326)
polygon_trans

housing_chapinero <-  st_intersection(polygon_trans, housing_ch)
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

##recuperar numero de baños

##eliminar palabra balcon
###buscar con ba
##imputar


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


