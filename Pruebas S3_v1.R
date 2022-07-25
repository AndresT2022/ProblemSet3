## Llamar pacman (contiene la función p_load)
require(pacman) 

## Llama/instala-llama las librerías listadas
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata,ggplot2, plotly ) # Get OSM's data

## 

crimen_bog <- read_sf("/Users/ignacioserrano/Documents/Maestria/Big Data/GitHub/dai_shp/DAILoc.shp")

cr_upz <- read_sf("/Users/ignacioserrano/Documents/Maestria/Big Data/GitHub/dai_shp/DAIUPZ.shp")



class(crimen_bog)

st_crs(crimen_bog)

polygon_crimen <- sf::st_transform(crimen_bog, 4326)
polygon_crimen



# por upz chapinero (Refugio, san isidro - patios,
# Chico - lago, Chapinero)
####-Pruebas Finales-----------

test_final_V2 <- st_as_sf(x=test_final_V2,coords=c("lon","lat"),crs=4326)
train_final_V2 <- st_as_sf(x=train_final_V2,coords=c("lon","lat"),crs=4326)

polygon_trans <- getbb(place_name = "UPZ Chapinero, Bogota", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") %>% .$multipolygon

housing <- st_as_sf(x=train_final_V2,coords=c("lon","lat"),crs=4326) 
housing_ch <- st_intersection(polygon_trans,housing)

mnz_censo = import("http://eduard-martinez.github.io/data/fill-gis-vars/mnz_censo.rds")
housing_ch_censal <- st_intersection(mnz_censo,housing_ch)
class(mnz_censo)
## about data
#browseURL("https://eduard-martinez.github.io/teaching/meca-4107/7-censo.txt")
#house_censo = st_join(housing_ch, mnz_censo)
colnames(house_censo) 
table(house_censo$med_VA1_ESTRATO)
map <- leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = polygon_trans, color = "blue")%>% 
  addCircleMarkers(data = housing_ch, color = "red")%>% 
  addLayersControl(
    baseGroups = c("Open Street", "World Imagery")
  )
map



polygon_trans <- getbb(place_name = "UPZ Chapinero, Bogota", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") %>% .$multipolygon


## convertir en sf
housing_ch <- st_as_sf(x=train_final_V2,coords=c("lon","lat"),crs=4326)
housing_chapinero <-  st_intersection(polygon_trans, housing_ch)
class(housing_chapinero)



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Bogotá, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station

## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Bogotá, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key="landuse" , value="retail") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
retail = osm_sf$osm_polygons %>% select(osm_id,amenity)
retail



parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)



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
parks_chapinero <- st_intersection(polygon_trans, parques_geometria)
food_court_chapinero <- st_intersection(polygon_trans, food_court)
retail_chapinero <- st_intersection(polygon_trans, retail)


# Calculamos el centroide de cada parque
centroides_parques <- gCentroid(as(parks_chapinero$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = parks_chapinero, col = "green",
              opacity = 0.8, popup = parks_chapinero$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)

# Calculamos el centroide de cada parque
centroides_transmilenio <- gCentroid(as(bus_station_chapinero$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = bus_station_chapinero, col = "green",
              opacity = 0.8, popup = bus_station_chapinero$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)






## Pintar las estaciones de autobus, la localidad y los aptos
map <- leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = polygon_trans, color = "blue")%>% 
  addCircleMarkers(data=bus_station_chapinero , col="red")%>%
  addPolygons(data=retail_chapinero , col="brown")%>%
  addPolygons(data=parks_chapinero , col="green")%>% 
  addCircleMarkers(data=housing_chapinero , col="yellow" , label=housing_chapinero$title, radius= 0.25)
addLayersControl(
  baseGroups = c("Open Street", "World Imagery")
)
map


centroides_sf_parks <- st_as_sf(centroides_parques, coords = c("x", "y"))
centroides_sf_bus_station<- st_as_sf(centroides_transmilenio, coords = c("x", "y"))


dist_matrix_parks <- st_distance(x = housing_chapinero, y = centroides_sf_parks)
dist_matrix_bus_station <- st_distance(x = housing_chapinero, y = centroides_sf_bus_station)

# Encontramos la distancia mínima a un parque
dist_min_parks <- apply(dist_matrix_parks, 1, min)
dist_min_bus_station <- apply(dist_matrix_bus_station, 1, min)
housing_chapinero$distancia_parque <- dist_min_parks
housing_chapinero$distancia_bus_station <- dist_min_bus_station




p <- ggplot(housing_chapinero, aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a un parque y el valor del inmueble en Chapinero") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)


p2 <- ggplot(housing_chapinero, aes(x = distancia_bus_station, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una transporte en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a una estación de transporte y el valor del inmueble en Chapinero") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2)


p3 <- ggplot(housing_chapinero, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p3)

p4 <- ggplot(housing_chapinero, aes(x = distancia_bus_station)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio en metros", y = "Cantidad",
       title = "Distribución de la distancia a Transmilenio") +
  theme_bw()
ggplotly(p4)


## todo lo anterior pero para el poblado ----
polygon_trans <- getbb(place_name = "Comuna 14 - El Poblado", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") 

housing <- st_as_sf(x=train_final_V2,coords=c("lon","lat"),crs=4326) 
housing_po <- st_intersection(polygon_trans,housing)


polygon_trans <- getbb(place_name = "Comuna 14 - El Poblado", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") 


## convertir en sf
housing_po <- st_as_sf(x=train_final_V2,coords=c("lon","lat"),crs=4326)
housing_poblado <-  st_intersection(polygon_trans, housing_po)
class(housing_poblado)



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station

## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="landuse" , value="retail") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
retail = osm_sf$osm_polygons %>% select(osm_id,amenity)
retail



parques <- opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)



## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm3 = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="amenity" , value="food_court") 
class(osm2)
## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3

## Obtener un objeto sf
food_court = osm_sf3$osm_points %>% select(osm_id,amenity)
food_court



bus_station_poblado <- st_intersection(polygon_trans, bus_station)
parks_poblado <- st_intersection(polygon_trans, parques_geometria)
food_court_poblado <- st_intersection(polygon_trans, food_court)
retail_poblado <- st_intersection(polygon_trans, retail)


# Calculamos el centroide de cada parque
centroides_parques <- gCentroid(as(parks_poblado$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = parks_poblado, col = "green",
              opacity = 0.8, popup = parks_poblado$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)

# Calculamos el centroide de cada parque
centroides_transmilenio <- gCentroid(as(bus_station_poblado$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = bus_station_poblado, col = "green",
              opacity = 0.8, popup = bus_station_poblado$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)






## Pintar las estaciones de autobus, la localidad y los aptos
map <- leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = polygon_trans, color = "blue")%>% 
  addCircleMarkers(data=bus_station_poblado , col="red")%>%
  addPolygons(data=retail_poblado , col="brown")%>%
  addPolygons(data=parks_poblado , col="green")%>% 
  addCircleMarkers(data=housing_poblado , col="yellow" , label=housing_poblado$title, radius= 0.25)
addLayersControl(
  baseGroups = c("Open Street", "World Imagery")
)
map


centroides_sf_parks <- st_as_sf(centroides_parques, coords = c("x", "y"))
centroides_sf_bus_station<- st_as_sf(centroides_transmilenio, coords = c("x", "y"))


dist_matrix_parks <- st_distance(x = housing_poblado, y = centroides_sf_parks)
dist_matrix_bus_station <- st_distance(x = housing_poblado, y = centroides_sf_bus_station)

# Encontramos la distancia mínima a un parque
dist_min_parks <- apply(dist_matrix_parks, 1, min)
dist_min_bus_station <- apply(dist_matrix_bus_station, 1, min)
housing_poblado$distancia_parque <- dist_min_parks
housing_poblado$distancia_bus_station <- dist_min_bus_station




p <- ggplot(housing_poblado, aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a un parque y el valor del inmueble en El Poblado") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)


p2 <- ggplot(housing_chapinero, aes(x = distancia_bus_station, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una transporte en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a una estación de transporte y el valor del inmueble en Chapinero") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2)


p3 <- ggplot(housing_chapinero, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p3)

p4 <- ggplot(housing_chapinero, aes(x = distancia_bus_station)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio en metros", y = "Cantidad",
       title = "Distribución de la distancia a Transmilenio") +
  theme_bw()
ggplotly(p4)


## ---- regresiones ----
colnames(housing_chapinero)
ols_1 <- lm(price~)


map