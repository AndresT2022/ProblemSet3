## load packages  -----    
rm(list=ls())
require(pacman)
p_load(rio, randomForest,caret, tidyverse , sf , leaflet , osmdata , nngeo, rgeos, ggplot2, plotly, dplyr, tidyr)
getwd()
#setwd("C:\\Users\\DELL\\OneDrive - Universidad de los Andes\\MECA 2022_2023\\BIGDATA\\TALLERES\\ProblemSet3")
#setwd("/Users/ignacioserrano/Documents/Maestria/Big Data/GitHub/ProblemSet3")

## load data
train <- import("train_final_V3.Rds") %>% mutate(base="train")
test <- import("test_final_V3.Rds") %>% mutate(base="test")
db_sf <- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)

## get polygons ----
poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") 
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon")  %>% .$multipolygon
class(chapinero)
### Crop chapinero ----
db_ch <- db_sf[chapinero,]

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
  add_osm_feature(key="amenity" , value="bar") 
class(osm3)
## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3

## Obtener un objeto sf
food_court = osm_sf3$osm_points %>% select(osm_id,amenity)
food_court



bus_station_chapinero <- st_intersection(chapinero, bus_station)
parks_chapinero <- st_intersection(chapinero, parques_geometria)
food_court_chapinero <- st_intersection(chapinero, food_court)
retail_chapinero <- st_intersection(chapinero, retail)


# Calculamos el centroide de cada parque
centroides_parques <- gCentroid(as(parks_chapinero$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = parks_chapinero, col = "green",
              opacity = 0.8, popup = parks_chapinero$name) %>%
  addCircles(lng = centroides_parques$x, 
             lat = centroides_parques$y, 
             col = "red", opacity = 1, radius = 1)

# Calculamos el centroide de cada bus station
centroides_transmilenio <- gCentroid(as(bus_station_chapinero$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addCircles(data = bus_station_chapinero, col = "green",
              opacity = 0.8, popup = bus_station_chapinero$name) %>%
  addCircles(lng = centroides_transmilenio$x, 
             lat = centroides_transmilenio$y, 
             col = "red", opacity = 1, radius = 1)

# centroides retails chapinero
centroides_retail <- gCentroid(as(retail_chapinero$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = retail_chapinero, col = "green",
             opacity = 0.8, popup = retail_chapinero$name) %>%
  addCircles(lng = centroides_retail$x, 
             lat = centroides_retail$y, 
             col = "red", opacity = 1, radius = 1)

# centroides bares chapinero
centroides_bar <- gCentroid(as(food_court_chapinero$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addCircles(data = food_court_chapinero, col = "green",
              opacity = 0.8, popup = retail_chapinero$name) %>%
  addCircles(lng = centroides_bar$x, 
             lat = centroides_bar$y, 
             col = "red", opacity = 1, radius = 1)

## Pintar las estaciones de autobus, la localidad y los aptos
leaflet() %>%
  addTiles(group = "Open Street")%>% 
  addPolygons(data = chapinero, color = "blue")%>% 
  addCircleMarkers(data=bus_station_chapinero , col="red")%>%
  addPolygons(data=retail_chapinero , col="brown")%>%
  addPolygons(data=parks_chapinero , col="green")%>%
  addCircles(data=food_court_chapinero, color = "purple")
  addCircleMarkers(data=db_ch , col="yellow" , label=db_ch$title, radius= 0.25)
addLayersControl(
  baseGroups = c("Open Street", "World Imagery")
)



centroides_sf_parks <- st_as_sf(centroides_parques, coords = c("x", "y"))
centroides_sf_bus_station<- st_as_sf(centroides_transmilenio, coords = c("x", "y"))
centroides_sf_retail<- st_as_sf(centroides_retail, coords = c("x", "y"))
centroides_sf_bar<- st_as_sf(centroides_bar, coords = c("x", "y"))

dist_matrix_parks <- st_distance(x = db_ch, y = centroides_sf_parks)
dist_matrix_bus_station <- st_distance(x = db_ch, y = centroides_sf_bus_station)
dist_matrix_retail <- st_distance(x = db_ch, y = centroides_sf_retail)
dist_matrix_bar <- st_distance(x = db_ch, y = centroides_sf_bar)


# Encontramos la distancia mínima a un parque
dist_min_parks <- apply(dist_matrix_parks, 1, min)
dist_min_bus_station <- apply(dist_matrix_bus_station, 1, min)
dist_min_retail <- apply(dist_matrix_retail, 1, min)
dist_min_bar <- apply(dist_matrix_bar, 1, min)


db_ch$distancia_parque <- dist_min_parks
db_ch$distancia_bus_station <- dist_min_bus_station
db_ch$distancia_retail <- dist_min_retail
db_ch$distancia_bar <- dist_min_bar


p <- ggplot(db_ch, aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a un parque y el valor del inmueble en Chapinero") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)


p2 <- ggplot(db_ch, aes(x = distancia_bus_station, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una transporte en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a una estación de transporte y el valor del inmueble en Chapinero") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2)


p3 <- ggplot(db_ch, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p3)

p4 <- ggplot(db_ch, aes(x = distancia_bus_station)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio en metros", y = "Cantidad",
       title = "Distribución de la distancia a Transmilenio") +
  theme_bw()
ggplotly(p4)

## crop Poblado ----
db_pob <- db_sf[poblado,]

## obtener la caja de coordenada que contiene el polígono de El Poblado
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm3 = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="leisure" , value="park") 
class(osm3)
## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3
      
## obtener la caja de coordenada que contiene el polígono de Medellin
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
bus_station_p = osm_sf$osm_points %>% select(osm_id,amenity)
bus_station_p

## Retail
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="landuse" , value="retail") 
class(osm)
## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
## Obtener un objeto sf
retail_p = osm_sf$osm_polygons %>% select(osm_id,landuse)
retail_p

parques_p <- opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_p_sf <- osmdata_sf(parques_p)
parques_p_geometria <- parques_p_sf$osm_polygons %>% 
  select(osm_id, name)

## nightclub

opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar") 
class(osm)
## extraer Simple Features Collection
osm_sf4 = osm %>% osmdata_sf()
osm_sf4
## Obtener un objeto sf
bar_p = osm_sf4$osm_points %>% select(osm_id,amenity)
bar_p



## Food court
opq(bbox = getbb("Medellín, Colombia"))
## objeto osm
osm3 = opq(bbox = getbb("Medellín, Colombia")) %>%
  add_osm_feature(key="amenity" , value="food_court") 
class(osm3)
## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3

## Obtener un objeto sf
food_court_p = osm_sf3$osm_points %>% select(osm_id,amenity)
food_court_p



bus_station_poblado <- st_intersection(poblado, bus_station_p)
parks_poblado <- st_intersection(poblado, parques_p_geometria)
food_court_poblado <- st_intersection(poblado, food_court_p)
retail_poblado <- st_intersection(poblado, retail_p)
nightclub_poblado <- st_intersection(poblado, night_p)
bar_poblado <- st_intersection(poblado, bar_p)

##Mapa 1 medellin ----      
leaflet() %>% addTiles() %>%
        addPolygons(data=poblado,color="red") %>% 
        addPolygons(data=parks_poblado,color="green") %>% 
        addCircleMarkers(data=bus_station_poblado,color="blue") %>% 
        addPolygons(data=retail_poblado,color="brown") %>% 
        addCircles(data=db_pob, color = "yellow") %>% 
  addCircles(data=food_court_poblado, color="purple") %>% 
  addCircles(data=bar_poblado, color = "black")
  
      
# Calculamos el centroide de cada parque
centroides_parques_p <- gCentroid(as(parks_poblado$geometry, "Spatial"), byid = T)
leaflet() %>%
addTiles() %>%
addPolygons(data = parks_poblado, col = "green",
                    opacity = 0.8, popup = parks_poblado$name) %>%
        addCircles(lng = centroides_parques_p$x, 
                   lat = centroides_parques_p$y, 
                   col = "red", opacity = 1, radius = 1)  
      
#centroids retail
centroides_retail_p <- gCentroid(as(retail_poblado$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = retail_poblado, col = "brown",
              opacity = 0.8, popup = retail_poblado$name) %>%
  addCircles(lng = centroides_retail_p$x, 
             lat = centroides_retail_p$y, 
             col = "red", opacity = 1, radius = 1)       
      
#centroids bar
centroides_bar_p <- gCentroid(as(bar_poblado$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addCircles(data = bar_poblado, col = "black",
              opacity = 0.8, popup = bar_poblado$name) %>%
  addCircles(lng = centroides_bar_p$x, 
             lat = centroides_bar_p$y, 
             col = "red", opacity = 1, radius = 1)       

centroides_sfp_parks <- st_as_sf(centroides_parques_p, coords = c("x", "y"))
centroides_sfp_retail<- st_as_sf(centroides_retail_p, coords = c("x", "y"))
centroides_sfp_bar<- st_as_sf(centroides_bar_p, coords = c("x", "y"))


dist_matrix_park_p <- st_distance(x = db_pob, y = centroides_sfp_parks)
dist_matrix_retail_p <- st_distance(x = db_pob, y = centroides_sfp_retail)
dist_matrix_bar_p <- st_distance(x = db_pob, y = centroides_sfp_bar)


# Encontramos la distancia mínima a un parque
dist_min_parks_p <- apply(dist_matrix_park_p, 1, min)
dist_min_retail_p <- apply(dist_matrix_retail_p, 1, min)
dist_min_bar_p <- apply(dist_matrix_bar_p, 1, min)


db_pob$distancia_parque <- dist_min_parks_p
db_pob$distancia_retail <- dist_min_retail_p
db_pob$distancia_bar <- dist_min_bar_p




p_p <- ggplot(db_pob, aes(x = distancia_parque, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad a un parque y el valor del inmueble en El Poblado") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p_p)


p2_p <- ggplot(db_pob, aes(x = distancia_retail, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a comercio en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre la proximidad al comercio y el valor del inmueble en El Poblado") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2_p)


p3_p <- ggplot(db_pob, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques en El Poblado") +
  theme_bw()
ggplotly(p3_p)

p4_p <- ggplot(db_pob, aes(x = distancia_retail)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima al comercio en metros", y = "Cantidad",
       title = "Distribución de la distancia al comercio en El Poblado") +
  theme_bw()
ggplotly(p4_p)     
      
      
      
 ### Unir bases -------  
db_ch <- select(db_ch, -distancia_bus_station)
final_base <- rbind(db_ch, db_pob)     

##Creacion de modelos ----
test <- subset(final_base, base == "test")
train <- subset(final_base, base == "train") 
#modelo 1
model_1 <- lm(price~surface_total+bathrooms+bedrooms+distancia_parque+distancia_retail+
                distancia_bar, data = train)

scatter <- ggplot(train, aes(x = surface_total, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Superficie en metros (log-scale)", 
       y = "Valor del inmueble (log-scale)",
       title = "Relación entre superficie y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(scatter)

#modelo 2

require("xgboost")

#Selección muestra de entrenamiento y prueba

id_train <- sample(1:nrow(train),size = 0.7*nrow(train), replace = F)
train_init<-train[id_train,]
evaluation<-train[-id_train,]


grid_default <- expand.grid(nrounds = c(250,500),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))


set.seed(1410)
ctrl <- makeTuneControlRandom(maxit = 10L)
xgboost <- train(
  price~surface_total+bathrooms+bedrooms+distancia_parque+distancia_retail+
    distancia_bar,
  data = train_init,
  method = "xgbTree",
 # trControl = ctrl,
  metric = "Sens",
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)



