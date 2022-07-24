#Problem set 3
R.version.string
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
       osmdata,   # Get OSM's data
       class,skimr,
       stringr, # Manipular texto
       stargazer, # Output de las regresiones
       plotly, # Gráficos dinámicos
       sf,rgdal,raster,rgeos) # Get OSM's data) # Visualizaciones dinámicas 

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

##Limpieza de textos
#test_final
# Todo en minuscula
test_final$description <- tolower(test_final$description)
# Eliminamos tildes
test_final$description <- iconv(test_final$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos espacios extras
test_final$description <- gsub("\\s+", " ", str_trim(test_final$description))

#train_final
# Todo en minuscula
train_final$description <- tolower(train_final$description)
# Eliminamos tildes
train_final$description <- iconv(train_final$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos espacios extras
train_final$description <- gsub("\\s+", " ", str_trim(train_final$description))

#test_final
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
class(test_final$new_surface)
test_final$new_surface <- as.numeric(test_final$new_surface)
class(test_final$new_surface)

# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T & new_surface>15,
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

## make buffer 2
house_buf <- st_buffer(test_final,dist=55)

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

## make buffer 3
house_buf <- st_buffer(test_final,dist=120)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_5=mean(surface_total.y,na.rm=T))

test_final <- left_join(test_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_5,
                                surface_total))

table(is.na(test_final$surface_total))

## make buffer 4
house_buf <- st_buffer(test_final,dist=200)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_6=mean(surface_total.y,na.rm=T))

test_final <- left_join(test_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_6,
                                surface_total))

table(is.na(test_final$surface_total))

## make buffer 5
house_buf <- st_buffer(test_final,dist=450)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_7=mean(surface_total.y,na.rm=T))

test_final <- left_join(test_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_7,
                                surface_total))

table(is.na(test_final$surface_total))


#--Baños
#test_final

table(is.na(test_final$bathrooms))
## Usar descripcion para hallar numero de baños

x1 = "[:space:]+[:digit:]+[:space:]+banos" ## pattern

test_final = test_final %>% 
  mutate(new_bathrooms = str_extract(string=test_final$description , pattern= x1))
sum(table(test_final$new_bathrooms))
## another pattern
x2 = "[:space:]+[:digit:]+[:space:]+bano"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                              str_extract(string=test_final$description , pattern= x2),
                              new_bathrooms))
sum(table(test_final$new_bathrooms))
x3 = "dos banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                              str_extract(string=test_final$description , pattern= x3),
                              new_bathrooms))
sum(table(test_final$new_bathrooms))

x4 = "tres banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                              str_extract(string=test_final$description , pattern= x4),
                              new_bathrooms))
sum(table(test_final$new_bathrooms))
x5 = "cuatro banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                              str_extract(string=test_final$description , pattern= x5),
                              new_bathrooms))
sum(table(test_final$new_bathrooms))
x6 = "cinco banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=test_final$description , pattern= x6),
                                new_bathrooms))
sum(table(test_final$new_bathrooms))
x7 = "seis banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=test_final$description , pattern= x7),
                                new_bathrooms))
sum(table(test_final$new_bathrooms))
x8 = "siete banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=test_final$description , pattern= x8),
                                new_bathrooms))
sum(table(test_final$new_bathrooms))
x9 = "ocho banos"
test_final = test_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=test_final$description , pattern= x9),
                                new_bathrooms))
sum(table(test_final$new_bathrooms))

#volver new_surface a numeric

for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                             pattern = "dos banos" , replacement = "2")
}

for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "tres banos" , replacement = "3")
}

for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "cuatro banos" , replacement = "4")
}
for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "cinco banos" , replacement = "5")
}
for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "seis banos" , replacement = "6")
}
for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "siete banos" , replacement = "7")
}
for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "ocho banos" , replacement = "8")
}
for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "[:space:]+banos" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_bathrooms[i]<-str_replace_all(string = test_final$new_bathrooms[i] , 
                                               pattern = "[:space:]+bano" , replacement = "")
}

class(test_final$new_bathrooms)
test_final$new_bathrooms <- as.numeric(test_final$new_bathrooms)
class(test_final$new_bathrooms)
sum(table(test_final$new_bathrooms))

# unificar baños encontrados con baños existentes
test_final = test_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            new_bathrooms,
                            bathrooms))
table(is.na(test_final$bathrooms))

## make buffer 1
house_buf <- st_buffer(test_final,dist=35)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_2=median(bathrooms.y,na.rm=T))

test_final <- left_join(test_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_2,
                            bathrooms))
table(is.na(test_final$bathrooms))

## make buffer 2
house_buf <- st_buffer(test_final,dist=55)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_3=median(bathrooms.y,na.rm=T))

test_final <- left_join(test_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_3,
                            bathrooms))
table(is.na(test_final$bathrooms))

## make buffer 3
house_buf <- st_buffer(test_final,dist=120)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_4=median(bathrooms.y,na.rm=T))

test_final <- left_join(test_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_4,
                            bathrooms))
table(is.na(test_final$bathrooms))

## make buffer 4
house_buf <- st_buffer(test_final,dist=200)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_5=median(bathrooms.y,na.rm=T))

test_final <- left_join(test_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_5,
                            bathrooms))
table(is.na(test_final$bathrooms))

## make buffer 5
house_buf <- st_buffer(test_final,dist=450)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=test_final)

house_buf <- st_join(house_buf,test_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_6=median(bathrooms.y,na.rm=T))

test_final <- left_join(test_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
test_final = test_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_6,
                            bathrooms))
table(is.na(test_final$bathrooms))


#--
table(is.na(train_final$surface_total))
#train_final
## Usar descripcion para hallar metros cuadrados

x = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2" ## pattern

train_final = train_final %>% 
  mutate(new_surface = str_extract(string=train_final$description , pattern= x))
sum(table(train_final$new_surface))
## another pattern
y = "[:space:]+[:digit:]+[:space:]+metros"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= y),
                              new_surface))
sum(table(train_final$new_surface))
z = "[:space:]+[:digit:]+[:space:]+mts"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= z),
                              new_surface))
sum(table(train_final$new_surface))

#volver new_surface a numeric

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "[:space:]+m2" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "[:space:]+metros" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "[:space:]+mts" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "," , replacement = ".")
}
class(train_final$new_surface)
train_final$new_surface <- as.numeric(train_final$new_surface)
class(train_final$new_surface)

# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T & new_surface>15,
                                new_surface,
                                surface_total))
table(is.na(train_final$surface_total))
## make buffer 1
house_buf <- st_buffer(train_final,dist=35)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_3=mean(surface_total.y,na.rm=T))

train_final <- left_join(train_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_3,
                                surface_total))
table(is.na(train_final$surface_total))

## make buffer 2
house_buf <- st_buffer(train_final,dist=55)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_4=mean(surface_total.y,na.rm=T))

train_final <- left_join(train_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_4,
                                surface_total))

table(is.na(train_final$surface_total))

## make buffer 3
house_buf <- st_buffer(train_final,dist=120)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_5=mean(surface_total.y,na.rm=T))

train_final <- left_join(train_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_5,
                                surface_total))

table(is.na(train_final$surface_total))

## make buffer 4
house_buf <- st_buffer(train_final,dist=200)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_6=mean(surface_total.y,na.rm=T))

train_final <- left_join(train_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_6,
                                surface_total))

table(is.na(train_final$surface_total))

## make buffer 5
house_buf <- st_buffer(train_final,dist=500)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_15=mean(surface_total.y,na.rm=T))

train_final <- left_join(train_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_15,
                                surface_total))

table(is.na(train_final$surface_total))

## make buffer 6
house_buf <- st_buffer(train_final,dist=5000)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"surface_total"])

st_geometry(house_buf) = NULL
house_buf_mean <- house_buf %>% group_by(property_id) %>% 
  summarise(surface_new_16=mean(surface_total.y,na.rm=T))

train_final <- left_join(train_final,house_buf_mean,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,
                                surface_new_16,
                                surface_total))

table(is.na(train_final$surface_total))

#--Baños
#train_final

table(is.na(train_final$bathrooms))
## Usar descripcion para hallar numero de baños

x1 = "[:space:]+[:digit:]+[:space:]+banos" ## pattern

train_final = train_final %>% 
  mutate(new_bathrooms = str_extract(string=train_final$description , pattern= x1))
sum(table(train_final$new_bathrooms))
## another pattern
x2 = "[:space:]+[:digit:]+[:space:]+bano"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x2),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))
x3 = "dos banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x3),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))

x4 = "tres banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x4),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))
x5 = "cuatro banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x5),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))
x6 = "cinco banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x6),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))
x7 = "seis banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x7),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))
x8 = "siete banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x8),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))
x9 = "ocho banos"
train_final = train_final %>% 
  mutate(new_bathrooms = ifelse(is.na(new_bathrooms)==T,
                                str_extract(string=train_final$description , pattern= x9),
                                new_bathrooms))
sum(table(train_final$new_bathrooms))

#volver new_surface a numeric

for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "dos banos" , replacement = "2")
}

for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "tres banos" , replacement = "3")
}

for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "cuatro banos" , replacement = "4")
}
for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "cinco banos" , replacement = "5")
}
for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "seis banos" , replacement = "6")
}
for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "siete banos" , replacement = "7")
}
for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "ocho banos" , replacement = "8")
}
for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "[:space:]+banos" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_bathrooms[i]<-str_replace_all(string = train_final$new_bathrooms[i] , 
                                                pattern = "[:space:]+bano" , replacement = "")
}

class(train_final$new_bathrooms)
train_final$new_bathrooms <- as.numeric(train_final$new_bathrooms)
class(train_final$new_bathrooms)
sum(table(train_final$new_bathrooms))

# unificar baños encontrados con baños existentes
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            new_bathrooms,
                            bathrooms))
table(is.na(train_final$bathrooms))

## make buffer 1
house_buf <- st_buffer(train_final,dist=35)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_2=median(bathrooms.y,na.rm=T))

train_final <- left_join(train_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_2,
                            bathrooms))
table(is.na(train_final$bathrooms))

## make buffer 2
house_buf <- st_buffer(train_final,dist=55)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_3=median(bathrooms.y,na.rm=T))

train_final <- left_join(train_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_3,
                            bathrooms))
table(is.na(train_final$bathrooms))

## make buffer 3
house_buf <- st_buffer(train_final,dist=120)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_4=median(bathrooms.y,na.rm=T))

train_final <- left_join(train_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_4,
                            bathrooms))
table(is.na(train_final$bathrooms))

## make buffer 4
house_buf <- st_buffer(train_final,dist=200)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_5=median(bathrooms.y,na.rm=T))

train_final <- left_join(train_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_5,
                            bathrooms))
table(is.na(train_final$bathrooms))

## make buffer 5
house_buf <- st_buffer(train_final,dist=500)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_6=median(bathrooms.y,na.rm=T))

train_final <- left_join(train_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_6,
                            bathrooms))
table(is.na(train_final$bathrooms))


## make buffer 6
house_buf <- st_buffer(train_final,dist=5000)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf , color="red") %>% 
  addCircles(data=train_final)

house_buf <- st_join(house_buf,train_final[,"bathrooms"])

st_geometry(house_buf) = NULL
house_buf_median <- house_buf %>% group_by(property_id) %>% 
  summarise(bathrooms_new_7=median(bathrooms.y,na.rm=T))

train_final <- left_join(train_final,house_buf_median,"property_id")
# unificar superficie encontrada con superficie
train_final = train_final %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)==T,
                            bathrooms_new_7,
                            bathrooms))
table(is.na(train_final$bathrooms))


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


