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
                                             pattern = "[:space:]+m" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "mts" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "m2" , replacement = "")
}

for (i in 1:nrow(test_final)) {
  test_final$new_surface[i]<-str_replace_all(string = test_final$new_surface[i] , 
                                             pattern = "m" , replacement = "")
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

## more patterns
h = "[:digit:]+[:space:]+m2"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= h),
                              new_surface))
sum(table(train_final$new_surface))

l = "[:digit:]+[:space:]+mts"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= l),
                              new_surface))
sum(table(train_final$new_surface))

k = "[:digit:]+[:space:]+metros"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= k),
                              new_surface))
sum(table(train_final$new_surface))

m = "[:digit:]+mts"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= m),
                              new_surface))
sum(table(train_final$new_surface))

n = "[:digit:]+m2"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= n),
                              new_surface))
sum(table(train_final$new_surface))

t = "[:digit:]+[:space:]+m"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= t),
                              new_surface))
sum(table(train_final$new_surface))

q = "[:digit:]+m"
train_final = train_final %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=train_final$description , pattern= q),
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
                                              pattern = "[:space:]+m" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "mts" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "m2" , replacement = "")
}

for (i in 1:nrow(train_final)) {
  train_final$new_surface[i]<-str_replace_all(string = train_final$new_surface[i] , 
                                              pattern = "m" , replacement = "")
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
#Limpio variables que no sirven

#borrar variables q no se consideran útiles
train_final<-train_final %>% 
  select(-"surface_covered",-"new_surface",-"surface_new_3",-"surface_new_4",
         -"surface_new_5",-"surface_new_6",-"surface_new_15"
         ,-"surface_new_16",-"new_bathrooms",-"bathrooms_new_2",-"bathrooms_new_3"
         ,-"bathrooms_new_4",-"bathrooms_new_5",-"bathrooms_new_6",-"bathrooms_new_7")

test_final<-test_final %>% 
  select(-"surface_covered",-"new_surface",-"surface_new_3",-"surface_new_4",
         -"surface_new_5",-"surface_new_6",-"surface_new_7"
         ,-"new_bathrooms",-"bathrooms_new_2",-"bathrooms_new_3"
         ,-"bathrooms_new_4",-"bathrooms_new_5",-"bathrooms_new_6")

saveRDS(train_final,file = "train_final_V3.rds")
saveRDS(test_final,file = "test_final_V3.rds")

#importar bases datos limpias ----

test_final <- readRDS("test_final_V3.rds")
train_final <- readRDS("train_final_V3.rds")

## convertir en sf
test_final <- st_as_sf(x=test_final,coords=c("lon","lat"),crs=4326)
train_final <- st_as_sf(x=train_final,coords=c("lon","lat"),crs=4326)

colnames(train_final)


#elacticnet----
el <- train(
  price ~ bedrooms+bathrooms+surface_total, data = train_final, method = "glmnet",
  trControl = trainControl("cv", number = 10), preProcess = c("center", "scale")
)
el

#superlearner----

require("tidyverse")
require("ranger")
require("SuperLearner")
# set the seed for reproducibility
set.seed(123)
# generate the observed data
n = 1000
x = runif(n, 0, 8)
y = 5 + 4 * sqrt(9 * x) * as.numeric(x <
                                       2) + as.numeric(x >= 2) * (abs(x - 6)^(2)) +
  rlaplace(n)
D <- data.frame(x, y) # observed data

xl <- seq(0, 8, 0.1)
yl <- (5 + 4 * sqrt(9 * xl) * as.numeric(xl < 2) + as.numeric(xl >= 2) * 
         (abs(xl -6)^(2)))
Dl <- data.frame(xl, yl) # for plotting the true data


# Specify the number of folds for
# V-fold cross-validation
folds = 5
## split data into 5 groups for 5-fold
## cross-validation we do this here so
## that the exact same folds will be
## used in both the SL fit with the R
## package, and the hand coded SL
index <- split(1:1000, 1:folds)
splt <- lapply(1:folds, function(ind) D[index[[ind]], ])

# view the first 6 observations in the # first [[1]] and second [[2]] folds
head(splt[[1]])

head(splt[[2]])

# Fit using the SuperLearner package,
# specify ntntoutcome-for-prediction
# (y), the predictors (x), the loss
# function (L2), ntntthe library
# (sl.lib), and number of folds
fitY <- SuperLearner(Y = y, X = data.frame(x),
                     method = "method.NNLS", SL.library = c("SL.lm", "SL.ranger"),
                     cvControl = list(V = folds, validRows = index))
# View the output: 'Risk' column
# returns the CV-MSE estimates
# 'Coef' column gives the weights # for the final SuperLearner
# (meta-learner)
fitY

# Now predict the outcome for all
# possible x
yS <- predict(fitY, newdata = data.frame(x = xl),onlySL = T)$pred
# Create a dataframe of all x
# and predicted SL responses
Dl1 <- data.frame(xl, yS)

library(nnls)
library(ranger)
library(data.table)
#------------------------------------------------------------
# Hand-coding Super Learner
#------------------------------------------------------------
## 2: the lapply() function is an
## efficient way to rotate through the
## folds to execute the following:
## (a) set the ii-th fold to be the
## validation set; (b) fit each
## algorithm on the training set, which
## is what's left after taking the
## ii-th fold out (i.e., splt[-ii]);
## (c) obtain the predicted outcomes
## for observations in the validation
## set; (d) estimate the estimated risk
## (CV-MSE) for each fold 2b: fit each
## algorithm on the training set (but
## not the ii-th validation set)
m1 <- lapply(1:folds, function(ii) lm(y ~x, data = rbindlist(splt[-ii])))
m2 <- lapply(1:folds, function(ii) ranger(y~x,data = rbindlist(splt[-ii])))

## 2c: predict the outcomes for
## observation in the ii-th validation
## set
p1 <- lapply(1:folds, function(ii) predict(m1[[ii]], newdata = rbindlist(splt[ii])))
p2 <- lapply(1:folds, function(ii) predict(m2[[ii]], data = rbindlist(splt[ii]))$predictions)
# add the predictions to grouped # dataset 'splt'
for (i in 1:folds) f
splt[[i]] <- cbind(splt[[i]], p1[[i]],
                   p2[[i]])
g
# view the first 6 observations in the # first fold column2 (y) is the
# observed outcome; column3 is the
# CV-predictions from lm column4 is # the CV-predictions from ranger;
head(splt[[1]])

## 2d: calculate CV risk for each
## method for with ii-th validation set
## our loss function is L2-squared
## error; so our risk is mean squared
## error
risk1 <- lapply(1:folds, function(ii) mean((splt[[ii]][,2] - splt[[ii]][, 3])^2))
risk2 <- lapply(1:folds, function(ii) mean((splt[[ii]][, 2] - splt[[ii]][, 4])^2))
## 3: average the estimated risks
## across the 5 folds to obtain 1
## measure of performance for each
## algorithm
a <- rbind(cbind("lm", mean(do.call(rbind, risk1), na.rm = T)),
           cbind("RF", mean(do.call(rbind,risk2), na.rm = T)))

# checking to see match with SL output
fitY
## 4: estimate SL weights using nnls
## (for convex combination) and
## normalize
# create a new datafame with the
# observed outcome (y) and
# CV-predictions from the 3 algorithms
X <- data.frame(do.call(rbind, splt))[, -1]
names(X) <- c("y", "lm", "RF")
head(X)

SL.r <- nnls(cbind(X[, 2], X[, 3]), X[, 1])$x
alpha <- as.matrix(SL.r/sum(SL.r))
alpha

# compare to the package's coefficients
fitY$coef

## 5a: fit all algorithms to original
## data and generate predictions
m1 <- lm(y~x, data = D)
m2 <- ranger(y ~ x, data = D)
## 5b: predict from each fit using all
## data
p1 <- predict(m1, newdata = D)
p2 <- predict(m2, data=D)$predictions
predictions <- cbind(p1, p2)

## 5c: for the observed data take a
## weighted combination of predictions
## using nnls coeficients as weights
y_pred <- predictions %*% alpha
head(y_pred)

## now apply to new dataset
## (xl)) to verify that our work
## predicts similar results as actual
## SL function
p1 <- predict(m1, newdata = data.frame(x = xl))
p2 <- predict(m2, data = data.frame(x = xl))$predictions
predictions <- cbind(p1, p2)
yS2 <- predictions %*% alpha

# now we have a new dataframe of # (xl) and SL manual predicted outcome
Dl2 <- data.frame(xl, yS2)
head(Dl2)
head(Dl1)




