# ProblemSet3
 El presente documento pretende realizar la predicción del precio de las viviendas en Chapinero (Bogotá) y El Poblado (Medellín) por medio de datos espaciales suministrados por el docente y datos espaciales obtenidos en OpenStreetMap.
Se realizaron pruebas con 5 modelos (dos XGboost, dos Random Forest y un OSL) del cual se definió escoge el modelo bajo la metodología XGboost que utiliza todas las variables planteadas.

Sript 1 : Se hace limpieza de base de datos

Script 2 : Se corren los modelos de análisis para identificar predictores de los precios de los inmuebles.

Se utiliza OLS para identificar un efecto coeherente de las variables independientes sobre la Y.
Se utiliza modelo RandomForest y Xgboost para identificar el ajuste del modelo propuesto.
Se identifica el siguiente modelo para predecir precios de vivienda en las localidades de Chapinero (Bogotá, Colombia) y El Poblado (Medellín, Colombia)

# Precio=Area+Num_Baños+Num_habitaciones+Dist_parque +Dist_bares +Dist_comercio
