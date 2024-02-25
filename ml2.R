library(randomForest)
library(rgdal)
library(sp)
library(raster)
library(terra)
library(rgdal)
memory.limit(size=8000)
# Carga de los datos
dem_30m <- raster("mde_30.tif")  # DEM de 30 metros
dem_reducido <- raster("mde_5.tif")  # DEM de la zona reducida

setwd("~/Documents/ISL/cuda")
memory.limit(size=8000)


# Definir la resolución deseada
resolucion_deseada <- 5  # en metros

# Calcular la resolución actual del raster dem_30m
resolucion_actual <- res(dem_30m)[1]  # Obtener la resolución en la primera dimensión

# Calcular el factor de escala para llegar a la resolución deseada
factor_escala <- resolucion_actual / resolucion_deseada

# Calcular el tamaño de celda para la resolución deseada
tamano_celda_deseado <- c(resolucion_deseada, resolucion_deseada)

# Crear un raster vacío con la misma extensión y CRS que dem_30m y la resolución deseada
raster_resolucion_deseada <- raster(extent(dem_30m), res = tamano_celda_deseado, crs = crs(dem_30m))

# Realizar el remuestreo del raster dem_30m a la resolución deseada
dem_resampled <- resample(dem_30m, raster_resolucion_deseada, method = "bilinear")




# Extracción de características
slope <- raster::terrain(dem_reducido, opt="slope")
aspect <- raster::terrain(dem_reducido, opt="aspect")
#curvature <- raster::terrain(dem_reducido, opt="curvature")

#curvature <- curvature(dem_30m)
#ndvi <- ndvi(imagen_satelital_1, imagen_satelital_2)
#swi <- swi(imagen_satelital_3, imagen_satelital_4)

# Preparación de los datos
datos <- data.frame(altura_5m = values(dem_reducido), 
                    slope = values(slope), aspect = values(aspect))
datos$y <- datos$altura_5m


# División de los datos en conjuntos de entrenamiento, validación y prueba
set.seed(123)
indices <- sample(1:nrow(datos), nrow(datos), replace=FALSE)
train_indices <- indices[1:floor(0.8*nrow(datos))]
val_indices <- indices[floor(0.8*nrow(datos))+1:floor(0.9*nrow(datos))]
test_indices <- tail(indices, n = nrow(datos) - floor(0.9 * nrow(datos)))

datos_train <- datos[train_indices, ]
datos_val <- datos[val_indices, ]
datos_test <- datos[test_indices, ]




# Entrenamiento del modelo Random Forest
#modelo_rf <- randomForest(y ~ ., data=datos_train, ntree=500, mtry=5) esto no anda porque el mde tiene nulos. hay que modificarlo
#lo que sigue es la salida momentanea...

# Imputar valores faltantes con la media de cada columna
datos_train_imputados <- apply(datos_train, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Ajustar el modelo de Random Forest con los datos imputados
modelo_rf <- randomForest(y ~ ., data = datos_train_imputados, ntree = 500, mtry = 5)


# Evaluación del modelo
prediccion_val <- predict(modelo_rf, datos_val)
rmse_val <- sqrt(mean((prediccion_val - datos_val$y)^2))

prediccion_test <- predict(modelo_rf, datos_test)
rmse_test <- sqrt(mean((prediccion_test - datos_test$y)^2))

# Aplicación del modelo al DEM de 5 metros
prediccion_dem_5m <- predict(modelo_rf, newdata=data.frame(caracteristicas_dem_5m))

# Guardando el DEM de 5 metros mejorado
dem_5m_mejorado <- raster(xmn=dem_5m@xmin, xmx=dem_5m@xmax, ymn=dem_5m@ymin, ymx=dem_5m@ymax, 
                          crs=dem_5m@crs, values=prediccion_dem_5m)
writeRaster(dem_5m_mejorado, "ruta/dem_5m_mejorado_rf.tif")


