library(raster)
library(caret)

setwd("~/Documents/ISL/cuda")


# Función para extraer características de una imagen
extract_features <- function(image) {
  if (nlayers(image) == 3) {
    # Convertir la imagen a matriz
    image_array <- as.array(image)
    
    # Extraer cada banda RGB por separado
    red_band <- image_array[,,1]
    green_band <- image_array[,,2]
    blue_band <- image_array[,,3]
    
    # Calcular características
    mean_red <- mean(red_band)
    mean_green <- mean(green_band)
    mean_blue <- mean(blue_band)
    sd_red <- sd(red_band)
    sd_green <- sd(green_band)
    sd_blue <- sd(blue_band)
    
    return(c(mean_red, mean_green, mean_blue, sd_red, sd_green, sd_blue))
  } else {
    # Si no es una imagen RGB, extraer valores de píxeles y calcular estadísticas
    pixel_values <- getValues(image)
    return(c(mean(pixel_values), sd(pixel_values)))
  }
}



interpolate_bilinear <- function(features, resolution_ratio) {
  # Convertir a raster layer
  features_raster <- raster(matrix(features, nrow = 1))
  
  # Definir la resolución objetivo
  target_resolution <- c(resolution_5m, resolution_5m)
  
  # Interpolar con el método bilineal usando resample
  interpolated_features <- resample(features_raster, target_resolution, method = "bilinear")
  
  # Convertir de nuevo a un vector
  interpolated_features_values <- as.vector(values(interpolated_features))
  
  return(interpolated_features_values)
}

# Cargar archivos TIFF
shade_5m <- raster("shade_5.tif")
shade_30m <- raster("shade_30.tif")


features_5m <- extract_features(shade_5m)
features_30m <- extract_features(shade_30m)



# Muestrear o interpolar las características de 30m para que coincidan con las de 5m
# Por simplicidad, aquí solo se copian las características de 30m, pero deberías usar un método de muestreo o interpolación adecuado


# Definir la resolución de 30 metros y 5 metros
resolution_30m <- 30
resolution_5m <- 5

# Muestrear o interpolar las características de 30m para que coincidan con las de 5m
# Por simplicidad, aquí usamos una interpolación bilineal simulada
sampled_features_30m <- interpolate_bilinear(features_30m, resolution_30m / resolution_5m)# Ahora puedes proceder a utilizar sampled_features_30m en tu análisis



sampled_features_30m <- features_30m  # Aquí puedes aplicar tu método de muestreo o interpolación

# Construir conjunto de datos combinado
# Suponiendo que 'shade_5m_values' y 'shade_30m_values' son las etiquetas de shade relief de 5m y 30m respectivamente
data <- data.frame(Shade_5m = shade_5m_values,
                   Shade_30m = shade_30m_values,
                   Features_5m = features_5m,
                   Features_30m = sampled_features_30m)

# Dividir datos en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
train_index <- sample(nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Entrenar un modelo de regresión lineal
model <- lm(Shade_5m ~ ., data = train_data)

# Evaluar el modelo
predicted_values <- predict(model, test_data)
mse <- mean((predicted_values - test_data$Shade_5m)^2)
print(paste("Mean Squared Error:", mse))
