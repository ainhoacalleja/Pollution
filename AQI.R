# Función para calcular el AQI (Air Quality Index)de un contaminante
calcular_aqi <- function(concentracion, limites) {
  C_low <- limites[1]  # Límite inferior del intervalo
  C_high <- limites[2] # Límite superior del intervalo
  I_low <- limites[3]  # AQI inferior
  I_high <- limites[4] # AQI superior
  
  aqi <- ((concentracion - C_low) / (C_high - C_low)) * (I_high - I_low) + I_low
  return(aqi)
}

# Definir los límites para PM2.5, NO2 y O3 (como ejemplo)
# Estos límites deben adaptarse a las normativas locales o internacionales. En cada vector: C_low, C_high, I_low y I_high,
limites_pm25 <- c(0, 12, 0, 50)        # PM2.5: 0-12 µg/m³  
limites_no2 <- c(0, 53, 0, 50)         # NO2: 0-53 ppb
limites_o3 <- c(0, 60, 0, 50)          # O3: 0-60 ppb

# Supongamos que tenemos las concentraciones de estos contaminantes
pm25_conc <- 10  # Concentración de PM2.5 en µg/m³
no2_conc <- 45   # Concentración de NO2 en ppb
o3_conc <- 55    # Concentración de O3 en ppb

# Calcular el AQI para cada contaminante
aqi_pm25 <- calcular_aqi(pm25_conc, limites_pm25)
aqi_no2 <- calcular_aqi(no2_conc, limites_no2)
aqi_o3 <- calcular_aqi(o3_conc, limites_o3)

# El AQI total será el valor máximo de los tres
aqi_total <- max(aqi_pm25, aqi_no2, aqi_o3)

# Mostrar los resultados
cat("AQI para PM2.5:", aqi_pm25, "\n")
cat("AQI para NO2:", aqi_no2, "\n")
cat("AQI para O3:", aqi_o3, "\n")
cat("AQI total:", aqi_total, "\n")
