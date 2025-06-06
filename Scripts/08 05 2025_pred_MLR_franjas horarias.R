#IMPORTAR EL DATASET HORARIO =========================================================================================

install.packages("dplyr")
library(dplyr)

#AGRUPACIÓN EN FRANJAS HORARIAS DE 4h =========================================================================================

# 0. ELIMINAR COLUMNAS NO DESEADAS
Tipología11_Temp_int_hourly <- Tipología11_Temp_int_hourly %>%
  select(-Indoor_PB_Temp, -Indoor_P1_Temp, -Indoor_P2_Temp)

# 1. RENOMBRAR LA COLUMNA DE TEMPERATURA INTERIOR
Tipología11_Temp_int_hourly <- Tipología11_Temp_int_hourly %>%
  rename(Temp = Indoor_P3_Temp)

# 2. Convertir la columna DATE a formato fecha-hora (dmy_hms)
Tipología11_Temp_int_hourly <- Tipología11_Temp_int_hourly %>%
  mutate(franja_4h = floor(hour(Date) / 4) * 4,
         datetime_franja = make_datetime(year(Date), month(Date), day(Date), franja_4h))

# Agrupar por la nueva columna datetime_franja y calcular la media de las demás variables
Tipología11_Temp_4h <- Tipología11_Temp_int_hourly %>%
  group_by(datetime_franja) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Verificar los primeros resultados
head(Tipología11_Temp_4h)

#CREAR DATASET "DESFASADO" PARA LA MLR =========================================================================================


#Copiamoos el dataset (por si acaso)
dataset_4h_MLR  <- Tipología11_Temp_4h

#Nuevas columnas dias anteriores (temperatura interior)
dataset_4h_MLR <- dataset_4h_MLR %>%
  mutate(Temp_minus_1 = lag(Temp, n = 1))

dataset_4h_MLR <- dataset_4h_MLR %>%
  mutate(Temp_minus_2 = lag(Temp, n = 2))

dataset_4h_MLR <- dataset_4h_MLR %>%
  mutate(Temp_minus_3 = lag(Temp, n = 3))

#Nuevas columnas dias anteriores (temperatura exterior)
dataset_4h_MLR <- dataset_4h_MLR %>%
  mutate(Outdoor_Temperature_1_previous = lag(Outdoor_Temperature, n = 1)  )

dataset_4h_MLR <- dataset_4h_MLR %>%
  mutate(Outdoor_Temperature_2_previous = lag(Outdoor_Temperature, n = 2)  )

dataset_4h_MLR <- dataset_4h_MLR %>%
  mutate(Outdoor_Temperature_3_previous = lag(Outdoor_Temperature, n = 3)  )

#Eliminar las filas con NA
dataset_4h_MLR <- na.omit(dataset_4h_MLR)

#Renombrar columnas 
dataset_4h_MLR <- dataset_4h_MLR %>%
  rename(Out_temp_1 = Outdoor_Temperature_1_previous)

dataset_4h_MLR <- dataset_4h_MLR %>%
  rename(Out_temp_2 = Outdoor_Temperature_2_previous)

dataset_4h_MLR <- dataset_4h_MLR %>%
  rename(Out_temp_3 = Outdoor_Temperature_3_previous)

dataset_4h_MLR <- dataset_4h_MLR %>%
  rename(In_temp_1 = Temp_minus_1)

dataset_4h_MLR <- dataset_4h_MLR %>%
  rename(In_temp_2 = Temp_minus_2)

dataset_4h_MLR <- dataset_4h_MLR %>%
  rename(In_temp_3 = Temp_minus_3)

#EXPORTAR EL DATASET =========================================================================================
install.packages("writexl")
library(writexl)

write_xlsx(dataset_4h_MLR, "C:/Users/ainhoa.arriazu/Desktop/DOCS Ainhoa/mi_dataset.xlsx")

#PREPARAR GRUPOS TEST Y TRAIN PARA REGRESIÓN LINEAL MULTIPLE ===========================================================================================

install.packages("caTools")
library(caTools)

# Dividir el conjunto de datos en entrenamiento y prueba (70%-30%)
set.seed(123)  # Para asegurar la replicabilidad
split <- sample.split(dataset_4h_MLR$Temp, SplitRatio = 0.7)

# Crear los conjuntos de entrenamiento y prueba
train_data <- subset(dataset_4h_MLR, split == TRUE)
test_data <- subset(dataset_4h_MLR, split == FALSE)

#REGRESIÓN LINEAL MULTIPLE CON TODAS LAS VARIABLES===========================================================================================
# Entrenar un modelo de regresión lineal (todas las variables)
modelo_lr_4h_todasv <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                  In_temp_1 + In_temp_2 + In_temp_3 + 
                  Out_temp_1 + Out_temp_2 + Out_temp_3, 
                data = train_data)


# Resumen del modelo (todas las variables)
summary(modelo_lr_4h_todasv)

# Entrenar un modelo de regresión lineal (todas las variables)
modelo_lr_4h_todasv <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                            In_temp_1 + In_temp_2 + In_temp_3 + 
                            Out_temp_1 + Out_temp_2 + Out_temp_3, 
                          data = train_data)

# Resumen del modelo (todas las variables)
summary(modelo_lr_4h_todasv)

# Hacer predicciones con el conjunto de prueba
predicciones_lr_4h_todasv <- predict(modelo_lr_4h_todasv, newdata = test_data)


# Ver las primeras predicciones
head(predicciones_lr_4h_todasv)


# Valores reales (soin los valores reales de temperatura del subdataset test)
valores_reales_lr <- test_data$Temp


# Calcular residuos (diferencia entre valores reales y las predicciones)
residuos_lr_4h_todasv <- valores_reales_lr - predicciones_lr_4h_todasv


# Calcular métricas de error
mse_lr_4h_todasv <- mean(residuos_lr_4h_todasv^2)         # Error Cuadrático Medio (MSE)
rmse_lr_4h_todasv <- sqrt(mse_lr_4h_todasv)               # Raíz del Error Cuadrático Medio (RMSE)
mae_lr_4h_todasv <- mean(abs(residuos_lr_4h_todasv))      # Error Absoluto Medio (MAE)


# Imprimir resultados
cat("MSE:", mse_lr_4h_todasv, "\n")
cat("RMSE:", rmse_lr_4h_todasv, "\n")
cat("MAE:", mae_lr_4h_todasv, "\n")


# Graficar predicciones vs. valores reales
plot(valores_reales_lr, predicciones_lr_4h_todasv, 
     main="Regresión lineal: Predicciones vs Valores Reales",
     xlab="Valores Reales (Temperatura)", 
     ylab="Predicciones (Temperatura)",
     pch=19, col="blue")

# Agregar una línea de referencia (y = x) para ver el ajuste perfecto
abline(a=0, b=1, col="red", lwd=2)  # Línea roja de referencia





#REGRESIÓN LINEAL MULTIPLE SOLO VARIABLES EXTERIORES ===========================================================================================
# Entrenar un modelo de regresión lineal (variables exteriores)
modelo_lr_4h_extv <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                            Out_temp_1 + Out_temp_2 + Out_temp_3, 
                          data = train_data)


# Resumen del modelo (variables exteriores)
summary(modelo_lr_4h_extv)

# Hacer predicciones con el conjunto de prueba
predicciones_lr_4h_extv <- predict(modelo_lr_4h_extv, newdata = test_data)


# Ver las primeras predicciones
head(predicciones_lr_4h_extv)


# Valores reales (soin los valores reales de temperatura del subdataset test)
valores_reales_lr <- test_data$Temp


# Calcular residuos (diferencia entre valores reales y las predicciones)
residuos_lr_4h_extv <- valores_reales_lr - predicciones_lr_4h_extv


# Calcular métricas de error
mse_lr_4h_extv <- mean(residuos_lr_4h_extv^2)         # Error Cuadrático Medio (MSE)
rmse_lr_4h_extv <- sqrt(mse_lr_4h_extv)               # Raíz del Error Cuadrático Medio (RMSE)
mae_lr_4h_extv <- mean(abs(residuos_lr_4h_extv))      # Error Absoluto Medio (MAE)


# Imprimir resultados
cat("MSE:", mse_lr_4h_extv, "\n")
cat("RMSE:", rmse_lr_4h_extv, "\n")
cat("MAE:", mae_lr_4h_extv, "\n")


# Graficar predicciones vs. valores reales
plot(valores_reales_lr, predicciones_lr_4h_extv, 
     main="Regresión lineal: Predicciones vs Valores Reales",
     xlab="Valores Reales (Temperatura)", 
     ylab="Predicciones (Temperatura)",
     pch=19, col="blue")

# Agregar una línea de referencia (y = x) para ver el ajuste perfecto
abline(a=0, b=1, col="red", lwd=2)  # Línea roja de referencia
