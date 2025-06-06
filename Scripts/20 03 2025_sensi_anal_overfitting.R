#Análisis de sensibilidad/paramétrico ===================================================================================

install.packages("MASS")
library(MASS)

#REGRESIÓN LINEAL: Modelo optimizado
modelo_lr_opt <- stepAIC(modelo_lr, direction = "both") # "both" busca el mejor subconjunto de variables
summary(modelo_lr_opt)


#REGRESIÓN LINEAL: Modelo ajustado=====================================

# Entrenar un modelo de regresión lineal
modelo_lr_ajustado <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                  In_temp_1 + 
                  Out_temp_1 + Out_temp_2, 
                data = train_data)


# Resumen del modelo
summary(modelo_lr_ajustado)


# Hacer predicciones con el conjunto de prueba
predicciones_lr_ajustado <- predict(modelo_lr_ajustado, newdata = test_data)


# Ver las primeras predicciones
head(predicciones_lr_ajustado)


# Valores reales (soin los valores reales de temperatura del subdataset test)
valores_reales_lr_ajustado <- test_data$Temp


# Calcular residuos (diferencia entre valores reales y las predicciones)
residuos_lr_ajustado <- valores_reales_lr_ajustado  - predicciones_lr_ajustado 


# Calcular métricas de error
mse_lr_ajustado  <- mean(residuos_lr_ajustado ^2)         # Error Cuadrático Medio (MSE)
rmse_lr_ajustado  <- sqrt(mse_lr_ajustado )               # Raíz del Error Cuadrático Medio (RMSE)
mae_lr_ajustado  <- mean(abs(residuos_lr_ajustado ))      # Error Absoluto Medio (MAE)


# Imprimir resultados
cat("MSE:", mse_lr_ajustado , "\n")
cat("RMSE:", rmse_lr_ajustado , "\n")
cat("MAE:", mae_lr_ajustado , "\n")


# Graficar predicciones vs. valores reales
plot(valores_reales_lr_ajustado , predicciones_lr_ajustado , 
     main="Regresión lineal: Predicciones vs Valores Reales (ajustado)",
     xlab="Valores Reales (Temperatura)", 
     ylab="Predicciones (Temperatura)",
     pch=19, col="blue")

# Agregar una línea de referencia (y = x) para ver el ajuste perfecto
abline(a=0, b=1, col="red", lwd=2)  # Línea roja de referencia


#REGRESIÓN LINEAL: Modelo ajustado - Calcular si está sobreajustado=====================================

# Hacer predicciones con el conjunto de prueba
predicciones_lr_ajustado <- predict(modelo_lr_ajustado, newdata = test_data)

# Hacer predicciones con el conjunto de entrenamiento
predicciones_train_lr_ajustado <- predict(modelo_lr_ajustado, newdata = train_data)

# Valores reales (solo los valores reales de temperatura del subdataset test y training)
valores_reales_lr_ajustado <- test_data$Temp
valores_reales_train_lr_ajustado <- train_data$Temp

# Calcular residuos (diferencia entre valores reales y las predicciones)
residuos_lr_ajustado <- valores_reales_lr_ajustado - predicciones_lr_ajustado
residuos_train_lr_ajustado <- valores_reales_train_lr_ajustado - predicciones_train_lr_ajustado

# Calcular métricas de error en el conjunto de prueba
mse_lr_ajustado_test <- mean(residuos_lr_ajustado^2)         # MSE en el conjunto de prueba
rmse_lr_ajustado_test <- sqrt(mse_lr_ajustado_test)          # RMSE en el conjunto de prueba
mae_lr_ajustado_test <- mean(abs(residuos_lr_ajustado))      # MAE en el conjunto de prueba

# Calcular métricas de error en el conjunto de entrenamiento
mse_lr_ajustado_train <- mean(residuos_train_lr_ajustado^2)         # MSE en el conjunto de entrenamiento
rmse_lr_ajustado_train <- sqrt(mse_lr_ajustado_train)               # RMSE en el conjunto de entrenamiento
mae_lr_ajustado_train <- mean(abs(residuos_train_lr_ajustado))      # MAE en el conjunto de entrenamiento

# Imprimir resultados de las métricas de error para ambos conjuntos
cat("MSE (Prueba):", mse_lr_ajustado_test, "\n")
cat("RMSE (Prueba):", rmse_lr_ajustado_test, "\n")
cat("MAE (Prueba):", mae_lr_ajustado_test, "\n")

cat("MSE (Entrenamiento):", mse_lr_ajustado_train, "\n")
cat("RMSE (Entrenamiento):", rmse_lr_ajustado_train, "\n")
cat("MAE (Entrenamiento):", mae_lr_ajustado_train, "\n")

# Comparar los resultados para detectar sobreajuste
if (mse_lr_ajustado_train < mse_lr_ajustado_test) {
  cat("El modelo podría estar sobreajustado.\n")
} else {
  cat("El modelo parece estar generalizando bien.\n")
}





















