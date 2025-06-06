#PREPARAR EL MODELO =====================================================================================================

install.packages("caTools")
library(caTools)

# Dividir el conjunto de datos en entrenamiento y prueba (70%-30%)
set.seed(123)  # Para asegurar la replicabilidad
split <- sample.split(dataset_tem_daily_ARX$Temp, SplitRatio = 0.7)

# Crear los conjuntos de entrenamiento y prueba
train_data <- subset(dataset_tem_daily_ARX, split == TRUE)
test_data <- subset(dataset_tem_daily_ARX, split == FALSE)



#1: REGRESIÓN LINEAL MULTIPLE ===========================================================================================

# Entrenar un modelo de regresión lineal
modelo_lr <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                  In_temp_1 + In_temp_2 + In_temp_3 + 
                  Out_temp_1 + Out_temp_2 + Out_temp_3, 
                data = train_data)


# Resumen del modelo
summary(modelo_lr)


# Hacer predicciones con el conjunto de prueba
predicciones_lr <- predict(modelo_lr, newdata = test_data)


# Ver las primeras predicciones
head(predicciones_lr)


# Valores reales (soin los valores reales de temperatura del subdataset test)
valores_reales_lr <- test_data$Temp


# Calcular residuos (diferencia entre valores reales y las predicciones)
residuos_lr <- valores_reales_lr - predicciones_lr


# Calcular métricas de error
mse_lr <- mean(residuos_lr^2)         # Error Cuadrático Medio (MSE)
rmse_lr <- sqrt(mse_lr)               # Raíz del Error Cuadrático Medio (RMSE)
mae_lr <- mean(abs(residuos_lr))      # Error Absoluto Medio (MAE)


# Imprimir resultados
cat("MSE:", mse_lr, "\n")
cat("RMSE:", rmse_lr, "\n")
cat("MAE:", mae_lr, "\n")


# Graficar predicciones vs. valores reales
plot(valores_reales_lr, predicciones_lr, 
     main="Regresión lineal: Predicciones vs Valores Reales",
     xlab="Valores Reales (Temperatura)", 
     ylab="Predicciones (Temperatura)",
     pch=19, col="blue")

# Agregar una línea de referencia (y = x) para ver el ajuste perfecto
abline(a=0, b=1, col="red", lwd=2)  # Línea roja de referencia




#Formatear tabla en word
install.packages("sjPlot")
library(sjPlot)

tab_model(modelo_lr, file = "modelo_lr.doc")


#2: RANDOM FOREST =======================================================================================================

# Instalar y cargar la librería randomForest (si es necesario)
install.packages("randomForest")  # Solo la primera vez
library(randomForest)


# Entrenar el modelo de Random Forest
set.seed(123)
modelo_rf <- randomForest(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                            In_temp_1 + In_temp_2 + In_temp_3 + 
                            Out_temp_1 + Out_temp_2 + Out_temp_3, 
                          data = train_data, 
                          ntree = 500,      # Número de árboles en el bosque
                          mtry = 3,         # Número de variables consideradas en cada división
                          importance = TRUE)


# Ver resumen del modelo
print(modelo_rf)


# Importancia de las variables
importance(modelo_rf)
varImpPlot(modelo_rf)  # Gráfico de importancia


# Hacer predicciones con el conjunto de prueba
predicciones_rf <- predict(modelo_rf, newdata = test_data)


# Valores reales
valores_reales_rf <- test_data$Temp  # Ahora tomamos los valores reales de test_data


# Calcular residuos
residuos_rf <- valores_reales_rf - predicciones_rf


# Calcular métricas de error
mse_rf <- mean(residuos_rf^2)         # MSE
rmse_rf <- sqrt(mse_rf)               # RMSE
mae_rf <- mean(abs(residuos_rf))      # MAE


# Imprimir resultados
cat("MSE (Random Forest):", mse_rf, "\n")
cat("RMSE (Random Forest):", rmse_rf, "\n")
cat("MAE (Random Forest):", mae_rf, "\n")


# Graficar predicciones vs. valores reales
plot(test_data$Temp, predicciones_rf, main="Random Forest: Predicciones vs Valores Reales",
     xlab="Valores Reales (Temperatura)", ylab="Predicciones (Temperatura)",
     pch=19, col="blue")
abline(a=0, b=1, col="red", lwd=2)  # Línea de referencia


#Para calcular el R2
install.packages("caret")  # Solo si no lo tienes instalado
library(caret)

r2_caret <- R2(predicciones_rf, valores_reales_rf)
cat("R² (Caret):", r2_caret, "\n")
