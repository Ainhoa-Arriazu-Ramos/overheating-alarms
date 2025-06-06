#IMPORTAR DATASET =====================================================================================================


#PREPARAR EL MODELO =====================================================================================================

install.packages("caTools")
library(caTools)

# Dividir el conjunto de datos en entrenamiento y prueba (70%-30%)
set.seed(123)  # Para asegurar la replicabilidad
split <- sample.split(dataset_tem_daily_ARX$Temp, SplitRatio = 0.7)

# Crear los conjuntos de entrenamiento y prueba
train_data <- subset(dataset_tem_daily_ARX, split == TRUE)
test_data <- subset(dataset_tem_daily_ARX, split == FALSE)



#1: REGRESIÓN LINEAL MULTIPLE CON VARIABLE INTERIORES ===========================================================================================

# Modelo de regresión lineal con variables interiores
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

#2: REGRESIÓN LINEAL MULTIPLE SIN VARIABLE INTERIORES ===========================================================================================

modelo_solo_outdoor <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                            Out_temp_1 + Out_temp_2 + Out_temp_3, 
                          data = train_data)

# Resumen del modelo
summary(modelo_solo_outdoor)


# Hacer predicciones con el conjunto de prueba
predicciones_solo_outdoor <- predict(modelo_solo_outdoor, newdata = test_data)


# Ver las primeras predicciones
head(predicciones_solo_outdoor)


#3: COMPARATIVA DE LOS DOS MODELOS ===========================================================================================

# RMSE
rmse_lr <- sqrt(mean((test_data$Temp - predicciones_lr)^2))
rmse_outdoor <- sqrt(mean((test_data$Temp - predicciones_solo_outdoor)^2))

# MAE
mae_lr <- mean(abs(test_data$Temp - predicciones_lr))
mae_outdoor <- mean(abs(test_data$Temp - predicciones_solo_outdoor))

# R² (usado desde el resumen del modelo sobre training set)
r2_lr <- summary(modelo_lr)$r.squared
r2_outdoor <- summary(modelo_solo_outdoor)$r.squared

#Mostrar resultados
cat("Modelo completo (modelo_lr):\n")
cat("  RMSE:", rmse_lr, "\n")
cat("  MAE :", mae_lr, "\n")
cat("  R²  :", r2_lr, "\n\n")

cat("Modelo solo outdoor (modelo_solo_outdoor):\n")
cat("  RMSE:", rmse_outdoor, "\n")
cat("  MAE :", mae_outdoor, "\n")
cat("  R²  :", r2_outdoor, "\n")

#Comparativa a nivel gráfico
install.packages("ggplot2")  # Solo si no lo tenés
library(ggplot2)

#Crear dataframe de comparación
comparacion <- data.frame(
  Real = test_data$Temp,
  Pred_completo = predicciones_lr,
  Pred_outdoor = predicciones_solo_outdoor
)

#Graficar predicciones vs reales
#MODELO COMPLETO
ggplot(comparacion, aes(x = Real, y = Pred_completo)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Modelo completo: Predicción vs Real",
       x = "Temperatura real (°C)",
       y = "Temperatura predicha (°C)") +
  theme_minimal()

#MODELO SOLO VARIABLES EXTERIORES
ggplot(comparacion, aes(x = Real, y = Pred_outdoor)) +
  geom_point(color = "darkorange", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Modelo solo outdoor: Predicción vs Real",
       x = "Temperatura real (°C)",
       y = "Temperatura predicha (°C)") +
  theme_minimal()

#GRÁFICO COMBINADO
# Convertimos a formato largo (long format) para ggplot
library(tidyr)

comparacion_larga <- comparacion %>%
  pivot_longer(cols = c(Pred_completo, Pred_outdoor),
               names_to = "Modelo",
               values_to = "Prediccion")

# Gráfico
ggplot(comparacion_larga, aes(x = Real, y = Prediccion, color = Modelo)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Comparación de modelos: Predicción vs Real",
       x = "Temperatura real (°C)",
       y = "Temperatura predicha (°C)",
       color = "Modelo") +
  scale_color_manual(values = c("Pred_completo" = "steelblue", 
                                "Pred_outdoor" = "darkorange")) +
  theme_minimal()
