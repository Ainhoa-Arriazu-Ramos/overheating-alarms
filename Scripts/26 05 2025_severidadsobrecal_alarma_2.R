#IMPORTAR BASE DE DATOS

#1: Calcular la variable objetivo: Grado-horas >26°C
library(dplyr)
library(tidyr)
library(lubridate)

Temp_int_hourly <- Temp_int_hourly %>%
  mutate(
    dia = as.Date(Date),  # extrae solo la fecha
    grados_hora = pmax(Indoor_T - 26, 0)  # si está por debajo de 26, cuenta como 0
  )


grados_horas_pordia <- Temp_int_hourly %>%
  group_by(dia) %>%
  summarise(grado_horas_26 = sum(grados_hora, na.rm = TRUE))



#2: Calcular las variables diarias
#Delta_T= diferencia entre temp exterior e interior
#Amplitud térmica: diferencia entre temperatura minima y máxima exterior

variables_diarias <- Temp_int_hourly %>%
  group_by(dia) %>%
  summarise(
    T_ext_media = mean(Outdoor_T, na.rm = TRUE),
    T_ext_max = max(Outdoor_T, na.rm = TRUE),
    T_ext_min = min(Outdoor_T, na.rm = TRUE),
    Rad_media = mean(Outdoor_GR, na.rm = TRUE),
    T_int_media = mean(Indoor_T, na.rm = TRUE)
  ) %>%
  mutate(
    Delta_T = T_int_media - T_ext_media,
    
    Amp_termica = T_ext_max - T_ext_min,
    
    T_int_1 = lag(T_int_media, 1), 
    T_int_2 = lag(T_int_media, 2), 
    T_int_3 = lag(T_int_media, 3), 
    
    T_ext_1 = lag(T_ext_media, 1), 
    T_ext_2 = lag(T_ext_media, 2), 
    T_ext_3 = lag(T_ext_media, 3),
  )

#3: Unir con la variable objetivo
riesgo_diario <- left_join(variables_diarias, grados_horas_pordia, by = c("dia"))

riesgo_diario <- riesgo_diario %>%
  drop_na(grado_horas_26, T_ext_media, Rad_media, T_int_diaantes, Delta_T, Amp_termica)


#4: Quitar filas con entradas NA
riesgo_diario <- na.omit(riesgo_diario)






#A partir de aqui, usar el dataframe: "riesgo_diario"========





install.packages("randomForest")   # Solo si no lo tienes
install.packages("ggplot2")        # Para el gráfico

library(randomForest)
library(ggplot2)

# Modelo RandomForest
modelo_rf <- randomForest(
  grado_horas_26 ~ T_ext_media + Rad_media + T_int_1 + T_int_2 + T_int_3 + 
    T_ext_1 + T_ext_2 + T_ext_3 + Delta_T + Amp_termica,
  data = riesgo_diario,
  ntree = 500,
  importance = TRUE
)


print(modelo_rf)

max(riesgo_diario$grado_horas_26, na.rm = TRUE)

#Ver importancia variables
importance(modelo_rf)
varImpPlot(modelo_rf)


#Validar el modelo con TRAIN y TEST ===============================

set.seed(123)  # Para reproducibilidad

# Dividir en train/test
n <- nrow(riesgo_diario)
train_indices <- sample(1:n, size = 0.7 * n)

train_data <- riesgo_diario[train_indices, ]
test_data  <- riesgo_diario[-train_indices, ]

#Modelo Random Forest
modelo_rf_test <- randomForest(
  grado_horas_26 ~ T_ext_media + Rad_media + T_int_1 + T_int_2 + T_int_3 +
    T_ext_1 + T_ext_2 + T_ext_3 + Delta_T + Amp_termica,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

print(modelo_rf_test)

# Predicciones en test
pred_rf <- predict(modelo_rf_test, newdata = test_data)

# Evaluar rendimiento
real <- test_data$grado_horas_26
mse_rf <- mean((pred_rf - real)^2)
rmse_rf <- sqrt(mse_rf)
r2_rf <- 1 - sum((real - pred_rf)^2) / sum((real - mean(real))^2)

cat("Random Forest - RMSE:", rmse_rf, " | R²:", r2_rf, "\n")

#Comparar el RF con un modelo de RLM ===============================
# Modelo lineal
modelo_lm <- lm(
  grado_horas_26 ~ T_ext_media + Rad_media + T_int_1 + T_int_2 + T_int_3 +
    T_ext_1 + T_ext_2 + T_ext_3 + Delta_T + Amp_termica,
  data = train_data
)

# Predicciones en test
pred_lm <- predict(modelo_lm, newdata = test_data)

# Evaluar rendimiento
mse_lm <- mean((pred_lm - real)^2)
rmse_lm <- sqrt(mse_lm)
r2_lm <- 1 - sum((real - pred_lm)^2) / sum((real - mean(real))^2)

cat("Modelo Lineal - RMSE:", rmse_lm, " | R²:", r2_lm, "\n")




#GRÁFICOS ==========================


library(ggplot2)

# Crear data.frame solo para RF
resultados_rf <- data.frame(
  Real = test_data$grado_horas_26,
  Predicho = pred_rf
)

# Gráfico
ggplot(resultados_rf, aes(x = Real, y = Predicho)) +
  geom_point(color = "forestgreen", alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(title = "Random Forest: Valores Reales vs Predichos",
       x = "Valor real de grado_horas_26",
       y = "Valor predicho") +
  theme_minimal()


# Crear un data.frame para graficar
resultados <- data.frame(
  Real = test_data$grado_horas_26,
  Pred_RF = pred_rf,
  Pred_LM = pred_lm
)

# Gráfico para ambos modelos
ggplot(resultados, aes(x = Real)) +
  geom_point(aes(y = Pred_RF), color = "forestgreen", alpha = 0.6, size = 2) +
  geom_point(aes(y = Pred_LM), color = "steelblue", alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(title = "Valores Reales vs Predichos en el conjunto de test",
       x = "Valor real de grado_horas_26",
       y = "Valor predicho",
       caption = "Verde: Random Forest | Azul: Modelo Lineal") +
  theme_minimal()



