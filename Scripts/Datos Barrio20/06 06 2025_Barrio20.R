library(dplyr)


#IMPORTAR BASE DE DATOS Viviendas_Barrio20
vivs_barrio20 <- Viviendas_Barrio20



#1: Variables desfasadas
vivs_barrio20 <- vivs_barrio20 %>%
  arrange(dwell_numb, year, month, day) %>%  # ordenar por vivienda y tiempo
  group_by(dwell_numb) %>%
  mutate(
    Int_T_1 = lag(Int_T, 1),
    Int_T_2 = lag(Int_T, 2),
    Int_T_3 = lag(Int_T, 3),

    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3)
  ) %>%
  ungroup()

#Quitar filas con entradas NA
vivs_barrio20 <- na.omit(vivs_barrio20)



#2: Variable ALARMA
#Límite diario en base al confort adaptativo EN 16798-1:2019
vivs_barrio20 <- vivs_barrio20 %>%
  mutate(trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3))
vivs_barrio20 <- vivs_barrio20 %>%
  mutate(limiteadap = (0.33 * trm)+21.8)

# Alarma
#si:
#trm > 30 o
#Int_T > limiteadap 
vivs_barrio20 <- vivs_barrio20 %>%
  mutate(
    alarma_real = if_else(trm > 30 | Int_T > limiteadap, 1, 0)
  )

#Miramos cómo es la distribución dentro de la variable dicotómica "alarma"
table(vivs_barrio20$alarma_real)


##########################################################


#Reentreno modelo sin Ext_RAD (no tenemos ese dato del barrio)

modelo_rlm_sin_rad <- lm(Int_T ~ Ext_T + Ext_T_1 + Ext_T_2 + Ext_T_3 + 
                           Int_T_1 + Int_T_2 + Int_T_3, 
                         data = train_data)

# Predecir temperatura interior
vivs_barrio20$Int_T_pred <- predict(modelo_rlm_sin_rad, newdata = vivs_barrio20)

# Calcular alarma predecida
vivs_barrio20 <- vivs_barrio20 %>%
  mutate(
    alarma_predicha = if_else(trm > 30 | Int_T_pred > limiteadap, 1, 0)
  )

#COMPARACIÓN ENTRE LA TEMP INTERIOR REAL Y LA PREDECIDA

library(ggplot2)

library(ggplot2)

ggplot(vivs_barrio20, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Dispersión: Temperatura Interior Real vs Predicha (Barrio20)",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  theme_minimal()

#Extraer métricas
install.packages("Metrics")
library(Metrics)
# Calcular errores
mae <- mae(vivs_barrio20$Int_T, vivs_barrio20$Int_T_pred)
rmse <- rmse(vivs_barrio20$Int_T, vivs_barrio20$Int_T_pred)
mbe <- mean(vivs_barrio20$Int_T_pred - vivs_barrio20$Int_T)

# R² (coeficiente de determinación)
sst <- sum((vivs_barrio20$Int_T - mean(vivs_barrio20$Int_T))^2)
sse <- sum((vivs_barrio20$Int_T - vivs_barrio20$Int_T_pred)^2)
r2 <- 1 - (sse / sst)

# Mostrar resultados
cat("=== MÉTRICAS DE DESEMPEÑO - PREDICCIÓN Int_T ===\n")
cat("MAE (Error Absoluto Medio):        ", round(mae, 3), "\n")
cat("RMSE (Raíz del Error Cuadrático): ", round(rmse, 3), "\n")
cat("MBE (Error Medio de Sesgo):        ", round(mbe, 3), "\n")
cat("R² (Coef. de determinación):       ", round(r2, 3), "\n")


#COMPARACIÓN ENTRE LA ALARMA REAL Y LA PREDECIDA

# Matriz de confusión
conf_mat_barrio <- table(Real = vivs_barrio20$alarma_real, Predicho = vivs_barrio20$alarma_predicha)
print(conf_mat_barrio)

#Gráfico
library(ggplot2)
# Convertir la matriz de confusión a data frame para ggplot
conf_mat_df <- as.data.frame(conf_mat_barrio)

# Graficar heatmap adaptado
ggplot(conf_mat_df, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de Confusión - Barrio20",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"))

# Extraer métricas
TN <- conf_mat_barrio[1, 1]
FP <- conf_mat_barrio[1, 2]
FN <- conf_mat_barrio[2, 1]
TP <- conf_mat_barrio[2, 2]

accuracy  <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * (precision * recall) / (precision + recall)

cat("=== MÉTRICAS DE DESEMPEÑO EN BARRIO20 ===\n")
cat("Accuracy :", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall   :", round(recall, 3), "\n")
cat("F1 Score :", round(f1_score, 3), "\n")






######################COMPARATIVA ENTRE VIVIENDA 4(REEDIFICADA) Y 5(AUTOCONSTRUIDA)

#Comparar exposición al sobrecalentamiento
# Conteo de alarmas reales por vivienda
table(vivs_barrio20$dwell_numb, vivs_barrio20$alarma_real)

# Porcentaje de días con alarma
vivs_barrio20 %>%
  group_by(dwell_numb) %>%
  summarise(
    dias = n(),
    alarmas = sum(alarma_real),
    porcentaje_alarma = round(100 * alarmas / dias, 1)
  )



  
  