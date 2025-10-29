# Importar base de datos: Vivtodas_verano_horario

# =====================================================================
# IMPORTACIÓN Y LIBRERÍAS
# =====================================================================

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(Metrics)

# =====================================================================
# LIMPIEZA INICIAL
# =====================================================================

Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  select(-grados_hora) %>%                 # Eliminar variable no necesaria
  filter(year == 2022) %>%                 # Mantener solo verano extremo 2022
  mutate(date = make_date(year, month, day))  # Crear variable de date


# =====================================================================
# DEFINICIÓN DE FRANJAS HORARIAS
# =====================================================================

franjas <- tibble(
  hour = 0:23,
  franja = case_when(
    hour %in% 0:3   ~ "Noche_0_3",
    hour %in% 4:7   ~ "Noche_4_7",
    hour %in% 8:11  ~ "Manana",
    hour %in% 12:15 ~ "Mediodia",
    hour %in% 16:19 ~ "Tarde",
    hour %in% 20:23 ~ "Noche_20_23"
  ),
  peso = case_when(
    hour %in% 0:7   ~ 3,
    hour %in% 8:11  ~ 1,
    hour %in% 12:15 ~ 2,
    hour %in% 16:19 ~ 2,
    hour %in% 20:23 ~ 1.5
  )
)

# Asignar franja a cada observación
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")


# =====================================================================
# TEMPERATURA MEDIA POR FRANJA HORARIA
# =====================================================================

T_franja_mean <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date, franja) %>%
  summarise(
    T_mean_franja = mean(Int_T, na.rm = TRUE),
    peso = mean(peso, na.rm = TRUE),  # Mantener el peso de la franja
    .groups = "drop"
  ) %>%
  mutate(
    tipo_umbral = if_else(
      franja %in% c("Noche_0_3", "Noche_4_7", "Noche_20_23"),
      "fijo", "adaptativo"
    )
  )

# =====================================================================
# VARIABLES DIARIAS Y LAGS (TEMPERATURA MEDIA)
# =====================================================================

Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean  = mean(Ext_T, na.rm = TRUE),       # Media diaria exterior
    Ext_RAD_mean = mean(Ext_RAD, na.rm = TRUE),    # Media diaria radiación
    Int_T_mean  = mean(Int_T, na.rm = TRUE),       # Media diaria interior
    .groups = "drop"
  ) %>%
  group_by(dwell_numb) %>%
  arrange(date) %>%
  mutate(
    # LAGS temperatura interior media
    Int_T_mean_1 = if_else(as.integer(date - lag(date, 1)) == 1, lag(Int_T_mean, 1), NA_real_),
    Int_T_mean_2 = if_else(as.integer(date - lag(date, 2)) == 2, lag(Int_T_mean, 2), NA_real_),
    Int_T_mean_3 = if_else(as.integer(date - lag(date, 3)) == 3, lag(Int_T_mean, 3), NA_real_),
    Int_T_mean_4 = if_else(as.integer(date - lag(date, 4)) == 4, lag(Int_T_mean, 4), NA_real_),
    Int_T_mean_5 = if_else(as.integer(date - lag(date, 5)) == 5, lag(Int_T_mean, 5), NA_real_),
    Int_T_mean_6 = if_else(as.integer(date - lag(date, 6)) == 6, lag(Int_T_mean, 6), NA_real_),
    Int_T_mean_7 = if_else(as.integer(date - lag(date, 7)) == 7, lag(Int_T_mean, 7), NA_real_),
    Int_T_mean_8 = if_else(as.integer(date - lag(date, 8)) == 8, lag(Int_T_mean, 8), NA_real_),
    Int_T_mean_9 = if_else(as.integer(date - lag(date, 9)) == 9, lag(Int_T_mean, 9), NA_real_),
    
    # LAGS temperatura exterior media
    Ext_T_mean_1 = if_else(as.integer(date - lag(date, 1)) == 1, lag(Ext_T_mean, 1), NA_real_),
    Ext_T_mean_2 = if_else(as.integer(date - lag(date, 2)) == 2, lag(Ext_T_mean, 2), NA_real_),
    Ext_T_mean_3 = if_else(as.integer(date - lag(date, 3)) == 3, lag(Ext_T_mean, 3), NA_real_),
    Ext_T_mean_4 = if_else(as.integer(date - lag(date, 4)) == 4, lag(Ext_T_mean, 4), NA_real_),
    Ext_T_mean_5 = if_else(as.integer(date - lag(date, 5)) == 5, lag(Ext_T_mean, 5), NA_real_),
    Ext_T_mean_6 = if_else(as.integer(date - lag(date, 6)) == 6, lag(Ext_T_mean, 6), NA_real_),
    Ext_T_mean_7 = if_else(as.integer(date - lag(date, 7)) == 7, lag(Ext_T_mean, 7), NA_real_),
    Ext_T_mean_8 = if_else(as.integer(date - lag(date, 8)) == 8, lag(Ext_T_mean, 8), NA_real_),
    Ext_T_mean_9 = if_else(as.integer(date - lag(date, 9)) == 9, lag(Ext_T_mean, 9), NA_real_)
  ) %>%
  ungroup()



# =====================================================================
# TEMPERATURA INTERIOR PONDERADA Y SUS LAGS
# =====================================================================

Vivtodas_diario_ponderada <- T_franja_mean %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Int_T_mean_ponderada = sum(T_mean_franja * peso, na.rm = TRUE) / sum(peso, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(dwell_numb) %>%
  arrange(date) %>%
  mutate(
    ## ---- LAGS INTERIOR PONDERADA ----
    Int_T_pond_1 = if_else(date == lag(date, 1) + 1, lag(Int_T_mean_ponderada, 1), NA_real_),
    Int_T_pond_2 = if_else(date == lag(date, 2) + 2, lag(Int_T_mean_ponderada, 2), NA_real_),
    Int_T_pond_3 = if_else(date == lag(date, 3) + 3, lag(Int_T_mean_ponderada, 3), NA_real_),
    Int_T_pond_4 = if_else(date == lag(date, 4) + 4, lag(Int_T_mean_ponderada, 4), NA_real_),
    Int_T_pond_5 = if_else(date == lag(date, 5) + 5, lag(Int_T_mean_ponderada, 5), NA_real_),
    Int_T_pond_6 = if_else(date == lag(date, 6) + 6, lag(Int_T_mean_ponderada, 6), NA_real_),
    Int_T_pond_7 = if_else(date == lag(date, 7) + 7, lag(Int_T_mean_ponderada, 7), NA_real_),
    Int_T_pond_8 = if_else(date == lag(date, 8) + 8, lag(Int_T_mean_ponderada, 8), NA_real_),
    Int_T_pond_9 = if_else(date == lag(date, 9) + 9, lag(Int_T_mean_ponderada, 9), NA_real_),
     ) %>%
  ungroup()




# =====================================================================
# UNIÓN DE TABLAS (franjas + variables predictoras)
# =====================================================================

base_final <- T_franja_mean %>%
  left_join(Vivtodas_diario,        by = c("dwell_numb", "date")) %>%     # lags Int_T_mean_mean
  left_join(Vivtodas_diario_ponderada, by = c("dwell_numb", "date")) %>%  # lags Int_T_mean_ponderada
  drop_na()   # <--- elimina filas con cualquier NA














# =========================================================================
# MODELO DE REGRESIÓN LINEAL MÚLTIPLE (MLR)-con temperatura interior media
# =========================================================================

# División del dataset por viviendas
train_viviendas <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_viviendas  <- c(2, 4, 8, 11)

train_data <- base_final %>% filter(dwell_numb %in% train_viviendas)
test_data  <- base_final %>% filter(dwell_numb %in% test_viviendas)

# ARX: Entrenamiento del modelo
modelo_Tint_mean <- lm(
  T_mean_franja ~ Ext_T_mean +
    Ext_T_mean_1 + Ext_T_mean_2 + Ext_T_mean_3 +
    Ext_T_mean_4 + Ext_T_mean_5 + Ext_T_mean_6 +
    Ext_T_mean_7 + Ext_T_mean_8 + Ext_T_mean_9 +
    Int_T_mean_1 + Int_T_mean_2 + Int_T_mean_3 +
    Int_T_mean_4 + Int_T_mean_5 + Int_T_mean_6 +
    Int_T_mean_7 + Int_T_mean_8 + Int_T_mean_9 +
    Ext_RAD_mean,
  data = train_data
)

summary(modelo_Tint_mean)

# Predicciones sobre el conjunto de test
test_data <- test_data %>%
  mutate(T_pred_mean = predict(modelo_Tint_mean, newdata = test_data))




# =========================================================================
# MODELO DE REGRESIÓN LINEAL MÚLTIPLE (MLR)-con temperatura interior ponderada
# =========================================================================

# Entrenamiento del modelo usando la temperatura ponderada y sus lags
modelo_Tint_ponderada <- lm(
  T_mean_franja ~ Ext_T_mean +
    Ext_T_mean_1 + Ext_T_mean_2 + Ext_T_mean_3 +
    Ext_T_mean_4 + Ext_T_mean_5 + Ext_T_mean_6 +
    Ext_T_mean_7 + Ext_T_mean_8 + Ext_T_mean_9 +
    Int_T_pond_1 + Int_T_pond_2 + Int_T_pond_3 +
    Int_T_pond_4 + Int_T_pond_5 + Int_T_pond_6 +
    Int_T_pond_7 + Int_T_pond_8 + Int_T_pond_9 +
    Ext_RAD_mean,
  data = train_data
)

summary(modelo_Tint_ponderada)

# Predicciones sobre el conjunto de test
test_data <- test_data %>%
  mutate(T_pred_pond = predict(modelo_Tint_ponderada, newdata = test_data))













#==========================================
# ANÁLISIS Y VISUALIZACIÓN DE RESULTADOS
#==========================================

#Con la predicción en base a la temperatura interior media==================================
windowsFonts(Times = windowsFont("Times New Roman"))

#1. 
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_mean)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "#00BFFF", linetype = "solid") +
  labs(
    x = "Real Indoor Temperature (°C)",
    y = "Predicted Indoor Temperature (°C)",
    title = "Model Performance – Indoor Temperature Prediction"
  ) +
  coord_fixed(ratio = 1) +         # relación 1:1
  xlim(20, 32) + ylim(20, 32) +    # límites iguales para comparación justa
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    plot.title = element_text(size = 15, hjust = 0.5)
  )


# 2. Desempeño por franja horaria
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_mean, color = franja)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "#00BFFF", linetype = "solid") +
  facet_wrap(~ franja) +
  labs(
    x = "Real Indoor Temperature (°C)",
    y = "Predicted Indoor Temperature (°C)",
    title = "Model Performance by Time Slot",
    color = "Time Slot"
  ) +
  coord_fixed(ratio = 1) +
  xlim(20, 32) + ylim(20, 32) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    plot.title = element_text(size = 15, hjust = 0.5),
    strip.text = element_text(size = 12),      # Texto de los nombres arriba de cada facet
    legend.position = "bottom"                 # Leyenda debajo para mejor lectura
  )



# 3. Métricas de desempeño global
R2   <- summary(modelo_Tint_mean)$r.squared
RMSE <- rmse(test_data$T_mean_franja, test_data$T_pred_mean)
MAE  <- mae(test_data$T_mean_franja, test_data$T_pred_mean)

cat("R² =", round(R2, 3), "\nRMSE =", round(RMSE, 2), "\nMAE =", round(MAE, 2))







#Con la predicción en base a la temperatura interior ponderada==================================
windowsFonts(Times = windowsFont("Times New Roman"))

# 1. Predicciones vs valores reales (todas las franjas en conjunto)
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_pond)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "#00BFFF", linetype = "solid") +
  labs(
    x = "Real Indoor Temperature (°C)",
    y = "Predicted Indoor Temperature (°C)",
    title = "Model Performance – Indoor Temperature Prediction"
  ) +
  coord_fixed(ratio = 1) +
  xlim(20, 32) + ylim(20, 32) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    plot.title = element_text(size = 15, hjust = 0.5)
  )

# 2. Desempeño por franja horaria
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_pond, color = franja)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "#00BFFF", linetype = "solid") +
  facet_wrap(~ franja) +
  labs(
    x = "Real Indoor Temperature (°C)",
    y = "Predicted Indoor Temperature (°C)",
    title = "Model Performance by Time Slot",
    color = "Time Slot"
  ) +
  coord_fixed(ratio = 1) +
  xlim(20, 32) + ylim(20, 32) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    plot.title = element_text(size = 15, hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )


# 3. Métricas de desempeño global
R2   <- summary(modelo_Tint_ponderada)$r.squared
RMSE <- rmse(test_data$T_mean_franja, test_data$T_pred_pond)
MAE  <- mae(test_data$T_mean_franja, test_data$T_pred_pond)

cat("R² =", round(R2, 3), "\nRMSE =", round(RMSE, 2), "\nMAE =", round(MAE, 2))
