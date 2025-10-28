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
  mutate(date = make_date(year, month, day))  # Crear variable de fecha


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
    Ext_T_mean  = mean(Ext_T, na.rm = TRUE),
    Ext_RAD_mean = mean(Ext_RAD, na.rm = TRUE),
    Int_T_mean  = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(dwell_numb) %>%
  arrange(date) %>%
  mutate(
    across(
      c(Ext_T_mean, Int_T_mean),
      list(
        lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3),
        lag4 = ~lag(., 4), lag5 = ~lag(., 5), lag6 = ~lag(., 6),
        lag7 = ~lag(., 7), lag8 = ~lag(., 8), lag9 = ~lag(., 9)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup() 


# =====================================================================
# TEMPERATURA INTERIOR PONDERADA Y SUS LAGS
# =====================================================================

Vivtodas_diario_ponderada <- T_franja_mean %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Int_T_ponderada = sum(T_mean_franja * peso, na.rm = TRUE) / sum(peso, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(dwell_numb) %>%
  arrange(date) %>%
  mutate(
    across(
      Int_T_ponderada,
      list(
        lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3),
        lag4 = ~lag(., 4), lag5 = ~lag(., 5), lag6 = ~lag(., 6),
        lag7 = ~lag(., 7), lag8 = ~lag(., 8), lag9 = ~lag(., 9)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()


# =====================================================================
# UNIÓN DE TABLAS (franjas + variables predictoras)
# =====================================================================

base_final <- T_franja_mean %>%
  left_join(Vivtodas_diario, by = c("dwell_numb", "date")) %>%            # lags de Int_T_mean
  left_join(Vivtodas_diario_ponderada, by = c("dwell_numb", "date")) %>%  # lags de Int_T_ponderada
  filter(
    !is.na(Ext_T_mean_lag9),
    !is.na(Int_T_mean_lag9),
    !is.na(Int_T_ponderada_lag9)
  )
















# =========================================================================
# MODELO DE REGRESIÓN LINEAL MÚLTIPLE (MLR)-con temperatura interior media
# =========================================================================

# División del dataset por viviendas
train_viviendas <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_viviendas  <- c(2, 4, 8, 11)

train_data <- base_final %>% filter(dwell_numb %in% train_viviendas)
test_data  <- base_final %>% filter(dwell_numb %in% test_viviendas)

# Entrenamiento del modelo
modelo_Tint_mean <- lm(
  T_mean_franja ~ Ext_T_mean +
    Ext_T_mean_lag1 + Ext_T_mean_lag2 + Ext_T_mean_lag3 +
    Ext_T_mean_lag4 + Ext_T_mean_lag5 + Ext_T_mean_lag6 +
    Ext_T_mean_lag7 + Ext_T_mean_lag8 + Ext_T_mean_lag9 +
    Int_T_mean_lag1 + Int_T_mean_lag2 + Int_T_mean_lag3 +
    Int_T_mean_lag4 + Int_T_mean_lag5 + Int_T_mean_lag6 +
    Int_T_mean_lag7 + Int_T_mean_lag8 + Int_T_mean_lag9 +
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
    Ext_T_mean_lag1 + Ext_T_mean_lag2 + Ext_T_mean_lag3 +
    Ext_T_mean_lag4 + Ext_T_mean_lag5 + Ext_T_mean_lag6 +
    Ext_T_mean_lag7 + Ext_T_mean_lag8 + Ext_T_mean_lag9 +
    Int_T_ponderada_lag1 + Int_T_ponderada_lag2 + Int_T_ponderada_lag3 +
    Int_T_ponderada_lag4 + Int_T_ponderada_lag5 + Int_T_ponderada_lag6 +
    Int_T_ponderada_lag7 + Int_T_ponderada_lag8 + Int_T_ponderada_lag9 +
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

#1. 
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_mean)) +
  geom_point(alpha = 0.5, color = "black") +
  
   geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  
  labs(
    title = "Temperatura real vs predicha por el modelo",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)"
  ) +
  theme_minimal()


# 2. Predicciones vs valores reales (coloreadas por franjas)
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_mean, color = franja)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "Temperatura real vs predicha por el modelo",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)",
    color = "Franja horaria"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )


# 3. Residuos del modelo
test_data$resid <- test_data$T_mean_franja - test_data$T_pred_mean

ggplot(test_data, aes(x = T_pred_mean, y = resid)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos vs Temperatura predicha",
    x = "Temperatura predicha (°C)",
    y = "Error (real - predicha)"
  ) +
  theme_minimal()


# 4. Desempeño por franja horaria
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_mean, color = franja)) +
  geom_point(alpha = 0.5) +
  
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  
  facet_wrap(~franja) +
  
  labs(
    title = "Temperatura real vs predicha por franja horaria",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)",
    color = "Franja horaria"
  ) +
  theme_minimal()


# 5. Métricas de desempeño global
R2   <- summary(modelo_Tint_mean)$r.squared
RMSE <- rmse(test_data$T_mean_franja, test_data$T_pred_mean)
MAE  <- mae(test_data$T_mean_franja, test_data$T_pred_mean)

cat("R² =", round(R2, 3), "\nRMSE =", round(RMSE, 2), "\nMAE =", round(MAE, 2))







#Con la predicción en base a la temperatura interior ponderada==================================

# 1. Predicciones vs valores reales (todas las franjas en conjunto)
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_pond)) +
  geom_point(alpha = 0.5, color = "black") +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  labs(
    title = "Temperatura real vs predicha por el modelo",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)"
  ) +
  theme_minimal()


# 2. Predicciones vs valores reales (coloreadas por franjas)
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_pond, color = franja)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "Temperatura real vs predicha por el modelo",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)",
    color = "Franja horaria"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )


# 3. Residuos del modelo
test_data$resid <- test_data$T_mean_franja - test_data$T_pred_pond

ggplot(test_data, aes(x = T_pred_pond, y = resid)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos vs Temperatura predicha",
    x = "Temperatura predicha (°C)",
    y = "Error (real - predicha)"
  ) +
  theme_minimal()


# 4. Desempeño por franja horaria
ggplot(test_data, aes(x = T_mean_franja, y = T_pred_pond, color = franja)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  facet_wrap(~franja) +
  labs(
    title = "Temperatura real vs predicha por franja horaria",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)",
    color = "Franja horaria"
  ) +
  theme_minimal()


# 5. Métricas de desempeño global
R2   <- summary(modelo_Tint_ponderada)$r.squared
RMSE <- rmse(test_data$T_mean_franja, test_data$T_pred_pond)
MAE  <- mae(test_data$T_mean_franja, test_data$T_pred_pond)

cat("R² =", round(R2, 3), "\nRMSE =", round(RMSE, 2), "\nMAE =", round(MAE, 2))







