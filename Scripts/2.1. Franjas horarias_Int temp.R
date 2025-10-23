#Importar base de datos: Vivtodas_verano_horario==========================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(Metrics)

#1. Limpieza inicialL======================================================================================

#Quitar grados hora
Vivtodas_verano_horario <- Vivtodas_verano_horario %>% select(-grados_hora)

#Quitar las entradas del año 2021 (nos quedamos solo con 2022, verano extremo)
#Se deja: Año 2022 (Junio, Julio y Agosto)
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  filter(year != 2021)

#Crear columna de fecha
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day))





#===============================================================================================================
#DESARROLLO: Predicción de la temperatura media de cada franja en base a temperaturas diarias de días anteriores
#===============================================================================================================

#2. Definir las franjas horarias y sus pesos===============================================================
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
    hour %in% 0:7   ~ 3,     # noches
    hour %in% 8:11  ~ 1,
    hour %in% 12:15 ~ 2,
    hour %in% 16:19 ~ 2,
    hour %in% 20:23 ~ 1.5
  )
)

#Asignar franja a cada observación horaria
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")




#3. Variables diarias y lags ===============================================================================================

Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),
    Ext_RAD_mean = mean(Ext_RAD, na.rm = TRUE),
    Int_T_mean = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  )

# Crear lags de hasta 9 días
Vivtodas_diario <- Vivtodas_diario %>%
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
    ),
    Ext_RAD_mean_lag1 = lag(Ext_RAD_mean, 1)
  ) %>%
  ungroup() %>%
  drop_na()



#4. Calcular temperatura media por franja================================================================================================

T_franja_mean <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date, franja) %>%
  summarise(
    T_mean_franja = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tipo_umbral = if_else(
      franja %in% c("Noche_0_3", "Noche_4_7", "Noche_20_23"),
      "fijo",
      "adaptativo"
    )
  )




#5. Unión de tablas (franja + variables predictoras)================================================================================================

base_final <- T_franja_mean %>%
  left_join(Vivtodas_diario, by = c("dwell_numb", "date")) %>%
  filter(!is.na(Ext_T_mean_lag9), !is.na(Int_T_mean_lag9))





#6.Modelo de regresión lineal (MRL)====================================================================================================
modelo <- lm(
  T_mean_franja ~ Ext_T_mean +
    Ext_T_mean_lag1 + Ext_T_mean_lag2 + Ext_T_mean_lag3 +
    Ext_T_mean_lag4 + Ext_T_mean_lag5 + Ext_T_mean_lag6 +
    Ext_T_mean_lag7 + Ext_T_mean_lag8 + Ext_T_mean_lag9 +
    Int_T_mean_lag1 + Int_T_mean_lag2 + Int_T_mean_lag3 +
    Int_T_mean_lag4 + Int_T_mean_lag5 + Int_T_mean_lag6 +
    Int_T_mean_lag7 + Int_T_mean_lag8 + Int_T_mean_lag9 +
    Ext_RAD_mean,
  data = base_final
)

summary(modelo)






#====================================================================================================
#ANÁLISIS
#====================================================================================================

#1. Predicciones vs valores reales===================================================================

# Predicciones del modelo
base_final$T_pred <- predict(modelo, newdata = base_final)

# Gráfico comparativo
ggplot(base_final, aes(x = T_mean_franja, y = T_pred)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Temperatura real vs predicha por el modelo",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)"
  ) +
  theme_minimal()



#2. Errores de predicción (residuos)=================================================================

base_final$resid <- base_final$T_mean_franja - base_final$T_pred

ggplot(base_final, aes(x = T_pred, y = resid)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Residuos vs Temperatura predicha",
    x = "Temperatura predicha (°C)",
    y = "Error (real - predicha)"
  ) +
  theme_minimal()



#3. Desempeño del modelo por franja horaria =================================================================

ggplot(base_final, aes(x = T_mean_franja, y = T_pred, color = franja)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  facet_wrap(~franja) +
  labs(
    title = "Temperatura real vs predicha por franja horaria",
    x = "Temperatura real (°C)",
    y = "Temperatura predicha (°C)"
  ) +
  theme_minimal()


#4. Serie temporal comparando real vs predicho ===============================================================

# Selecciona una vivienda de ejemplo
ejemplo <- base_final %>% filter(dwell_numb == unique(dwell_numb)[1])

ggplot(ejemplo, aes(x = date)) +
  geom_line(aes(y = T_mean_franja, color = "Real")) +
  geom_line(aes(y = T_pred, color = "Predicha"), linetype = "dashed") +
  labs(
    title = "Serie temporal de temperatura (real vs predicha)",
    x = "Fecha", y = "Temperatura (°C)"
  ) +
  scale_color_manual(values = c("Real" = "black", "Predicha" = "red")) +
  theme_minimal()


#Métricas cuantitativas: R², RMSE, MAE ======================================================================

R2 <- summary(modelo)$r.squared
RMSE <- rmse(base_final$T_mean_franja, base_final$T_pred)
MAE <- mae(base_final$T_mean_franja, base_final$T_pred)

cat("R² =", round(R2, 3), "\nRMSE =", round(RMSE, 2), "\nMAE =", round(MAE, 2))





