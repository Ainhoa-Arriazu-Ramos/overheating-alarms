#Importar base de datos: Vivtodas_verano_horario==========================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(ggplot2)

#0. Quitar grados hora
Vivtodas_verano_horario <- Vivtodas_verano_horario %>% select(-grados_hora)

# --- 1. Crear columna de fecha ---
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day))

# --- 2. Definir las franjas horarias y sus pesos ---
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

# --- 3. Asignar franja a cada observación horaria ---
Vivtodas_verano_horario2 <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")

# --- 4. Calcular temperatura media por franja ---
Vivtodas_franja_mean <- Vivtodas_verano_horario2 %>%
  group_by(dwell_numb, date, franja, peso) %>%
  summarise(
    T_mean_franja = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  )

# --- 5. Calcular temperatura ponderada diaria (según los pesos) ---
T_ponderada_diaria <- Vivtodas_franja_mean %>%
  group_by(dwell_numb, date) %>%
  summarise(
    T_ponderada = sum(T_mean_franja * peso, na.rm = TRUE) / sum(peso, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6. Calcular medias diarias de las variables exteriores ---
Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),
    Ext_RAD_mean = mean(Ext_RAD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(T_ponderada_diaria, by = c("dwell_numb", "date")) %>%
  arrange(dwell_numb, date)

# --- 7. Crear retardos (lags) de 1 a 9 días ---
Vivtodas_diario_lags <- Vivtodas_diario %>%
  group_by(dwell_numb) %>%
  arrange(date) %>%
  mutate(
    across(
      c(Ext_T_mean, T_ponderada),
      list(
        lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3),
        lag4 = ~lag(., 4), lag5 = ~lag(., 5), lag6 = ~lag(., 6),
        lag7 = ~lag(., 7), lag8 = ~lag(., 8), lag9 = ~lag(., 9)
      ),
      .names = "{.col}_{.fn}"
    ),
    Ext_RAD_mean_lag1 = lag(Ext_RAD_mean, 1)
  ) %>%
  ungroup()

# --- 8. Unir las medias de franja con las variables predictoras ---
modelo_base <- Vivtodas_franja_mean %>%
  left_join(Vivtodas_diario_lags, by = c("dwell_numb", "date")) %>%
  filter(!is.na(Ext_T_mean_lag9), !is.na(T_ponderada_lag9))

# --- 9. Ajustar modelo lineal global ---
modelo <- lm(
  T_mean_franja ~ Ext_T_mean +
    Ext_T_mean_lag1 + Ext_T_mean_lag2 + Ext_T_mean_lag3 +
    Ext_T_mean_lag4 + Ext_T_mean_lag5 + Ext_T_mean_lag6 +
    Ext_T_mean_lag7 + Ext_T_mean_lag8 + Ext_T_mean_lag9 +
    T_ponderada_lag1 + T_ponderada_lag2 + T_ponderada_lag3 +
    T_ponderada_lag4 + T_ponderada_lag5 + T_ponderada_lag6 +
    T_ponderada_lag7 + T_ponderada_lag8 + T_ponderada_lag9 +
    Ext_RAD_mean,
  data = modelo_base
)

# --- 10. Ver resultados ---
summary(modelo)



#========================
#GRÁFICOS
#========================

#1. Observado vs Predicho ======================================================
# Añadir predicciones al dataframe
modelo_base <- modelo_base %>%
  mutate(T_predicha = predict(modelo))

# Gráfico básico
plot(modelo_base$T_mean_franja, modelo_base$T_predicha,
     xlab = "T_mean_franja observada",
     ylab = "T_mean_franja predicha",
     main = "Temperatura interior por franja: Observada vs Predicha",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # línea 1:1



#========================
#CALCULAR EL DESEMPEÑO PARA CADA FRANJA
#========================
library(Metrics)  # para rmse y mae

# Crear columna de residuo
modelo_base <- modelo_base %>%
  mutate(residuo = T_mean_franja - T_predicha)

# Calcular métricas por franja
errores_por_franja <- modelo_base %>%
  group_by(franja) %>%
  summarise(
    RMSE = sqrt(mean(residuo^2, na.rm = TRUE)),
    MAE  = mean(abs(residuo), na.rm = TRUE),
    R2   = cor(T_mean_franja, T_predicha, use = "complete.obs")^2,
    .groups = "drop"
  )

errores_por_franja


























