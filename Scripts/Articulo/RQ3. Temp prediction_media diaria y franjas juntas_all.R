#===========================================
#PREDICCIÓN DIARIA (2 dias previos)
#===========================================

#Importar la base de datos: Vivtodas_diario_media.xlsx

#Cargar librerías
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)

#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(fecha = make_date(year, month, day)) %>%
  group_by(dwell_numb) %>%
  arrange(fecha) %>%
  mutate(
    Int_T_1 = if_else(as.integer(fecha - lag(fecha, 1)) == 1, lag(Int_T, 1), NA_real_),
    Int_T_2 = if_else(as.integer(fecha - lag(fecha, 2)) == 2, lag(Int_T, 2), NA_real_),
    
    Ext_T_1 = if_else(as.integer(fecha - lag(fecha, 1)) == 1, lag(Ext_T, 1), NA_real_),
    Ext_T_2 = if_else(as.integer(fecha - lag(fecha, 2)) == 2, lag(Ext_T, 2), NA_real_),
    
      ) %>%
  ungroup()

# Quitar filas con entradas NA
Vivtodas_diario_media <- na.omit(Vivtodas_diario_media)



#ARX==========================================================================
# Dividimos el dataset según dwell_numb
train_viviendas <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_viviendas  <- c(2, 4, 8, 11)

# Crear subconjuntos
train_data <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% train_viviendas)

test_data <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% test_viviendas)

#Modelo de ARX con los datos de entrenamiento
modelo_arx <- lm(Int_T ~ Ext_T + 
                   Ext_T_1 + Ext_T_2 
                 + Int_T_1 + Int_T_2, 
                 data = train_data)

summary(modelo_arx)


# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_arx, newdata = test_data)

#Gráfico de dispersion para variable Int_T
windowsFonts(Times = windowsFont("Times New Roman"))

ggplot(test_data, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "#00BFFF", linetype = "solid") +
  labs(
    x = "Real Mean Indoor Temperature (°C)",
    y = "Predicted Mean Indoor Temperature (°C)"
  ) +
  xlim(20, 32) +
  ylim(20, 32) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# Cálculo de errores
calcular_errores <- function(real, pred) {
  mse <- mean((real - pred)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mae <- mean(abs(real - pred), na.rm = TRUE)
  return(list(MSE = mse, RMSE = rmse, MAE = mae))
}

errores <- calcular_errores(test_data$Int_T, test_data$Int_T_pred)

data.frame(
  Modelo = c("Todas las viviendas"),
  MSE = c(errores$MSE),
  RMSE = c(errores$RMSE),
  MAE = c(errores$MAE)
)











#=========================================================
#PREDICCIÓN POR FRANJAS (2 dias previos-12 LAGS PREVIOS)
#=========================================================

#Importar la base de datos: Vivtodas_verano_horario.xlsx

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(Metrics)

# Crear variable fecha
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day))

# DEFINICIÓN DE FRANJAS HORARIAS
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

Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")

# TEMPERATURA MEDIA POR FRANJA
Vivtodas_franja <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date, franja) %>%
  summarise(
    Int_T_franja = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  )

# 12 LAGS AGRUPADOS POR VIVIENDA Y FRANJA
Vivtodas_franja_lags <- Vivtodas_franja %>%
  arrange(dwell_numb, franja, date) %>%
  group_by(dwell_numb, franja) %>%
  mutate(
    Int_T_franja_lag1  = lag(Int_T_franja, 1),
    Int_T_franja_lag2  = lag(Int_T_franja, 2),
    Int_T_franja_lag3  = lag(Int_T_franja, 3),
    Int_T_franja_lag4  = lag(Int_T_franja, 4),
    Int_T_franja_lag5  = lag(Int_T_franja, 5),
    Int_T_franja_lag6  = lag(Int_T_franja, 6),
    Int_T_franja_lag7  = lag(Int_T_franja, 7),
    Int_T_franja_lag8  = lag(Int_T_franja, 8),
    Int_T_franja_lag9  = lag(Int_T_franja, 9),
    Int_T_franja_lag10 = lag(Int_T_franja, 10),
    Int_T_franja_lag11 = lag(Int_T_franja, 11),
    Int_T_franja_lag12 = lag(Int_T_franja, 12)
  ) %>%
  ungroup()

# VARIABLES EXTERIORES
Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dwell_numb, date) %>%
  group_by(dwell_numb) %>%
  mutate(
    Ext_T_mean_lag1 = lag(Ext_T_mean, 1),
    Ext_T_mean_lag2 = lag(Ext_T_mean, 2)
  ) %>%
  ungroup()

# UNION FINAL
Vivtodas_final <- Vivtodas_franja_lags %>%
  left_join(Vivtodas_diario, by = c("dwell_numb", "date"))


#MODELO ARX===================================================
# TRAIN / TEST
train_viviendas <- c(1,3,5,6,7,9,10,12,13)
test_viviendas  <- c(2,4,8,11)

train_data <- Vivtodas_final %>% filter(dwell_numb %in% train_viviendas)
test_data  <- Vivtodas_final %>% filter(dwell_numb %in% test_viviendas)

# MODELO ARX
modelo_franja <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data
)

summary(modelo_franja)

# PREDICCIÓN
test_data <- test_data %>%
  mutate(Int_T_pred = predict(modelo_franja, newdata = test_data)) %>%
  drop_na(Int_T_pred)

# Gráfico de dispersión para variable Int_T_franja
windowsFonts(Times = windowsFont("Times New Roman"))

ggplot(test_data, aes(x = Int_T_franja, y = Int_T_pred)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "#00BFFF", linetype = "solid") +
  labs(
    x = "Real Mean Indoor Temperature (°C)",
    y = "Predicted Mean Indoor Temperature (°C)"
  ) +
  xlim(20, 32) +
  ylim(20, 32) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# METRICAS
R2  <- summary(modelo_franja)$r.squared
RMSE <- rmse(test_data$Int_T_franja, test_data$Int_T_pred)
MAE  <- mae(test_data$Int_T_franja, test_data$Int_T_pred)

cat(
  "R² (train) =", round(R2, 3),
  "\nRMSE (test) =", round(RMSE, 2),
  "\nMAE (test) =", round(MAE, 2)
)
