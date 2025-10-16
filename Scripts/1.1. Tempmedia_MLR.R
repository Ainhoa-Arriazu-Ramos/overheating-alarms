#Importar la base de datos: Vivtodas_diario_media.xlsx

#==========================================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)


#==========================================================================


#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  # Crear columna de fecha
  mutate(fecha = make_date(year, month, day)) %>%
  
  group_by(dwell_numb) %>%        # agrupar por vivienda
  arrange(dwell_numb, fecha) %>%  # ordenar por fecha
  
  mutate(
    Int_T_1 = lag(Int_T, 1),
    Int_T_2 = lag(Int_T, 2),
    Int_T_3 = lag(Int_T, 3),
    Int_T_4 = lag(Int_T, 4),
    Int_T_5 = lag(Int_T, 5),
    Int_T_6 = lag(Int_T, 6),
    Int_T_7 = lag(Int_T, 7),
    Int_T_8 = lag(Int_T, 8),
    Int_T_9 = lag(Int_T, 9),
    
    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3),
    Ext_T_4 = lag(Ext_T, 4),
    Ext_T_5 = lag(Ext_T, 5),
    Ext_T_6 = lag(Ext_T, 6),
    Ext_T_7 = lag(Ext_T, 7),
    Ext_T_8 = lag(Ext_T, 8),
    Ext_T_9 = lag(Ext_T, 9)
  ) %>%
  ungroup()


# Quitar filas con entradas NA
Vivtodas_diario_media <- na.omit(Vivtodas_diario_media)



#MLR==========================================================================
set.seed(123)
split <- initial_split(Vivtodas_diario_media, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 + Ext_T_4 + Ext_T_5 + Ext_T_6 + Ext_T_7 + Ext_T_8 + Ext_T_9
                   + Int_T_1 + Int_T_2 + Int_T_3 + Int_T_4 + Int_T_5 + Int_T_6 + Int_T_7 + Int_T_8 + Int_T_9, 
                   data = train_data)

summary(modelo_rlm)


# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)

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