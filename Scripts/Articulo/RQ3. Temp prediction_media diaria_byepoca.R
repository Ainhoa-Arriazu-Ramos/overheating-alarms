#=========================================================================
#PREDICCIÓN DIARIA (2 dias previos) - Separando segun época constructiva
#=========================================================================

#Importar la base de datos: Vivtodas_diario_media.xlsx

#Cargar librerías
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)



#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha======================
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








#División en dos subdatasets=================================================================
Viv_antescte <- subset(Vivtodas_diario_media, dwell_numb %in% c(1,2,3,4,5,6)) #Antes del CTE
Viv_despuesscte <- subset(Vivtodas_diario_media, dwell_numb %in% c(7,8,9,10,11,12)) #Después del CTE










#ARX ANTES DEL CTE==========================================================================
# Dividimos el dataset según dwell_numb
train_viviendas_antescte <- c(1, 3, 5, 6)
test_viviendas_antescte  <- c(2, 4)

# Crear subconjuntos
train_data_antescte <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% train_viviendas_antescte)

test_data_antescte <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% test_viviendas_antescte)

#Modelo de ARX con los datos de entrenamiento
modelo_arx_antescte <- lm(Int_T ~ Ext_T + 
                   Ext_T_1 + Ext_T_2 
                 + Int_T_1 + Int_T_2, 
                 data = train_data_antescte)

summary(modelo_arx_antescte)


# Predecir Int_T sobre los datos test
test_data_antescte$Int_T_pred <- predict(modelo_arx_antescte, newdata = test_data_antescte)

#Gráfico de dispersion para variable Int_T
windowsFonts(Times = windowsFont("Times New Roman"))

ggplot(test_data_antescte, aes(x = Int_T, y = Int_T_pred)) +
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

errores <- calcular_errores(test_data_antescte$Int_T, test_data_antescte$Int_T_pred)

data.frame(
  Modelo = c("Viviendas antes del CTE"),
  MSE = c(errores$MSE),
  RMSE = c(errores$RMSE),
  MAE = c(errores$MAE)
)





#ARX DESPUES DEL CTE==========================================================================
# Dividimos el dataset según dwell_numb
train_viviendas_despuescte <- c(7, 9, 11, 12)
test_viviendas_despuescte  <- c(8, 10)

# Crear subconjuntos
train_viviendas_despuescte <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% train_viviendas_despuescte)

test_viviendas_despuescte <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% test_viviendas_despuescte)

#Modelo de ARX con los datos de entrenamiento
modelo_arx_despuescte <- lm(Int_T ~ Ext_T + 
                            Ext_T_1 + Ext_T_2 
                          + Int_T_1 + Int_T_2, 
                          data = train_viviendas_despuescte)

summary(modelo_arx_despuescte)


# Predecir Int_T sobre los datos test
test_viviendas_despuescte$Int_T_pred <- predict(modelo_arx_despuescte, newdata = test_viviendas_despuescte)

#Gráfico de dispersion para variable Int_T
windowsFonts(Times = windowsFont("Times New Roman"))

ggplot(test_viviendas_despuescte, aes(x = Int_T, y = Int_T_pred)) +
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

errores <- calcular_errores(test_viviendas_despuescte$Int_T, test_viviendas_despuescte$Int_T_pred)

data.frame(
  Modelo = c("Viviendas después del CTE"),
  MSE = c(errores$MSE),
  RMSE = c(errores$RMSE),
  MAE = c(errores$MAE)
)
