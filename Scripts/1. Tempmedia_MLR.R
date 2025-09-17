#Importar la base de datos: Vivtodas_diario_media.xlsx

#==========================================================================

#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha
library(dplyr)
library(lubridate)

Vivtodas_diario_media <- Vivtodas_diario_media %>%
  # Crear columna de fecha
  mutate(fecha = make_date(year, month, day)) %>%
  
  group_by(dwell_numb) %>%        # agrupar por vivienda
  arrange(dwell_numb, fecha) %>%  # ordenar por fecha
  
  mutate(
    Int_T_1 = lag(Int_T, 1),   # día anterior
    Int_T_2 = lag(Int_T, 2),   # dos días atrás
    Int_T_3 = lag(Int_T, 3),   
    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3)
  ) %>%
  ungroup()


# Quitar filas con entradas NA
Vivtodas_diario_media <- na.omit(Vivtodas_diario_media)



#==========================================================================
#Con todas las viviendas juntas
set.seed(123)
split <- initial_split(Vivtodas_diario_media, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data)

summary(modelo_rlm)


# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)

#Gráfico de dispersion para variable Int_T
ggplot(test_data, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Todas viviendas: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  xlim(20, 30) +
  ylim(20, 30) +
  coord_fixed(ratio = 1) +
  theme_minimal()


#==========================================================================



#Dividir base segun periodos constructivos->Cuatro bases de datos distintas
library(dplyr)

Viv_sinnormativa_diamed <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(1, 2, 3, 4))

Viv_ct79_diamed <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(5, 6))

Viv_cte2006_diamed <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(7, 8, 9))

Viv_cte2019_diamed <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(10, 11, 12))


#====================================================================================================


#MODELO PREDICTIVO de TEMPERATURA INTERIOR MEDIA (VARIABLE CONTINUA) 
install.packages("rsample")
library(rsample)
library(ggplot2)


#1. Viv_sinnormativa_diamed---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_sinnormativa_diamed, prop = 0.7)  # 70% train, 30% test
train_data_1 <- training(split)
test_data_1 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_1 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 
                 + Int_T_1 + Int_T_2 + Int_T_3, 
                 data = train_data_1)

summary(modelo_rlm_1)


# Predecir Int_T sobre los datos test
test_data_1$Int_T_pred <- predict(modelo_rlm_1, newdata = test_data_1)

#Gráfico de dispersion para variable Int_T
ggplot(test_data_1, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Viv_sin normativa: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  xlim(20, 30) +
  ylim(20, 30) +
  coord_fixed(ratio = 1) +
  theme_minimal()






#2. Viv_ct79_diamed---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_ct79_diamed, prop = 0.7)  # 70% train, 30% test
train_data_2 <- training(split)
test_data_2 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_2 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_2)

summary(modelo_rlm_2)


# Predecir Int_T sobre los datos test
test_data_2$Int_T_pred <- predict(modelo_rlm_2, newdata = test_data_2)

#Gráfico de dispersion para variable Int_T
ggplot(test_data_2, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Viv_ct79: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  xlim(20, 30) +
  ylim(20, 30) +
  coord_fixed(ratio = 1) +
  theme_minimal()





#3. Viv_cte2006_diamed---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2006_diamed, prop = 0.7)  # 70% train, 30% test
train_data_3 <- training(split)
test_data_3 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_3 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_3)

summary(modelo_rlm_3)


# Predecir Int_T sobre los datos test
test_data_3$Int_T_pred <- predict(modelo_rlm_3, newdata = test_data_3)

#Gráfico de dispersion para variable Int_T
ggplot(test_data_3, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Viv_cte2006: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  xlim(20, 30) +
  ylim(20, 30) +
  coord_fixed(ratio = 1) +
  theme_minimal()




#4. Viv_cte2019_diamed---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2019_diamed, prop = 0.7)  # 70% train, 30% test
train_data_4 <- training(split)
test_data_4 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_4 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_4)

summary(modelo_rlm_4)


# Predecir Int_T sobre los datos test
test_data_4$Int_T_pred <- predict(modelo_rlm_4, newdata = test_data_4)

#Gráfico de dispersion para variable Int_T
ggplot(test_data_4, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Viv_cte2019: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  xlim(20, 30) +
  ylim(20, 30) +
  coord_fixed(ratio = 1) +
  theme_minimal()


# CÁLCULO DE ERRORES CON MAE
calcular_errores <- function(real, pred) {
  mse <- mean((real - pred)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  mae <- mean(abs(real - pred), na.rm = TRUE)
  return(list(MSE = mse, RMSE = rmse, MAE = mae))
}

errores_1 <- calcular_errores(test_data_1$Int_T, test_data_1$Int_T_pred)
errores_2 <- calcular_errores(test_data_2$Int_T, test_data_2$Int_T_pred)
errores_3 <- calcular_errores(test_data_3$Int_T, test_data_3$Int_T_pred)
errores_4 <- calcular_errores(test_data_4$Int_T, test_data_4$Int_T_pred)

data.frame(
  Modelo = c("Sin normativa", "CT-79", "CTE 2006", "CTE 2019"),
  MSE = c(errores_1$MSE, errores_2$MSE, errores_3$MSE, errores_4$MSE),
  RMSE = c(errores_1$RMSE, errores_2$RMSE, errores_3$RMSE, errores_4$RMSE),
  MAE = c(errores_1$MAE, errores_2$MAE, errores_3$MAE, errores_4$MAE)
)

