#Importar la bases de datos completa: Vivreales_Tdiario_20212022

#Dividir base segun periodos constructivos->Cuatro bases de datos distintas
library(dplyr)

Viv_sinnormativa <- Vivreales_Tdiario_20212022 %>%
  filter(dwell_numb %in% c(1, 2, 3, 4))

Viv_ct79 <- Vivreales_Tdiario_20212022 %>%
  filter(dwell_numb %in% c(5, 6))

Viv_cte2006 <- Vivreales_Tdiario_20212022 %>%
  filter(dwell_numb %in% c(7, 8, 9))

Viv_cte2019 <- Vivreales_Tdiario_20212022 %>%
  filter(dwell_numb %in% c(10, 11, 12))



#MODELO PREDICTIVO de TEMPERATURA INTERIOR (VARIABLE CONTINUA) ==============================================
install.packages("rsample")
library(rsample)
library(ggplot2)


#1. Viv_sinnormativa---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_sinnormativa, prop = 0.7)  # 70% train, 30% test
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






#2. Viv_ct79---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_ct79, prop = 0.7)  # 70% train, 30% test
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





#3. Viv_cte2006---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2006, prop = 0.7)  # 70% train, 30% test
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




#4. Viv_cte2019---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2019, prop = 0.7)  # 70% train, 30% test
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


#CÁLCULO DE ERRORES

calcular_errores <- function(real, pred) {
  mse <- mean((real - pred)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  return(list(MSE = mse, RMSE = rmse))
}

errores_1 <- calcular_errores(test_data_1$Int_T, test_data_1$Int_T_pred)
errores_2 <- calcular_errores(test_data_2$Int_T, test_data_2$Int_T_pred)
errores_3 <- calcular_errores(test_data_3$Int_T, test_data_3$Int_T_pred)
errores_4 <- calcular_errores(test_data_4$Int_T, test_data_4$Int_T_pred)

data.frame(
  Modelo = c("Sin normativa", "CT-79", "CTE 2006", "CTE 2019"),
  MSE = c(errores_1$MSE, errores_2$MSE, errores_3$MSE, errores_4$MSE),
  RMSE = c(errores_1$RMSE, errores_2$RMSE, errores_3$RMSE, errores_4$RMSE)
)

