#IMPORTAR BASE DE DATOS Vivreales_Tdiario_20212022
viv_verano_diario <- Vivreales_Tdiario_20212022

#MODELO PREDICTIVO de TEMPERATURA INTERIOR (VARIABLE CONTINUA) ==============================================
install.packages("rsample")
library(rsample)

set.seed(123)  # Para reproducibilidad
split <- initial_split(viv_verano_diario, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_sinint <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3, 
                 data = train_data)

summary(modelo_rlm_sinint)


# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_rlm_sinint, newdata = test_data)

# Métricas para variable continua (Int_T)
mae <- mean(abs(test_data$Int_T_pred - test_data$Int_T))
rmse <- sqrt(mean((test_data$Int_T_pred - test_data$Int_T)^2))
cat("MAE:", mae, "\nRMSE:", rmse)

#Gráfico de dispersion para variable Int_T
library(ggplot2)
ggplot(test_data, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "solid") +
  labs(title = "Dispersión: Temperatura Interior Real vs Predicha",
       x = "Temperatura Interior Real (°C)",
       y = "Temperatura Interior Predicha (°C)") +
  theme_minimal()
