#IMPORTAR BASE DE DATOS Vivreales_Tdiario_20212022
viv_verano_diario <- Vivreales_Tdiario_20212022

#Modelo de Random Forest ===================================================
set.seed(123)  # Para reproducibilidad

# Dividir en train/test (70% train)
n <- nrow(viv_verano_diario)
train_indices <- sample(1:n, size = 0.7 * n)

train_data <- viv_verano_diario[train_indices, ]
test_data  <- viv_verano_diario[-train_indices, ]

# Cargar paquete randomForest (instalar solo si no está)
# install.packages("randomForest")
library(randomForest)

# Entrenar Random Forest
modelo_rf_train <- randomForest(
  grados_hora ~ Ext_T + Ext_RAD + Int_T_1 + Int_T_2 + Int_T_3 +
    Ext_T_1 + Ext_T_2 + Ext_T_3,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

print(modelo_rf_train)

# Predicciones en test
pred_rf <- predict(modelo_rf_train, newdata = test_data)

# Evaluar rendimiento
real <- test_data$grados_hora
mse_rf <- mean((pred_rf - real)^2)
rmse_rf <- sqrt(mse_rf)
r2_rf <- 1 - sum((real - pred_rf)^2) / sum((real - mean(real))^2)

cat("Random Forest - RMSE:", rmse_rf, " | R²:", r2_rf, "\n")
