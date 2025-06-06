#IMPORTAR BASE DE DATOS Vivreales_Tdiario_20212022
viv_verano_diario <- Vivreales_Tdiario_20212022

library(dplyr)
library(ggplot2)
library(rsample)
library(yardstick)

max_lag <- 15

for (i in 1:max_lag) {
  viv_verano_diario <- viv_verano_diario %>%
    group_by(dwell_numb) %>%
    mutate(!!paste0("Int_T_", i) := lag(Int_T, i),
           !!paste0("Ext_T_", i) := lag(Ext_T, i)) %>%
    ungroup()
}

viv_verano_diario <- na.omit(viv_verano_diario)

set.seed(123)
split <- initial_split(viv_verano_diario, prop = 0.7)
train_data <- training(split)
test_data  <- testing(split)

results <- data.frame(Lags = integer(), RMSE = double(), R2 = double())

for (n_lags in 1:max_lag) {
  
  int_lags <- paste0("Int_T_", 1:n_lags)
  ext_lags <- paste0("Ext_T_", 1:n_lags)
  predictors <- c("Ext_T", "Ext_RAD", int_lags, ext_lags)
  
  formula_lm <- as.formula(paste("grados_hora ~", paste(predictors, collapse = " + ")))
  
  # Entrenar modelo de regresión lineal múltiple
  lm_model <- lm(formula = formula_lm, data = train_data)
  
  # Predicción
  preds <- predict(lm_model, newdata = test_data)
  
  # Métricas
  rmse_val <- yardstick::rmse_vec(truth = test_data$grados_hora, estimate = preds)
  r2_val   <- yardstick::rsq_vec(truth = test_data$grados_hora, estimate = preds)
  
  results <- rbind(results, data.frame(Lags = n_lags, RMSE = rmse_val, R2 = r2_val))
}

# Gráfico solo R²
ggplot(results, aes(x = Lags, y = R2)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "R² según número de días previos incluidos (lags)",
    x = "Número de días previos incluidos (lags)",
    y = "R²"
  ) +
  theme_minimal()

# Gráfico solo RMSE
ggplot(results, aes(x = Lags, y = RMSE)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "RMSE según número de días previos incluidos (lags)",
    x = "Número de días previos incluidos (lags)",
    y = "RMSE"
  ) +
  theme_minimal()











library(dplyr)
library(rsample)
library(yardstick)

set.seed(123)

# Número de folds para cross-validation
k <- 5

# Crear los folds (particiones)
folds <- vfold_cv(viv_verano_diario, v = k, strata = NULL)

max_lag <- 15
results_cv <- data.frame(Lags = integer(), RMSE = double(), R2 = double())

for (n_lags in 1:max_lag) {
  
  int_lags <- paste0("Int_T_", 1:n_lags)
  ext_lags <- paste0("Ext_T_", 1:n_lags)
  predictors <- c("Ext_T", "Ext_RAD", int_lags, ext_lags)
  
  formula_lm <- as.formula(paste("grados_hora ~", paste(predictors, collapse = " + ")))
  
  # Guardar métricas para cada fold
  rmse_folds <- c()
  r2_folds <- c()
  
  for (fold in folds$splits) {
    train_data <- analysis(fold)
    test_data  <- assessment(fold)
    
    # Ajustar modelo
    lm_model <- lm(formula = formula_lm, data = train_data)
    
    # Predecir
    preds <- predict(lm_model, newdata = test_data)
    
    # Calcular métricas
    rmse_val <- yardstick::rmse_vec(truth = test_data$grados_hora, estimate = preds)
    r2_val   <- yardstick::rsq_vec(truth = test_data$grados_hora, estimate = preds)
    
    rmse_folds <- c(rmse_folds, rmse_val)
    r2_folds <- c(r2_folds, r2_val)
  }
  
  # Promedio de métricas en los folds
  mean_rmse <- mean(rmse_folds)
  mean_r2 <- mean(r2_folds)
  
  results_cv <- rbind(results_cv, data.frame(Lags = n_lags, RMSE = mean_rmse, R2 = mean_r2))
}

# Graficar resultados
library(ggplot2)

ggplot(results_cv, aes(x = Lags)) +
  geom_line(aes(y = R2), color = "steelblue", size = 1.2) +
  geom_point(aes(y = R2), color = "steelblue", size = 2) +
  labs(
    title = "Cross-Validation: R² según número de días previos (lags)",
    x = "Número de días previos incluidos (lags)",
    y = "R²"
  ) +
  theme_minimal()

ggplot(results_cv, aes(x = Lags)) +
  geom_line(aes(y = RMSE), color = "darkred", size = 1.2) +
  geom_point(aes(y = RMSE), color = "darkred", size = 2) +
  labs(
    title = "Cross-Validation: RMSE según número de días previos (lags)",
    x = "Número de días previos incluidos (lags)",
    y = "RMSE"
  ) +
  theme_minimal()
