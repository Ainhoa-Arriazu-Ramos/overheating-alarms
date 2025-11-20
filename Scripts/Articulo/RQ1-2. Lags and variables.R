# Cargar librerías
library(dplyr)
library(MASS)
library(Metrics)
library(lubridate)
library(purrr)

#===========================================
#CREAR DATASET CON LAGS
#===========================================
# Dataset
df <- Vivtodas_diario_media

# Eliminar variables de RH
df <- df %>% dplyr::select(-Int_RH, -Ext_RH)

# Crear columna fecha
df <- df %>%
  mutate(fecha = make_date(year, month, day))

# Máximo número de días previos a considerar
max_lags <- 30

# Variables para generar lags
vars <- c("Int_T", "Ext_T", "Ext_RAD")

# Agrupar por vivienda y ordenar por fecha
df <- df %>%
  arrange(dwell_numb, fecha) %>%
  group_by(dwell_numb)

# Crear lags automáticamente
for (var in vars) {
  for (lag_i in 1:max_lags) {
    lag_name <- paste0(var, "_", lag_i)
    df[[lag_name]] <- if_else(
      as.integer(df$fecha - lag(df$fecha, lag_i)) == lag_i,
      lag(df[[var]], lag_i),
      NA_real_
    )
  }
}

# Desagrupar
df <- df %>% ungroup()

# Eliminar filas con NA generadas por los lags
df <- df %>% na.omit()

# Resultado final
df_lags <- df
names(df_lags)





#===========================================
#CÁLCULO LAGS OPTIMOS
#===========================================
# Variables rezagadas de entrada
lag_vars <- names(df_lags)[grepl("_\\d+$", names(df_lags))]  # todas las columnas con "_1", "_2", ...

# Nos interesan solo las columnas que sean de Ext_T, Ext_RAD o Int_T
lag_vars <- lag_vars[grepl("Int_T|Ext_T|Ext_RAD", lag_vars)]

# Data frame para guardar RMSE
rmse_results <- data.frame(max_lag = integer(), RMSE = numeric())

# Máximo número de lags a probar (1 hasta max_lags)
for (lag_i in 1:max_lags) {
  
  # Seleccionar solo los lags hasta lag_i
  lags_selected <- lag_vars[grepl(paste0("_([1-", lag_i, "])$"), lag_vars)]
  
  # Fórmula del modelo ARX: Int_T ~ lags seleccionados
  f <- as.formula(paste("Int_T ~", paste(lags_selected, collapse = " + ")))
  
  # Ajustar modelo robusto
  mod <- rlm(f, data = df_lags)
  
  # Calcular RMSE
  rmse_val <- rmse(df_lags$Int_T, predict(mod, df_lags))
  
  # Guardar resultados
  rmse_results <- rbind(rmse_results, data.frame(max_lag = lag_i, RMSE = rmse_val))
}

# Ver resultados
print(rmse_results)

# Graficar RMSE vs número de lags
library(ggplot2)
ggplot(rmse_results, aes(x = max_lag, y = RMSE)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(x = "Número de lags incluidos", y = "RMSE", 
       title = "Optimización de número de lags para modelo ARX") +
  theme_minimal()

#RESULTADO: 9 dias previos es lo óptimo







#===========================================
#MODELOS ARX 
#===========================================

#1. Modelo ARX usando los lags óptimos de 9 días para las tres variables (Int_T, Ext_T, Ext_RAD) para predecir la temperatura interior Int_T
# Número de lags óptimos
optimal_lag <- 9

# Lags a usar (1 a 9) de Int_T, Ext_T, Ext_RAD
lag_vars <- names(df_lags)[grepl(paste0("_(1|2|3|4|5|6|7|8|9)$"), names(df_lags))]
lag_vars <- lag_vars[grepl("Int_T|Ext_T|Ext_RAD", lag_vars)]

# Variables actuales a incluir: Ext_T y Ext_RAD del día a predecir
current_vars <- c("Ext_T", "Ext_RAD")

# Crear fórmula ARX incluyendo lags + variables actuales
formula_arx <- as.formula(
  paste("Int_T ~", paste(c(lag_vars, current_vars), collapse = " + "))
)

# Definir training y test por dwellings
train_dwellings <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_dwellings  <- c(2, 4, 8, 11)

df_train <- df_lags %>% filter(dwell_numb %in% train_dwellings)
df_test  <- df_lags %>% filter(dwell_numb %in% test_dwellings)

# Ajustar modelo ARX robusto con training set
model_arx <- rlm(formula_arx, data = df_train)

# Resumen del modelo
summary(model_arx)

# Predicciones sobre test set
predictions_test <- predict(model_arx, df_test)

# Calcular RMSE sobre test set
rmse_test <- rmse(df_test$Int_T, predictions_test)
print(paste("RMSE del modelo ARX sobre el test set:", round(rmse_test, 4))) #RESULTADO:0.6519 (test)

# Predicciones sobre training set para comparar
predictions_train <- predict(model_arx, df_train)
rmse_train <- rmse(df_train$Int_T, predictions_train)
print(paste("RMSE del modelo ARX sobre el training set:", round(rmse_train, 4))) #RESULTADO:0.6397 (train)

# Función para calcular R²
r2 <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
}

# R2
pred_train_full <- predict(model_arx, df_train)
pred_test_full  <- predict(model_arx, df_test)

r2_train_full <- r2(df_train$Int_T, pred_train_full)
r2_test_full  <- r2(df_test$Int_T, pred_test_full)

print(paste("R² modelo ARX completo - train:", round(r2_train_full, 4))) #RESULTADO: 0.8759 (train)
print(paste("R² modelo ARX completo - test: ", round(r2_test_full, 4))) #RESULTADO: 0.8549 (test)






#2. Modelo usando las variables que son significativas. Ext_T del dia a predecir; Ext_T (lag de 2 dias); Int_T (lag de 2 dias)
# Lags a usar (solo 1 y 2) de Int_T y Ext_T
lag_vars <- names(df_lags)[grepl("_(1|2)$", names(df_lags))]
lag_vars <- lag_vars[grepl("Int_T|Ext_T", lag_vars)]

# Variables actuales a incluir: Ext_T del día a predecir
current_vars <- c("Ext_T")

# Fórmula ARX simplificada
formula_arx_simple <- as.formula(
  paste("Int_T ~", paste(c(lag_vars, current_vars), collapse = " + "))
)

# Definir training y test por dwellings
train_dwellings <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_dwellings  <- c(2, 4, 8, 11)

df_train <- df_lags %>% filter(dwell_numb %in% train_dwellings)
df_test  <- df_lags %>% filter(dwell_numb %in% test_dwellings)

# Ajustar modelo ARX robusto con training set
model_arx_simple <- rlm(formula_arx_simple, data = df_train)

# Resumen del modelo
summary(model_arx_simple)

# Predicciones sobre test set
predictions_test <- predict(model_arx_simple, df_test)

# Calcular RMSE sobre test set
rmse_test <- rmse(df_test$Int_T, predictions_test)
print(paste("RMSE del modelo ARX simplificado sobre el test set:", round(rmse_test, 4))) #RESULTADO: 0.6389 (test)

# Predicciones sobre training set para comparar
predictions_train <- predict(model_arx_simple, df_train)
rmse_train <- rmse(df_train$Int_T, predictions_train)
print(paste("RMSE del modelo ARX simplificado sobre el training set:", round(rmse_train, 4))) #RESULTADO: 0.6517 (train)

# R2
pred_train_simple <- predict(model_arx_simple, df_train)
pred_test_simple  <- predict(model_arx_simple, df_test)

r2_train_simple <- r2(df_train$Int_T, pred_train_simple)
r2_test_simple  <- r2(df_test$Int_T, pred_test_simple)

print(paste("R² modelo ARX simplificado - train:", round(r2_train_simple, 4)))#RESULTADO: 0.8712 (train)
print(paste("R² modelo ARX simplificado - test: ", round(r2_test_simple, 4))) #RESULTADO: 0.8606 (test)




#3.1. Modelo sin variables interiores y solo dos dias antes
#Lags a usar (solo 1 y 2) de Ext_T
lag_vars <- names(df_lags)[grepl("_(1|2)$", names(df_lags))]
lag_vars <- lag_vars[grepl("Ext_T", lag_vars)]  # Solo lags de temperatura exterior

# Variables actuales a incluir: Ext_T del día a predecir
current_vars <- c("Ext_T")

# Fórmula ARX solo con temperatura exterior
formula_arx_ext <- as.formula(
  paste("Int_T ~", paste(c(lag_vars, current_vars), collapse = " + "))
)

# Definir training y test por dwellings
train_dwellings <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_dwellings  <- c(2, 4, 8, 11)

df_train <- df_lags %>% filter(dwell_numb %in% train_dwellings)
df_test  <- df_lags %>% filter(dwell_numb %in% test_dwellings)

# Ajustar modelo ARX robusto con training set
model_arx_ext <- rlm(formula_arx_ext, data = df_train)

# Resumen del modelo
summary(model_arx_ext)

# Predicciones sobre test set
predictions_test <- predict(model_arx_ext, df_test)

# Calcular RMSE sobre test set
rmse_test <- rmse(df_test$Int_T, predictions_test)
print(paste("RMSE modelo ARX solo Ext_T - test set:", round(rmse_test, 4)))#RESULTADO: 1.2402 (test)

# Predicciones sobre training set para comparar
predictions_train <- predict(model_arx_ext, df_train)
rmse_train <- rmse(df_train$Int_T, predictions_train)
print(paste("RMSE modelo ARX solo Ext_T - train set:", round(rmse_train, 4))) #RESULTADO: 1.2386 (train)

# R²
r2 <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
}

r2_train <- r2(df_train$Int_T, predictions_train)
r2_test  <- r2(df_test$Int_T, predictions_test)

print(paste("R² modelo ARX solo Ext_T - train:", round(r2_train, 4))) #RESULTADO: 0.5349 (train)
print(paste("R² modelo ARX solo Ext_T - test: ", round(r2_test, 4))) #RESULTADO: 0.4748 (test)





#3.2. Modelo sin variables interiores y 9 dias antes
#Lags a usar (1 a 9) de Ext_T
lag_vars <- names(df_lags)[grepl("_(1|2|3|4|5|6|7|8|9)$", names(df_lags))]
lag_vars <- lag_vars[grepl("Ext_T", lag_vars)]  # Solo lags de temperatura exterior

# Variables actuales a incluir: Ext_T del día a predecir
current_vars <- c("Ext_T")

# Fórmula ARX solo con temperatura exterior y 9 lags
formula_arx_ext9 <- as.formula(
  paste("Int_T ~", paste(c(lag_vars, current_vars), collapse = " + "))
)

# Definir training y test por dwellings
train_dwellings <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_dwellings  <- c(2, 4, 8, 11)

df_train <- df_lags %>% filter(dwell_numb %in% train_dwellings)
df_test  <- df_lags %>% filter(dwell_numb %in% test_dwellings)

# Ajustar modelo ARX robusto con training set
model_arx_ext9 <- rlm(formula_arx_ext9, data = df_train)

# Resumen del modelo
summary(model_arx_ext9)

# Predicciones sobre test set
predictions_test <- predict(model_arx_ext9, df_test)

# Calcular RMSE sobre test set
rmse_test <- rmse(df_test$Int_T, predictions_test)
print(paste("RMSE modelo ARX solo Ext_T (9 lags) - test set:", round(rmse_test, 4))) #RESULTADO: 1.1786 (test)

# Predicciones sobre training set para comparar
predictions_train <- predict(model_arx_ext9, df_train)
rmse_train <- rmse(df_train$Int_T, predictions_train)
print(paste("RMSE modelo ARX solo Ext_T (9 lags) - train set:", round(rmse_train, 4))) #RESULTADO: 1.1861 (train)

# R²
r2 <- function(y_true, y_pred) {
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
}

r2_train <- r2(df_train$Int_T, predictions_train)
r2_test  <- r2(df_test$Int_T, predictions_test)

print(paste("R² modelo ARX solo Ext_T (9 lags) - train:", round(r2_train, 4)))#RESULTADO: 0.5735 (train)
print(paste("R² modelo ARX solo Ext_T (9 lags) - test: ", round(r2_test, 4)))#RESULTADO: 0.5257 (test)
