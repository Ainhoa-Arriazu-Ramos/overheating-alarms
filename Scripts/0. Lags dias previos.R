# Cargar librerías
library(dplyr)
library(MASS)
library(Metrics)

# Dataset
df <- Vivtodas_diario_media

# Máximo número de días previos a considerar (esto puede ser más)
max_lags <- 30

# Crear lags para cada variable predictora
for (lag in 1:max_lags) {
  df <- df %>%
    mutate(
      !!paste0("Ext_T_lag", lag) := lag(Ext_T, lag),
      !!paste0("Ext_RH_lag", lag) := lag(Ext_HR, lag),
      !!paste0("Ext_RAD_lag", lag) := lag(Ext_RAD, lag),
      !!paste0("Int_RH_lag", lag) := lag(Int_RH, lag)
    )
}

# Quitar filas con NA generados por los lags
df <- na.omit(df)

# Obtener nombres de variables rezagadas
predictors <- names(df)[grepl("_lag", names(df))]

# Función para calcular RMSE según máximo lag incluido
rmse_results <- data.frame(max_lag = integer(), RMSE = numeric())

for (max_lag in 1:max_lags) {
  # Seleccionamos los lags hasta el día max_lag
  lags_vars <- grep(paste0("_lag[1-", max_lag, "]"), predictors, value = TRUE)
  
  # Fórmula del modelo
  f <- as.formula(paste("Int_T ~", paste(lags_vars, collapse = " + ")))
  
  # Ajuste del modelo RLM
  mod <- rlm(f, data = df)
  
  # Calcular RMSE
  rmse_val <- rmse(df$Int_T, predict(mod, df))
  
  # Guardar resultados
  rmse_results <- rbind(rmse_results, data.frame(max_lag = max_lag, RMSE = rmse_val))
}

print("RMSE según número de días previos considerados:")
print(rmse_results)

