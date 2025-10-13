#Importar base de datos: Vivtodas_diario_media.xlsx

# Cargar librerías
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)
library(pROC)


#==================================================================
# 1️⃣ Preparar la base de datos
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(fecha = make_date(year, month, day)) %>%
  group_by(dwell_numb) %>%
  arrange(dwell_numb, fecha) %>%
  mutate(
    Int_T_1 = lag(Int_T, 1),
    Int_T_2 = lag(Int_T, 2),
    Int_T_3 = lag(Int_T, 3),
    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3)
  ) %>%
  ungroup() %>%
  na.omit() %>%
  mutate(
    trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3),
    limiteadap = (0.33 * trm) + 21.8,
    alarma_real = if_else(trm > 30 | Int_T > limiteadap, 1, 0)
  )

#==================================================================
# 2️⃣ Dividir por periodos constructivos
Viv_sinnormativa <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(1, 2, 3, 4))
Viv_ct79        <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(5, 6))
Viv_cte2006     <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(7, 8, 9))
Viv_cte2019     <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(10, 11, 12))

#==================================================================
# 3️⃣ Función para procesar Método 1 y Método 2
procesar_viviendas <- function(df){
  
  # Dividir en train/test
  set.seed(123)
  split <- initial_split(df, prop = 0.7)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Modelo lineal para predecir temperatura
  modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD +
                     Ext_T_1 + Ext_T_2 + Ext_T_3 +
                     Int_T_1 + Int_T_2 + Int_T_3,
                   data = train_data)
  
  # Predecir Int_T
  test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)
  
  # Método 1: Alarma binaria
  test_data$Alarma_pred <- ifelse(test_data$trm > 30 | test_data$Int_T_pred > test_data$limiteadap, 1, 0)
  
  # Método 2: Probabilidad de superar límite (usando sigma de los residuos)
  sigma <- sd(resid(modelo_rlm))
  test_data$prob_alarma <- 1 - pnorm(test_data$limiteadap, mean = test_data$Int_T_pred, sd = sigma)
  test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)
  
  # Seleccionar columnas finales
  test_data <- test_data %>%
    select(dwell_numb, fecha, Int_T, limiteadap, alarma_real, Int_T_pred, Alarma_pred, prob_alarma, prob_alarma_pct)
  
  return(test_data)
}

#==================================================================
# 4️⃣ Aplicar función a cada grupo
test_sinnormativa <- procesar_viviendas(Viv_sinnormativa)
test_ct79        <- procesar_viviendas(Viv_ct79)
test_cte2006     <- procesar_viviendas(Viv_cte2006)
test_cte2019     <- procesar_viviendas(Viv_cte2019)


#==================================================================
#GRÁFICOS

graficar_scatter_alarma_quadrantes <- function(df, titulo){
  
  # Umbral que deja por encima el 90% de TP
  df_tp <- df %>% filter(alarma_real == 1)
  umbral_90 <- quantile(df_tp$Int_T_pred, probs = 0.1)
  
  ggplot(df, aes(x = Int_T, y = Int_T_pred, color = factor(alarma_real))) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    # Líneas de cuadrantes
    geom_hline(yintercept = umbral_90, color = "blue", linetype = "solid", size = 1) +  # vertical para TP90
    geom_vline(xintercept = mean(df$limiteadap), color = "red", linetype = "solid", size = 1) +  # horizontal para limite diario
    labs(title = titulo,
         x = "Temperatura Interior Real (°C)",
         y = "Temperatura Interior Predicha (°C)",
         color = "Alarma Real") +
    theme_minimal() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          plot.title=element_text(size=16, face="bold"))
}

# Graficar cada grupo
plot_sinnormativa <- graficar_scatter_alarma_quadrantes(test_sinnormativa, "Viv_sinnormativa")
plot_ct79        <- graficar_scatter_alarma_quadrantes(test_ct79, "Viv_ct79")
plot_cte2006     <- graficar_scatter_alarma_quadrantes(test_cte2006, "Viv_cte2006")
plot_cte2019     <- graficar_scatter_alarma_quadrantes(test_cte2019, "Viv_cte2019")

plot_sinnormativa
plot_ct79
plot_cte2006
plot_cte2019
