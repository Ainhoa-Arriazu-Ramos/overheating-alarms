#=========================================================
#PREDICCIÓN POR FRANJAS con SUBSETS (2 dias previos-12 LAGS PREVIOS)
#=========================================================


#Importar la base de datos: Vivtodas_verano_horario.xlsx


#Cargar librerias
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(Metrics)


# Crear variable fecha
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day))

# DEFINICIÓN DE FRANJAS HORARIAS
franjas <- tibble(
  hour = 0:23,
  franja = case_when(
    hour %in% 0:3   ~ "Noche_0_3",
    hour %in% 4:7   ~ "Noche_4_7",
    hour %in% 8:11  ~ "Manana",
    hour %in% 12:15 ~ "Mediodia",
    hour %in% 16:19 ~ "Tarde",
    hour %in% 20:23 ~ "Noche_20_23"
  ),
  peso = case_when(
    hour %in% 0:7   ~ 3,
    hour %in% 8:11  ~ 1,
    hour %in% 12:15 ~ 2,
    hour %in% 16:19 ~ 2,
    hour %in% 20:23 ~ 1.5
  )
)

Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")

# TEMPERATURA MEDIA POR FRANJA
Vivtodas_franja <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date, franja) %>%
  summarise(
    Int_T_franja = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  )

# 12 LAGS AGRUPADOS POR VIVIENDA Y FRANJA
Vivtodas_franja_lags <- Vivtodas_franja %>%
  arrange(dwell_numb, franja, date) %>%
  group_by(dwell_numb, franja) %>%
  mutate(
    Int_T_franja_lag1  = lag(Int_T_franja, 1),
    Int_T_franja_lag2  = lag(Int_T_franja, 2),
    Int_T_franja_lag3  = lag(Int_T_franja, 3),
    Int_T_franja_lag4  = lag(Int_T_franja, 4),
    Int_T_franja_lag5  = lag(Int_T_franja, 5),
    Int_T_franja_lag6  = lag(Int_T_franja, 6),
    Int_T_franja_lag7  = lag(Int_T_franja, 7),
    Int_T_franja_lag8  = lag(Int_T_franja, 8),
    Int_T_franja_lag9  = lag(Int_T_franja, 9),
    Int_T_franja_lag10 = lag(Int_T_franja, 10),
    Int_T_franja_lag11 = lag(Int_T_franja, 11),
    Int_T_franja_lag12 = lag(Int_T_franja, 12)
  ) %>%
  ungroup()

# VARIABLES EXTERIORES
Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dwell_numb, date) %>%
  group_by(dwell_numb) %>%
  mutate(
    Ext_T_mean_lag1 = lag(Ext_T_mean, 1),
    Ext_T_mean_lag2 = lag(Ext_T_mean, 2)
  ) %>%
  ungroup()

# UNION FINAL
Vivtodas_final <- Vivtodas_franja_lags %>%
  left_join(Vivtodas_diario, by = c("dwell_numb", "date"))

# Quitar filas con entradas NA
Vivtodas_final <- na.omit(Vivtodas_final)





#División en dos subdatasets=================================================================
Viv_antescte <- subset(Vivtodas_final, dwell_numb %in% c(1,2,3,4,5,6)) #Antes del CTE
Viv_despuesscte <- subset(Vivtodas_final, dwell_numb %in% c(7,8,9,10,11,12)) #Después del CTE


#---------------------------------------------------------
# 1. Crear los dos grupos
#---------------------------------------------------------

Viv_antescte      <- Vivtodas_final %>% filter(dwell_numb %in% 1:6)
Viv_despuesscte   <- Vivtodas_final %>% filter(dwell_numb %in% 7:12)

grupos <- list(
  "AntesCTE"  = Viv_antescte,
  "DespuesCTE" = Viv_despuesscte
)

#---------------------------------------------------------
# 2. Franjas horarias a procesar automáticamente
#---------------------------------------------------------

franjas_list <- c("Noche_0_3","Noche_4_7","Manana","Mediodia","Tarde","Noche_20_23")

#---------------------------------------------------------
# 3. Train–test específico para cada grupo
#---------------------------------------------------------

train_test_list <- list(
  "AntesCTE" = list(
    train = c(1,3,5,6),
    test  = c(2,4)
  ),
  "DespuesCTE" = list(
    train = c(7,9,11,12),
    test  = c(8,10)
  )
)

#---------------------------------------------------------
# 4. Función para ajustar modelo ARX
#---------------------------------------------------------

ajustar_modelo_arx <- function(df){
  lm(
    Int_T_franja ~ 
      Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
      Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
      Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
      Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
    data = df
  )
}

#---------------------------------------------------------
# 5. Bucle general: 6 franjas × 2 grupos con train/test diferentes
#---------------------------------------------------------

resultados_modelos <- list()
resultados_pred <- list()
metricas <- data.frame()

for(g in names(grupos)){
  
  df_group <- grupos[[g]]
  
  # train y test específicos de ese grupo
  train_viv <- train_test_list[[g]]$train
  test_viv  <- train_test_list[[g]]$test
  
  for(f in franjas_list){
    
    # subset de esa franja
    df_franja <- df_group %>% filter(franja == f)
    
    # split
    train_data <- df_franja %>% filter(dwell_numb %in% train_viv)
    test_data  <- df_franja %>% filter(dwell_numb %in% test_viv)
    
    # Entrena modelo
    modelo <- ajustar_modelo_arx(train_data)
    
    # Guarda modelo
    resultados_modelos[[paste(g,f,sep="_")]] <- modelo
    
    # Predicciones
    test_data <- test_data %>%
      mutate(Int_T_pred = predict(modelo, newdata = test_data))
    
    resultados_pred[[paste(g,f,sep="_")]] <- test_data
    
    # Métricas
    metricas <- rbind(metricas,
                      data.frame(
                        Grupo = g,
                        Franja = f,
                        R2   = round(summary(modelo)$r.squared, 3),
                        RMSE = round(rmse(test_data$Int_T_franja, test_data$Int_T_pred), 2),
                        MAE  = round(mae(test_data$Int_T_franja, test_data$Int_T_pred), 2)
                      )
    )
  }
}

#=========================================================
# 6. Output de métricas
#=========================================================

print(metricas)
