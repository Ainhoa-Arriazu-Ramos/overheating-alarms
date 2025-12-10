#=========================================================
#PREDICCIÓN POR FRANJAS con SUBSETS (2 dias previos-12 LAGS PREVIOS)
#=========================================================


#Importar la base de datos: Vivtodas_diario_media.xlsx


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

#Ver el nombre de las franjas
unique(Vivtodas_final$franja)
  #"Manana"      "Mediodia"    "Noche_0_3"   "Noche_20_23" "Noche_4_7"   "Tarde"






#Dividir el dataset en 6 subsets en base a las franjas de cada dia ===============================
Noche_0_3          <- subset(Vivtodas_final, franja == "Noche_0_3")
Noche_4_7          <- subset(Vivtodas_final, franja == "Noche_4_7")
Manana_8_11        <- subset(Vivtodas_final, franja == "Manana")
Mediodia_12_15     <- subset(Vivtodas_final, franja == "Mediodia")
Tarde_16_19        <- subset(Vivtodas_final, franja == "Tarde")
Noche_20_23        <- subset(Vivtodas_final, franja == "Noche_20_23")




#ARX==========================================================================



#FRNAJA: Noche_0_3
# Dividimos el dataset según dwell_numb
train_viviendas <- c(1,3,5,6,7,9,10,12,13)
test_viviendas  <- c(2,4,8,11)

train_data_Noche_0_3 <- Noche_0_3 %>% filter(dwell_numb %in% train_viviendas)
test_data_Noche_0_3  <- Noche_0_3 %>% filter(dwell_numb %in% test_viviendas)

# MODELO ARX
modelo_Noche_0_3 <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data_Noche_0_3
)

summary(modelo_Noche_0_3)

# PREDICCIÓN
test_data_Noche_0_3 <- test_data_Noche_0_3 %>%
  mutate(Int_T_pred = predict(modelo_Noche_0_3, newdata = test_data_Noche_0_3))



#======================



# FRANJA: Noche_4_7
train_data_Noche_4_7 <- Noche_4_7 %>% filter(dwell_numb %in% train_viviendas)
test_data_Noche_4_7  <- Noche_4_7 %>% filter(dwell_numb %in% test_viviendas)

modelo_Noche_4_7 <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data_Noche_4_7
)

summary(modelo_Noche_4_7)

test_data_Noche_4_7 <- test_data_Noche_4_7 %>%
  mutate(Int_T_pred = predict(modelo_Noche_4_7, newdata = test_data_Noche_4_7))



#======================



# FRANJA: Manana
train_data_Manana_8_11 <- Manana_8_11 %>% filter(dwell_numb %in% train_viviendas)
test_data_Manana_8_11  <- Manana_8_11 %>% filter(dwell_numb %in% test_viviendas)

modelo_Manana_8_11 <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data_Manana_8_11
)

summary(modelo_Manana_8_11)

test_data_Manana_8_11 <- test_data_Manana_8_11 %>%
  mutate(Int_T_pred = predict(modelo_Manana_8_11, newdata = test_data_Manana_8_11))



#======================



# FRANJA: Mediodia
train_data_Mediodia_12_15 <- Mediodia_12_15 %>% filter(dwell_numb %in% train_viviendas)
test_data_Mediodia_12_15  <- Mediodia_12_15 %>% filter(dwell_numb %in% test_viviendas)

modelo_Mediodia_12_15 <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data_Mediodia_12_15
)

summary(modelo_Mediodia_12_15)

test_data_Mediodia_12_15 <- test_data_Mediodia_12_15 %>%
  mutate(Int_T_pred = predict(modelo_Mediodia_12_15, newdata = test_data_Mediodia_12_15))



#======================




# FRANJA: Tarde
train_data_Tarde_16_19 <- Tarde_16_19 %>% filter(dwell_numb %in% train_viviendas)
test_data_Tarde_16_19  <- Tarde_16_19 %>% filter(dwell_numb %in% test_viviendas)

modelo_Tarde_16_19 <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data_Tarde_16_19
)

summary(modelo_Tarde_16_19)

test_data_Tarde_16_19 <- test_data_Tarde_16_19 %>%
  mutate(Int_T_pred = predict(modelo_Tarde_16_19, newdata = test_data_Tarde_16_19))



#======================



# FRANJA: Noche_20_23
train_data_Noche_20_23 <- Noche_20_23 %>% filter(dwell_numb %in% train_viviendas)
test_data_Noche_20_23  <- Noche_20_23 %>% filter(dwell_numb %in% test_viviendas)

modelo_Noche_20_23 <- lm(
  Int_T_franja ~ 
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data_Noche_20_23
)

summary(modelo_Noche_20_23)

test_data_Noche_20_23 <- test_data_Noche_20_23 %>%
  mutate(Int_T_pred = predict(modelo_Noche_20_23, newdata = test_data_Noche_20_23))



#MOSTRAR TODOS LOS RESULTADOS DEL ARX SEGUIDOS====================

# Lista de modelos con nombres bonitos
lista_modelos <- list(
  "Noche_0_3"     = modelo_Noche_0_3,
  "Noche_4_7"     = modelo_Noche_4_7,
  "Manana_8_11"   = modelo_Manana_8_11,
  "Mediodia_12_15"= modelo_Mediodia_12_15,
  "Tarde_16_19"   = modelo_Tarde_16_19,
  "Noche_20_23"   = modelo_Noche_20_23
)

# Mostrar todos los summary uno detrás de otro
for (nombre in names(lista_modelos)) {
  cat("\n========================================\n")
  cat("SUMMARY DEL MODELO:", nombre, "\n")
  cat("========================================\n")
  print(summary(lista_modelos[[nombre]]))
}



#GRÁFICO DE DISPERSIÓN====================

library(ggplot2)
library(dplyr)
library(Metrics)  # para rmse() y mae()
windowsFonts(Times = windowsFont("Times New Roman"))

# Combinar todos los test_data en uno solo
test_combined <- bind_rows(
  test_data_Noche_0_3     %>% mutate(franja = "Noche_0_3"),
  test_data_Noche_4_7     %>% mutate(franja = "Noche_4_7"),
  test_data_Manana_8_11   %>% mutate(franja = "Manana_8_11"),
  test_data_Mediodia_12_15%>% mutate(franja = "Mediodia_12_15"),
  test_data_Tarde_16_19   %>% mutate(franja = "Tarde_16_19"),
  test_data_Noche_20_23   %>% mutate(franja = "Noche_20_23")
)

# Gráfico de dispersión con ggplot

#Todo en un gráfico:
library(RColorBrewer)
paleta_franjas <- brewer.pal(6, "Set1")

ggplot(test_combined, aes(x = Int_T_franja, y = Int_T_pred, color = franja)) +
  geom_point(alpha = 0.4, size = 2) +  # puntos semi-transparentes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.2) +  # línea sólida
  scale_color_manual(values = paleta_franjas) +  # aplicar paleta intensa
  labs(
    x = "Real Mean Indoor Temperature (°C)",
    y = "Predicted Mean Indoor Temperature (°C)",
    color = "Franja"
  ) +
  xlim(20, 32) +
  ylim(20, 32) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )


#Separando en 6 gráficos diferentes:
ggplot(test_combined, aes(x = Int_T_franja, y = Int_T_pred)) +
  geom_point(alpha = 0.4, color = "#0072B2", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00", linetype = "solid", size = 1.2) +
  labs(
    x = "Real Mean Indoor Temperature (°C)",
    y = "Predicted Mean Indoor Temperature (°C)"
  ) +
  xlim(20, 32) +
  ylim(20, 32) +
  coord_fixed(ratio = 1) +
  facet_wrap(~franja, ncol = 3) +  # un panel por franja
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    strip.text = element_text(size = 12)
  )



#MÉTRICAS====================

metrics_list <- list(
  "Noche_0_3"      = test_data_Noche_0_3,
  "Noche_4_7"      = test_data_Noche_4_7,
  "Manana_8_11"    = test_data_Manana_8_11,
  "Mediodia_12_15" = test_data_Mediodia_12_15,
  "Tarde_16_19"    = test_data_Tarde_16_19,
  "Noche_20_23"    = test_data_Noche_20_23
)

for(f in names(metrics_list)){
  df <- metrics_list[[f]]
  modelo_name <- paste0("modelo_", f)
  modelo <- get(modelo_name)
  
  R2   <- summary(modelo)$r.squared
  RMSE_val <- rmse(df$Int_T_franja, df$Int_T_pred)
  MAE_val  <- mae(df$Int_T_franja, df$Int_T_pred)
  
  cat("\n====================================\n")
  cat("Franja:", f, "\n")
  cat("R² (train) =", round(R2, 3),
      "\nRMSE (test) =", round(RMSE_val, 2),
      "\nMAE (test) =", round(MAE_val, 2), "\n")
}




