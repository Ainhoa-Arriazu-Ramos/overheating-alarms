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



#1. Viv_sinnormativa---------------------------------------------------------------------
library(rsample)

set.seed(123)  # Para reproducibilidad
split <- initial_split(Vivtodas_diario_media, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 
                 + Int_T_1 + Int_T_2 + Int_T_3, 
                 data = train_data)

# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)

library(dplyr)
library(tidyr)
library(ggplot2)

# Filtrar solo los datos de julio
test_data_julio <- test_data %>%
  filter(month == 7)

# Reformatear a formato largo para ggplot
test_data_long <- test_data_julio %>%
  select(day, Int_T, Int_T_pred) %>%
  pivot_longer(cols = c(Int_T, Int_T_pred),
               names_to = "tipo",
               values_to = "temperatura")

# Crear el boxplot
ggplot(test_data_long, aes(x = factor(day), y = temperatura, fill = tipo)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Temperaturas reales y predichas por día (Julio)",
       x = "Día de julio",
       y = "Temperatura interior (°C)",
       fill = "Tipo de temperatura") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
