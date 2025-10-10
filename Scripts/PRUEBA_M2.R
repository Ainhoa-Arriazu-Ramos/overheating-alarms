#Importar la base de datos: Vivtodas_diario_media.xlsx

#==========================================================================

#Cargar bibliotecas
library(dplyr)
library(lubridate)
install.packages("pROC")
library(pROC)
library(rsample)
library(ggplot2)
library(caret)

#Quitar grados hora
Vivtodas_diario_media <- Vivtodas_diario_media %>% select(-grados_hora)

#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  # Crear columna de fecha
  mutate(fecha = make_date(year, month, day)) %>%
  
  group_by(dwell_numb) %>%        # agrupar por vivienda
  arrange(dwell_numb, fecha) %>%  # ordenar por fecha
  
  mutate(
    Int_T_1 = lag(Int_T, 1),
    Int_T_2 = lag(Int_T, 2),
    Int_T_3 = lag(Int_T, 3),
    Int_T_4 = lag(Int_T, 4),
    Int_T_5 = lag(Int_T, 5),
    Int_T_6 = lag(Int_T, 6),
    Int_T_7 = lag(Int_T, 7),
    Int_T_8 = lag(Int_T, 8),
    Int_T_9 = lag(Int_T, 9),
    
    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3),
    Ext_T_4 = lag(Ext_T, 4),
    Ext_T_5 = lag(Ext_T, 5),
    Ext_T_6 = lag(Ext_T, 6),
    Ext_T_7 = lag(Ext_T, 7),
    Ext_T_8 = lag(Ext_T, 8),
    Ext_T_9 = lag(Ext_T, 9)
  ) %>%
  ungroup()


# Quitar filas con entradas NA
Vivtodas_diario_media <- na.omit(Vivtodas_diario_media)


#====================================================================================================

#CREAR ALARMA EN BASE A LIMITE ADAPTATIVO DIARIO EN 16798-1:2019 
#1. CREAR LA VARIABLE: Límite diario en base al confort adaptativo EN 16798-1:2019
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3))
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(limiteadap = (0.33 * trm)+21.8)

#2. CREAR VARIABLE: Alarma
#si:
#trm > 30 o
#Int_T > limiteadap 
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(
    alarma_real = if_else(trm > 30 | Int_T > limiteadap, 1, 0)
  )



#====================================================================================================
#MÉTODO 2: PROBABILIDAD DE ALARMA
#====================================================================================================


#ANALISIS CON TODAS LAS VIVIENDAS JUNTAS==========================================================================

# MODELO RLM: predecir temperatura interior
set.seed(123)
split <- initial_split(Vivtodas_diario_media, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 + Ext_T_4 + Ext_T_5 + Ext_T_6 + Ext_T_7 + Ext_T_8 + Ext_T_9
                 + Int_T_1 + Int_T_2 + Int_T_3 + Int_T_4 + Int_T_5 + Int_T_6 + Int_T_7 + Int_T_8 + Int_T_9, 
                 data = train_data)

summary(modelo_rlm)


# Predecir temperatura interior en test
test_data <- test_data %>%
  mutate(
    Int_T_pred = predict(modelo_rlm, newdata = test_data),
    # Aquí queda tu variable alarma_test
    alarma_test = ifelse(trm > 30 | Int_T_pred > limiteadap, 1, 0),
    )

#Añadir tambien la columna Int_T_pred en el train data
train_data$Int_T_pred <- predict(modelo_rlm, newdata = train_data)


# MODELO LOGÍSTICO: predecir alarma en función de Int_T_pred y limiteadap
modelo_logit <- glm(alarma_real ~ Int_T_pred + limiteadap, 
                    data = train_data, family = binomial)

summary(modelo_logit)

# Predecir probabilidad de alarma en test
test_data$prob_alarma <- predict(modelo_logit, newdata = test_data, type = "response")

test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)  # redondea a 1 decimal





#Dicotomizar para poder comparar con alarmas reales========================================================================

#============================================================
# PERCENTILES CONDICIONADOS A ALARMAS REALES: 10,30,50,70
#============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# Probabilidades solo de los casos con alarma real
prob_alarmas_reales <- test_data$prob_alarma[test_data$alarma_real == 1]

# Calcular percentiles 10, 30, 50, 70
percentiles_cond <- quantile(prob_alarmas_reales, probs = c(0.1, 0.3, 0.5, 0.7))
percentiles_cond

# Crear columnas binarias según cada percentil
test_data <- test_data %>%
  mutate(
    alarma_cond10 = ifelse(prob_alarma >= percentiles_cond[1], 1, 0),
    alarma_cond30 = ifelse(prob_alarma >= percentiles_cond[2], 1, 0),
    alarma_cond50 = ifelse(prob_alarma >= percentiles_cond[3], 1, 0),
    alarma_cond70 = ifelse(prob_alarma >= percentiles_cond[4], 1, 0)
  )

# Función para etiquetar tipo de predicción
tipo_pred <- function(pred, real){
  case_when(
    real == 1 & pred == 1 ~ "TP",
    real == 1 & pred == 0 ~ "FN",
    real == 0 & pred == 1 ~ "FP",
    real == 0 & pred == 0 ~ "TN"
  )
}

# Transformar datos a formato largo para ggplot
barras_data <- test_data %>%
  select(alarma_real, alarma_cond10, alarma_cond30, alarma_cond50, alarma_cond70) %>%
  pivot_longer(cols = starts_with("alarma_cond"), names_to = "percentil", values_to = "pred") %>%
  mutate(tipo = tipo_pred(pred, alarma_real)) %>%
  group_by(percentil, tipo) %>%
  summarise(n = n(), .groups = "drop")

# Gráfico de barras apiladas
ggplot(barras_data, aes(x = percentil, y = n, fill = tipo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "TP" = "#2ca02c",   # verde oscuro
    "TN" = "#98df8a",   # verde claro
    "FP" = "#d62728",   # rojo oscuro
    "FN" = "#ff9896"    # rojo claro
  )) +
  labs(title = "TP, FN, FP, TN según percentil condicionado a alarmas reales",
       x = "Percentil condicionado",
       y = "Número de observaciones",
       fill = "Tipo de predicción") +
  theme_minimal(base_size = 14)




#============================================================
# SENSIBILIDAD Y ESPECIFICIDAD PARA PERCENTILES CONDICIONADOS
#============================================================

library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)

# Lista de percentiles condicionados a alarmas reales
prob_alarmas_reales <- test_data$prob_alarma[test_data$alarma_real == 1]
percentiles_cond <- quantile(prob_alarmas_reales, probs = c(0.1, 0.3, 0.5, 0.7))
names(percentiles_cond) <- c("10", "30", "50", "70")

# Crear columnas binarias según cada percentil
for (p in names(percentiles_cond)) {
  test_data[[paste0("alarma_cond", p)]] <- ifelse(test_data$prob_alarma >= percentiles_cond[p], 1, 0)
}

# Función para calcular sensibilidad y especificidad
calc_metrics <- function(pred_col, real) {
  cm <- confusionMatrix(as.factor(pred_col), as.factor(real), positive = "1")
  data.frame(
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  )
}

# Calcular métricas para cada percentil
metrics <- lapply(names(percentiles_cond), function(p){
  df <- calc_metrics(test_data[[paste0("alarma_cond", p)]], test_data$alarma_real)
  df$Percentil <- p
  df
}) %>% bind_rows()

metrics$Percentil <- as.numeric(metrics$Percentil)

# Gráfico de líneas de sensibilidad y especificidad
metrics_long <- metrics %>%
  pivot_longer(cols = c("Sensibilidad", "Especificidad"), names_to = "Metrica", values_to = "Valor")

ggplot(metrics_long, aes(x = Percentil, y = Valor, color = Metrica)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(10, 30, 50, 70)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Sensibilidad y Especificidad según percentil condicionado a alarmas reales",
       x = "Percentil condicionado (%)",
       y = "Valor (%)",
       color = "Métrica") +
  theme_minimal(base_size = 14)





























#Gráfico de FN-FP... segun intervalos de temperatura========================================================================

library(dplyr)
library(ggplot2)

#1.Crear la predicción binaria según tu umbral
prob_umbral <- 0.41  # tu umbral del 41%
test_data <- test_data %>%
  mutate(
    alarma_pred = ifelse(prob_alarma >= prob_umbral, 1, 0),
    tipo = case_when(
      alarma_real == 1 & alarma_pred == 1 ~ "TP",
      alarma_real == 1 & alarma_pred == 0 ~ "FN",
      alarma_real == 0 & alarma_pred == 1 ~ "FP",
      alarma_real == 0 & alarma_pred == 0 ~ "TN"
    ),
#2.Crear intervalos de 0.25ºC para la temperatura predicha
temp_bin = round(Int_T_pred / 0.25) * 0.25
  )

#3.Contar el número de observaciones por tipo en cada intervalo
barras_data <- test_data %>%
  group_by(temp_bin, tipo) %>%
  summarise(n = n(), .groups = "drop")

#4.Gráfico de barras apiladas
ggplot(barras_data, aes(x = factor(temp_bin), y = n, fill = tipo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "TP" = "#2ca02c",   # verde oscuro
    "TN" = "#98df8a",   # verde claro
    "FP" = "#d62728",   # rojo oscuro
    "FN" = "#ff9896"    # rojo claro
  )) +
  labs(title = "TP, FN, FP, TN por temperatura predicha",
       x = "Temperatura interior predicha (ºC)",
       y = "Número de observaciones",
       fill = "Tipo de predicción") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # etiquetas verticales
  )
