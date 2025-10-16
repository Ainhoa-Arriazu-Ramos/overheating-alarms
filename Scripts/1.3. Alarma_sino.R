#Importar la base de datos: Vivtodas_diario_media.xlsx

#==========================================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)


#==========================================================================



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
#MÉTODO 1: ALARMA SI/NO
#====================================================================================================


#MLR==========================================================================

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
    residuos_alarma = alarma_real - alarma_test
  )

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data$alarma_real, test_data$alarma_test)

# Crear tabla de contingencia
conf_mat <- table(Real = test_data$alarma_real, Predicho = test_data$alarma_test) %>% 
  as.data.frame()

# Graficar heatmap
ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Todas viviendas",
       x = "Predicted Alarm",
       y = "Real Alarm") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))






#============================================================
#Gráfico de FN-FP... segun intervalos de temperatura
#============================================================

# 1. Clasificar cada fila como TP, TN, FP, FN
test_data <- test_data %>%
  mutate(
    categoria = case_when(
      alarma_real == 1 & alarma_test == 1 ~ "TP",
      alarma_real == 0 & alarma_test == 0 ~ "TN",
      alarma_real == 0 & alarma_test == 1 ~ "FP",
      alarma_real == 1 & alarma_test == 0 ~ "FN"
    )
  )

# 2. Crear columna de rango de temperatura predicha (0.5 °C)
test_data <- test_data %>%
  mutate(
    temp_bin = cut(Int_T_pred,
                   breaks = seq(floor(min(Int_T_pred)), ceiling(max(Int_T_pred)), by = 0.5),
                   include.lowest = TRUE,
                   right = FALSE)
  )

# 3. Contar número de observaciones por temp_bin y categoría
barras_data <- test_data %>%
  group_by(temp_bin, categoria) %>%
  summarise(n = n(), .groups = "drop")

# 4. Definir colores: TP/TN verdes, FP/FN rojos
colores <- c("TP" = "#2ECC71",  # verde
             "TN" = "#27AE60",  # verde oscuro
             "FP" = "#E74C3C",  # rojo
             "FN" = "#C0392B")  # rojo oscuro

# 5. Gráfico de barras apiladas
ggplot(barras_data, aes(x = temp_bin, y = n, fill = categoria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(
    x = "Rango de temperatura interior predicha (°C)",
    y = "Número de observaciones",
    fill = "Categoría",
    title = "Distribución de TP, TN, FP y FN por temperatura predicha"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(face = "bold")
  )
