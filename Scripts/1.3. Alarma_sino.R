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


#ARX==========================================================================

# Dividimos el dataset según dwell_numb
train_viviendas <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_viviendas  <- c(2, 4, 8, 11)

# Crear subconjuntos
train_data <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% train_viviendas)

test_data <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% test_viviendas)



#Modelo de ARX con los datos de entrenamiento
modelo_arx <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 + Ext_T_4 + Ext_T_5 + Ext_T_6 + Ext_T_7 + Ext_T_8 + Ext_T_9
                 + Int_T_1 + Int_T_2 + Int_T_3 + Int_T_4 + Int_T_5 + Int_T_6 + Int_T_7 + Int_T_8 + Int_T_9, 
                 data = train_data)

summary(modelo_arx)

# Predecir temperatura interior en test
test_data <- test_data %>%
  mutate(
    Int_T_pred = predict(modelo_arx, newdata = test_data),
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
windowsFonts(TimesNR = windowsFont("Times New Roman"))

ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 14, family = "TimesNR") +  # Números dentro de las cajas
  scale_fill_gradient(low = "white", high = "steelblue", name = "") +  # Quita "Freq" de la leyenda
  labs(
    x = "Alarm_predicted",
    y = "Alarm_real"
  ) +
  theme_minimal(base_family = "TimesNR") +  # Times en todo el gráfico
  theme(
    axis.text = element_text(size = 18),       # Números de los ejes
    axis.title = element_text(size = 20),      # Títulos de los ejes
    legend.text = element_text(size = 16)      # Números de la barra de colores
  )



#============================================================
#MÉTRICAS DE EVALUACIÓN DEL MODELO DE ALARMAS
#============================================================

# Crear matriz de confusión
conf_mat <- table(Real = test_data$alarma_real, Predicho = test_data$alarma_test)

# Extraer valores
TN <- conf_mat[1,1]  # Verdaderos negativos
FP <- conf_mat[1,2]  # Falsos positivos
FN <- conf_mat[2,1]  # Falsos negativos
TP <- conf_mat[2,2]  # Verdaderos positivos

# Total de observaciones
total <- TN + FP + FN + TP

# Porcentajes de cada tipo de caso:
casos <- data.frame(
  Tipo = c("Verdaderos negativos", "Falsos positivos", "Falsos negativos", "Verdaderos positivos"),
  Cantidad = c(TN, FP, FN, TP),
  Porcentaje = round(100 * c(TN, FP, FN, TP) / total, 1)
)
print(casos)

#=======================================================================================================

# Calcular métricas
accuracy  <- (TP + TN) / total
recall    <- TP / (TP + FN)           # Sensibilidad o Tasa de verdaderos positivos
specificity <- TN / (TN + FP)         # Especificidad o Tasa de verdaderos negativos
precision <- TP / (TP + FP)           # Valor predictivo positivo
f1_score  <- 2 * (precision * recall) / (precision + recall)

# Mostrar resultados en porcentaje
resultados <- data.frame(
  Métrica = c("Accuracy", "Recall (Sensibilidad)", "Specificity", "Precision", "F1-score"),
  Valor = round(100 * c(accuracy, recall, specificity, precision, f1_score), 1)
)

print(resultados)



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



# 5. Gráfico de barras apiladas
# Registrar Times New Roman en Windows
windowsFonts(TimesNR = windowsFont("Times New Roman"))


colores <- c(
  "TP" = "#27AE60",  # verde más intenso (TP)
  "TN" = "#2ECC71",  # verde más suave (TN)
  "FP" = "#E74C3C",  # rojo más suave (FP)
  "FN" = "#C0392B"   # rojo más intenso (FN)
)

barras_data$temp_bin <- factor(barras_data$temp_bin, levels = sort(unique(barras_data$temp_bin)))

ggplot(barras_data, aes(x = temp_bin, y = n, fill = categoria)) +
  geom_bar(stat = "identity") +  # sin borde
  scale_fill_manual(values = colores, name = "") +
  labs(
    x = "Predicted Temperature (°C)",
    y = "Count"
  ) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5), expand = c(0,0)) +  # expand=0 evita ticks extra
  theme_minimal(base_family = "TimesNR") +
  theme(
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14)
  )
