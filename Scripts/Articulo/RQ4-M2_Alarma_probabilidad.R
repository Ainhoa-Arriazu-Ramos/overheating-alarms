#Importar la base de datos: Vivtodas_diario_media.xlsx

#==========================================================================

#Cargar librerías
library(dplyr)
library(lubridate)
install.packages("pROC")
library(pROC)
library(rsample)
library(ggplot2)
library(caret)
library(tidyr)

#==========================================================================

#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(fecha = make_date(year, month, day)) %>%
  group_by(dwell_numb) %>%
  arrange(fecha) %>%
  mutate(
    Int_T_1 = if_else(as.integer(fecha - lag(fecha, 1)) == 1, lag(Int_T, 1), NA_real_),
    Int_T_2 = if_else(as.integer(fecha - lag(fecha, 2)) == 2, lag(Int_T, 2), NA_real_),
    
    Ext_T_1 = if_else(as.integer(fecha - lag(fecha, 1)) == 1, lag(Ext_T, 1), NA_real_),
    Ext_T_2 = if_else(as.integer(fecha - lag(fecha, 2)) == 2, lag(Ext_T, 2), NA_real_),
    Ext_T_3 = if_else(as.integer(fecha - lag(fecha, 3)) == 3, lag(Ext_T, 3), NA_real_),
    
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
modelo_arx <- lm(Int_T ~ Ext_T + 
                   Ext_T_1 + Ext_T_2 
                 + Int_T_1 + Int_T_2, 
                 data = train_data)

summary(modelo_arx)


# Predecir temperatura interior en test
test_data <- test_data %>%
  mutate(
    Int_T_pred = predict(modelo_arx, newdata = test_data),
    # Aquí queda tu variable alarma_test
    alarma_test = ifelse(trm > 30 | Int_T_pred > limiteadap, 1, 0),
    )

#Añadir tambien la columna Int_T_pred en el train data
train_data$Int_T_pred <- predict(modelo_arx, newdata = train_data)


# MODELO LOGÍSTICO: predecir alarma en función de Int_T_pred y limiteadap======================
modelo_logit <- glm(alarma_real ~ Int_T_pred + limiteadap, 
                    data = train_data, family = binomial)

summary(modelo_logit)

# Predecir probabilidad de alarma en test
test_data$prob_alarma <- predict(modelo_logit, newdata = test_data, type = "response")

test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)  # redondea a 1 decimal






#============================================================
# PERCENTIL CONDICIONADO A ALARMAS REALES: 10
#============================================================

# Probabilidades solo de los casos con alarma real
prob_alarmas_reales <- test_data$prob_alarma[test_data$alarma_real == 1]

# Calcular percentil 10
percentil_cond <- quantile(prob_alarmas_reales, probs = 0.1)
percentil_cond




#============================================================
# GRÁFICO: Probabilidad de alarma (%) vs Temperatura interior predicha
#============================================================

# Convertir percentil condicional a %
percentil_cond_pct <- round(percentil_cond * 100, 1)

# Registrar Times New Roman (solo necesario en Windows)
windowsFonts(TimesNR = windowsFont("Times New Roman"))

# Crear el gráfico
ggplot(test_data, aes(x = Int_T_pred, y = prob_alarma_pct)) +
  geom_point(aes(color = factor(alarma_real)), alpha = 1, size = 2) +
  geom_hline(yintercept = percentil_cond_pct, linetype = "dashed", color = "gray40") +
  annotate("text",
           x = min(test_data$Int_T_pred),
           y = percentil_cond_pct,
           label = paste0("P10 = ", percentil_cond_pct, "%"),
           vjust = -0.8, hjust = 0, size = 4, family = "TimesNR") +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     name = "Alarm_real",
                     labels = c("No (0)", "Yes (1)")) +
  scale_x_continuous(limits = c(min(test_data$Int_T_pred), 32),
                     breaks = seq(floor(min(test_data$Int_T_pred)), 32, by = 1)) +
  labs(
    x = "Predicted Daily Mean Indoor Temperature (°C)",
    y = "Probability of alarm (%)"
  ) +
  theme_minimal(base_size = 12, base_family = "TimesNR") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    axis.title = element_text(size = 14, margin = margin(t = 10, r = 10)),
    axis.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13)
  )

#============================================================
# Valor de temperatura más cercano a la probabilidad de 40.2%
#============================================================

# Valor del percentil 10
umbral_pct10 <- 41.4

# Buscar la fila más cercana a ese valor de probabilidad
fila_cercana <- test_data %>%
  mutate(dif = abs(prob_alarma_pct - umbral_pct10)) %>%
  arrange(dif) %>%
  slice(1) %>%
  select(Int_T_pred, prob_alarma_pct, limiteadap, alarma_real)

fila_cercana





#============================================================
# DICOTOMIZAR PROBABILIDAD DE ALARMA SEGÚN PERCENTIL 10
#============================================================

# Crear la columna binaria según el percentil 10 (poner el valor % a mano)
test_data <- test_data %>%
  mutate(
    alarma_pred_P10 = ifelse(prob_alarma_pct >= 40.2, 1, 0)
  )


#============================================================
# MATRIZ DE CONFUSIÓN VISUAL (HEATMAP) - PERCENTIL 10
#============================================================

# Función auxiliar para generar matriz y gráfico con estilo personalizado
plot_conf_matrix <- function(data, pred_col, label) {
  
  # Asegurar fuente Times New Roman
  windowsFonts(TimesNR = windowsFont("Times New Roman"))
  
  # Crear tabla de contingencia forzando niveles 0 y 1
  conf_mat <- table(
    Real = factor(data$alarma_real, levels = c(0, 1)),
    Predicho = factor(data[[pred_col]], levels = c(0, 1))
  ) %>% as.data.frame()
  
  # Graficar heatmap con el estilo solicitado
  p <- ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Freq), size = 14, family = "TimesNR") +  # Números dentro de las cajas
    scale_fill_gradient(low = "white", high = "steelblue", name = "") +  # Sin "Freq" en leyenda
    labs(
     x = "Alarm_predicted",
      y = "Alarm_real"
    ) +
    theme_minimal(base_family = "TimesNR") +  # Times en todo el gráfico
    theme(
      axis.text = element_text(size = 18),        # Texto ejes
      axis.title = element_text(size = 20),       # Títulos ejes
      legend.text = element_text(size = 16),      # Texto leyenda
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),  # Título centrado
      panel.grid = element_blank(),               # Sin líneas de fondo
      axis.ticks = element_blank()                # Sin marcas de eje
    )
  
  print(p)
  
  return(conf_mat)
}

# Ejecutar para percentil 10
conf_P10 <- plot_conf_matrix(test_data, "alarma_pred_P10", "Percentil 10")


#============================================================
#MÉTRICAS DE EVALUACIÓN DEL MODELO DE ALARMAS
#============================================================
# Crear matriz de confusión (percentil 10)
conf_mat <- table(
  Real = test_data$alarma_real,
  Predicho = test_data$alarma_pred_P10
)

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

#============================================================

accuracy  <- (TP + TN) / total
recall    <- TP / (TP + FN)           # Sensibilidad o tasa de verdaderos positivos
specificity <- TN / (TN + FP)         # Especificidad o tasa de verdaderos negativos
precision <- TP / (TP + FP)           # Valor predictivo positivo
f1_score  <- 2 * (precision * recall) / (precision + recall)

# Mostrar resultados en porcentaje
resultados <- data.frame(
  Métrica = c("Accuracy", "Recall (Sensibilidad)", "Specificity", "Precision", "F1-score"),
  Valor = round(100 * c(accuracy, recall, specificity, precision, f1_score), 1)
)

print(resultados)





#============================================================
# Gráfico de FN-FP... segun intervalos de temperatura
#============================================================

# 1. Clasificar cada fila como TP, TN, FP, FN
test_data <- test_data %>%
  mutate(
    categoria = case_when(
      alarma_real == 1 & alarma_pred_P10 == 1 ~ "TP",
      alarma_real == 0 & alarma_pred_P10 == 0 ~ "TN",
      alarma_real == 0 & alarma_pred_P10 == 1 ~ "FP",
      alarma_real == 1 & alarma_pred_P10 == 0 ~ "FN"
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
colores <- c(
  "TP" = "#27AE60",  # verde más intenso
  "TN" = "#2ECC71",  # verde más suave
  "FP" = "#E74C3C",  # rojo más suave
  "FN" = "#C0392B"   # rojo más intenso
)

# 5. Gráfico de barras apiladas
# Registrar Times New Roman en Windows
windowsFonts(TimesNR = windowsFont("Times New Roman"))

# Asegurar que el eje X esté ordenado correctamente
barras_data$temp_bin <- factor(barras_data$temp_bin, levels = sort(unique(barras_data$temp_bin)))

# Crear el gráfico de barras apiladas con estilo uniforme
ggplot(barras_data, aes(x = temp_bin, y = n, fill = categoria)) +
  geom_bar(stat = "identity") +  # sin borde negro
  scale_fill_manual(values = colores, name = "") +
  labs(
    x = "Predicted Temperature (°C)",
    y = "Count",
      ) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5), expand = c(0, 0)) +
  theme_minimal(base_family = "TimesNR") +
  theme(
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
     )

#============================================================
# Valor de temperatura más cercano a la probabilidad de 40.2%
#============================================================

# Valor del percentil 10
umbral_pct10 <- 40.2

# Buscar la fila más cercana a ese valor de probabilidad
fila_cercana <- test_data %>%
  mutate(dif = abs(prob_alarma_pct - umbral_pct10)) %>%
  arrange(dif) %>%
  slice(1) %>%
  select(Int_T_pred, prob_alarma_pct, limiteadap, alarma_real)

fila_cercana
