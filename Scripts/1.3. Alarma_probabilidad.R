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
library(tidyr)

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


# MODELO RLM: predecir temperatura interior======================
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


# MODELO LOGÍSTICO: predecir alarma en función de Int_T_pred y limiteadap======================
modelo_logit <- glm(alarma_real ~ Int_T_pred + limiteadap, 
                    data = train_data, family = binomial)

summary(modelo_logit)

# Predecir probabilidad de alarma en test
test_data$prob_alarma <- predict(modelo_logit, newdata = test_data, type = "response")

test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)  # redondea a 1 decimal









#============================================================
# PERCENTILES CONDICIONADOS A ALARMAS REALES: 10,30,50,70
#============================================================

# Probabilidades solo de los casos con alarma real
prob_alarmas_reales <- test_data$prob_alarma[test_data$alarma_real == 1]

# Calcular percentiles 10, 30, 50, 70
percentiles_cond <- quantile(prob_alarmas_reales, probs = c(0.1, 0.3, 0.5, 0.7))
percentiles_cond



# GRÁFICO: Probabilidad de alarma (%) vs Temperatura interior predicha ============

# Convertir percentiles condicionales a %
percentiles_cond_pct <- round(percentiles_cond * 100, 1)

# Crear el gráfico
ggplot(test_data, aes(x = Int_T_pred, y = prob_alarma_pct)) +
  geom_point(aes(color = factor(alarma_real)), alpha = 0.6, size = 2) +
  geom_hline(yintercept = percentiles_cond_pct[1], linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = percentiles_cond_pct[2], linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = percentiles_cond_pct[3], linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = percentiles_cond_pct[4], linetype = "dashed", color = "gray40") +
  annotate("text", x = min(test_data$Int_T_pred), y = percentiles_cond_pct[1], 
           label = paste0("P10 = ", percentiles_cond_pct[1], "%"), vjust = -0.8, hjust = 0, size = 3.5) +
  annotate("text", x = min(test_data$Int_T_pred), y = percentiles_cond_pct[2], 
           label = paste0("P30 = ", percentiles_cond_pct[2], "%"), vjust = -0.8, hjust = 0, size = 3.5) +
  annotate("text", x = min(test_data$Int_T_pred), y = percentiles_cond_pct[3], 
           label = paste0("P50 = ", percentiles_cond_pct[3], "%"), vjust = -0.8, hjust = 0, size = 3.5) +
  annotate("text", x = min(test_data$Int_T_pred), y = percentiles_cond_pct[4], 
           label = paste0("P70 = ", percentiles_cond_pct[4], "%"), vjust = -0.8, hjust = 0, size = 3.5) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "firebrick"),
                     name = "Alarma real",
                     labels = c("No", "Sí")) +
  labs(
    x = "Temperatura interior predicha (°C)",
    y = "Probabilidad de alarma (%)",
    title = "Relación entre la temperatura interior predicha y la probabilidad de alarma",
    subtitle = "Líneas punteadas muestran umbrales de percentiles condicionados a alarmas reales"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )





#============================================================
# DICOTOMIZAR PROBABILIDAD DE ALARMA SEGÚN PERCENTILES
#============================================================

# Crear las columnas binarias según cada percentil (poner los % a mano)
test_data <- test_data %>%
  mutate(
    alarma_pred_P10 = ifelse(prob_alarma_pct >= 40.2, 1, 0),
    alarma_pred_P30 = ifelse(prob_alarma_pct >= 73, 1, 0),
    alarma_pred_P50 = ifelse(prob_alarma_pct >= 90.8, 1, 0),
    alarma_pred_P70 = ifelse(prob_alarma_pct >= 97.6, 1, 0)
  )


#============================================================
# MATRICES DE CONFUSIÓN VISUALES (HEATMAP) PARA CADA PERCENTIL
#============================================================

# Función auxiliar para generar matriz y gráfico robusta
plot_conf_matrix <- function(data, pred_col, label) {
  
  # Crear tabla de contingencia forzando niveles 0 y 1
  conf_mat <- table(
    Real = factor(data$alarma_real, levels = c(0,1)),
    Predicho = factor(data[[pred_col]], levels = c(0,1))
  ) %>% as.data.frame()
  
  # Graficar heatmap
  p <- ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Freq), size = 8) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = paste("Matriz de confusión - Umbral", label),
         x = "Predicción",
         y = "Valor real") +
    theme_minimal() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold"))
  
  print(p)
  
  return(conf_mat)
}

# Generar matrices y gráficos
conf_P10 <- plot_conf_matrix(test_data, "alarma_pred_P10", "Percentil 10")
conf_P30 <- plot_conf_matrix(test_data, "alarma_pred_P30", "Percentil 30")
conf_P50 <- plot_conf_matrix(test_data, "alarma_pred_P50", "Percentil 50")
conf_P70 <- plot_conf_matrix(test_data, "alarma_pred_P70", "Percentil 70")





#Se elige el Percentil 10 




#============================================================
#Gráfico de FN-FP... segun intervalos de temperatura
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
    title = "Distribución de TP, TN, FP y FN por temperatura predicha (Percentil 10)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(face = "bold")
  )
