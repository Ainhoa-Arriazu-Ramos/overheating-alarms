#Importar la base de datos: Vivtodas_diario_media.xlsx

#Cargar bibliotecas
library(dplyr)
library(lubridate)
install.packages("pROC")
library(pROC)
install.packages("rsample")
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

#MÉTODO 2: PROBABILIDAD DE ALARMA====================================================================================================
set.seed(123)
split <- initial_split(Vivtodas_diario_media, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

# Paso 1: Modelo RLM para predecir temperatura interior
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 
                 + Int_T_1 + Int_T_2 + Int_T_3, 
                 data = train_data)

# Predecir temperatura interior en train y test
train_data$Int_T_pred <- predict(modelo_rlm, newdata = train_data)
test_data$Int_T_pred  <- predict(modelo_rlm, newdata = test_data)

#Ajustar modelo logístico que prediga alarma en función de la temperatura predicha y el límite adaptativo
modelo_logit <- glm(alarma_real ~ Int_T_pred + limiteadap, 
                    data = train_data, family = binomial)

summary(modelo_logit)

#Predecir probabilidad de alarma en test
test_data$prob_alarma <- predict(modelo_logit, newdata = test_data, type = "response")

#Crear columna con probabilidad en porcentaje
test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)  # redondea a 1 decimal







#Gráfico: alarmas y probabilidad de tenerlas =======================================

# Crear columna de borde
test_data$borde <- ifelse(test_data$alarma_real == 1, 1.5, 0)

# Gráfico
ggplot(test_data, aes(x = Int_T, y = Int_T_pred, fill = prob_alarma_cat)) +
  geom_point(aes(stroke = borde), size = 2.5, shape = 21, color = "black", alpha = 0.8) +
  scale_fill_brewer(palette = "YlOrRd", name = "Probabilidad de alarma") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Temperatura real vs predicha",
       x = "Temperatura interior real (ºC)",
       y = "Temperatura interior predicha (ºC)") +
  theme_minimal(base_size = 14)





#A partir de qué temperatura predicha hay un 90% de alarmas reales?============

# Filtrar solo las observaciones con alarma real
alarmas_real <- test_data %>% filter(alarma_real == 1)

# Calcular la temperatura predicha que deja el 90% de alarmas reales por encima
# Es decir, el percentil 10 de Int_T_pred en alarmas reales
temp_pred_umbral90 <- quantile(alarmas_real$Int_T_pred, probs = 0.1)

temp_pred_umbral90

#Añadir linea horizontal al gráfico
ggplot(test_data, aes(x = Int_T, y = Int_T_pred, fill = prob_alarma_cat)) +
  geom_point(aes(stroke = borde), size = 2.5, shape = 21, color = "black", alpha = 0.8) +
    # Línea roja horizontal
  geom_hline(yintercept = temp_pred_umbral90, color = "red", linetype = "dashed", size = 1.2) +
    # Texto sobre la línea indicando el valor
  annotate("text", 
           x = max(test_data$Int_T) * 0.95,  # posición horizontal (95% del eje x)
           y = temp_pred_umbral90 + 0.3,     # posición vertical ligeramente encima de la línea
           label = paste0("Temp_pred = ", round(temp_pred_umbral90, 2), " ºC"),
           color = "red",
           size = 3,
           hjust = 1) +
    scale_fill_brewer(palette = "YlOrRd", name = "Probabilidad de alarma") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Temperatura real vs predicha (línea roja: 90% alarmas reales)",
       x = "Temperatura interior real (ºC)",
       y = "Temperatura interior predicha (ºC)") +
  theme_minimal(base_size = 14)

#Tabla de contingencia usando este umbral de temperatura predicha para ver cuántos TP, FP, TN y FN hay usando esta regla

#Crear columna de predicción de alarma según el umbral de temperatura predicha
test_data <- test_data %>%
  mutate(alarma_pred_temp90 = ifelse(Int_T_pred >= temp_pred_umbral90, 1, 0))

#Crear tabla de contingencia 2x2
conf_mat_temp90 <- table(
  Real = test_data$alarma_real,
  Predicho = test_data$alarma_pred_temp90
)

conf_mat_temp90

# Convertir la tabla de contingencia a data.frame para ggplot
conf_mat_temp90_df <- as.data.frame(conf_mat_temp90)

# Heatmap
ggplot(conf_mat_temp90_df, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +  # tamaño del texto dentro de las celdas
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Tabla de contingencia - umbral 90% alarmas reales",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))



#sociar el umbral de temperatura predicha (temp_pred_umbral90) con la probabilidad de alarma predicha (prob_alarma), para determinar a partir de qué % conviene activar la alarma.

#Filtrar las observaciones que están por encima del umbral de temperatura predicha
# Observaciones con temperatura predicha >= temp_pred_umbral90
obs_umbral <- test_data %>% 
  filter(Int_T_pred >= temp_pred_umbral90) %>%
  select(Int_T_pred, alarma_real, prob_alarma, prob_alarma_pct)
#Revisar las probabilidades de alarma correspondientes
summary(obs_umbral$prob_alarma_pct)

#Gráfico COMBINANDO TODO============
# Umbral de probabilidad (percentil 10 de las observaciones por encima del umbral de temp)
# Crear categorías de probabilidad cada 20%
test_data <- test_data %>%
  mutate(prob_alarma_cat = cut(prob_alarma_pct,
                               breaks = seq(0, 100, by = 20),
                               include.lowest = TRUE,
                               labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")),
         borde = ifelse(alarma_real == 1, 1.5, 0))  # borde negro solo para alarmas

# Gráfico combinado
ggplot(test_data, aes(x = prob_alarma_pct, y = Int_T_pred, fill = prob_alarma_cat)) +
  geom_point(aes(stroke = borde), size = 3, shape = 21, color = "black", alpha = 0.8) +
  
  # Línea horizontal: umbral temperatura predicha
  geom_hline(yintercept = temp_pred_umbral90, color = "red", linetype = "dashed", size = 1.2) +
  annotate("text",
           x = max(test_data$prob_alarma_pct) * 0.95,
           y = temp_pred_umbral90 + 0.3,
           label = paste0("Temp_pred = ", round(temp_pred_umbral90,2), " ºC"),
           color = "red",
           size = 3,
           hjust = 1) +
  
  # Línea vertical: umbral probabilidad
  geom_vline(xintercept = prob_umbral90, color = "blue", linetype = "dashed", size = 1.2) +
  annotate("text",
           x = prob_umbral90 + 1,
           y = min(test_data$Int_T_pred) + 0.3,
           label = paste0("Prob_alarma = ", round(prob_umbral90,1), " %"),
           color = "blue",
           size = 3,
           angle = 90,
           vjust = -0.5) +
  
  scale_fill_brewer(palette = "YlOrRd", name = "Probabilidad de alarma") +
  labs(title = "Temperatura predicha vs Probabilidad de alarma",
       x = "Probabilidad de alarma (%)",
       y = "Temperatura predicha (ºC)") +
  theme_minimal(base_size = 14)































#EVALUACIÓN==========================================================================================
#Evaluar qué tan bien tu modelo probabilístico detecta las alarmas reales, y cómo la probabilidad predicha se traduce en aciertos o fallos.

#ROC
library(pROC)
#Crear objeto ROC
roc_obj <- roc(test_data$alarma_real, test_data$prob_alarma)
# Gráfico
plot(roc_obj, col = "blue", main = "Curva ROC - Probabilidad de alarma")
auc(roc_obj)
# Encontrar el umbral óptimo usando el índice de Youden
umbral_optimo <- coords(roc_obj, x = "best", best.method = "youden", ret = c("threshold","sensitivity","specificity"))
umbral_optimo
umbral_val <- as.numeric(umbral_optimo["threshold"])
umbral_val

#Asociar al valor de temperatura predicha
# Filtrar observaciones con probabilidad mayor o igual al umbral
test_data %>%
  filter(prob_alarma >= umbral_val) %>%
  select(Int_T_pred, Int_T, alarma_real, prob_alarma)



#Crear la predicción de alarma según el umbral óptimo
# Crear columna con predicción de alarma según umbral
test_data <- test_data %>%
  mutate(alarma_pred_opt = ifelse(prob_alarma >= umbral_val, 1, 0))


# Tabla de contingencia
conf_mat <- table(
  Real = test_data$alarma_real,
  Predicho = test_data$alarma_pred_opt
)

conf_mat