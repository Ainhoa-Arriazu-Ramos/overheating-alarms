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



#====================================================================================================
#MÉTODO 2: PROBABILIDAD DE ALARMA
#====================================================================================================


#ANALISIS CON TODAS LAS VIVIENDAS JUNTAS==========================================================================

# MODELO RLM: predecir temperatura interior
set.seed(123)
split <- initial_split(Vivtodas_diario_media, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 
                 + Int_T_1 + Int_T_2 + Int_T_3, 
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


train_data$Int_T_pred <- predict(modelo_rlm, newdata = train_data)
test_data$Int_T_pred  <- predict(modelo_rlm, newdata = test_data)


# MODELO LOGÍSTICO: predecir alarma en función de Int_T_pred y limiteadap
modelo_logit <- glm(alarma_real ~ Int_T_pred + limiteadap, 
                    data = train_data, family = binomial)

summary(modelo_logit)

# Predecir probabilidad de alarma en test
test_data$prob_alarma <- predict(modelo_logit, newdata = test_data, type = "response")

test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)  # redondea a 1 decimal

# Categorizar probabilidad de alarma cada 20%
test_data <- test_data %>%
  mutate(prob_alarma_cat = cut(prob_alarma_pct,
                               breaks = seq(0, 100, 20),
                               include.lowest = TRUE,
                               labels = c("0–20%", "21–40%", "41–60%", "61–80%", "81–100%")),
         borde = ifelse(alarma_real == 1, 1.5, 0))



# ===================================================================
# CALCULAR UMBRALES OPTIMOS (percentil 10 de alarmas reales)
alarmas_real <- test_data %>% filter(alarma_real == 1)

# Umbral temperatura predicha
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

# Umbral probabilidad de alarma
prob_umbral90 <- quantile(alarmas_real$prob_alarma, probs = 0.1)
prob_umbral90

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
  
# Línea vertical: umbral probabilidad (convertido a porcentaje)
geom_vline(xintercept = prob_umbral90 * 100, color = "blue", linetype = "dashed", size = 1.2) +
annotate("text",
           x = prob_umbral90 * 100 + 1,
           y = min(test_data$Int_T_pred) + 0.3,
           label = paste0("Prob_alarma = ", round(prob_umbral90*100,1), " %"),
           color = "blue",
           size = 3,
           angle = 90,
           vjust = -0.5) +
  
  scale_fill_brewer(palette = "YlOrRd", name = "Probabilidad de alarma") +
  labs(title = "Temperatura predicha vs Probabilidad de alarma",
       x = "Probabilidad de alarma (%)",
       y = "Temperatura predicha (ºC)") +
  theme_minimal(base_size = 14)


#TABLA DE CONTINGENCIA===============

# CREAR COLUMNA DE ALARMA PREDICHA CON UMBRALES OPTIMOS (Alarma si se supera el Umbral de temp O el Umbral %)
test_data <- test_data %>%
  mutate(
    alarma_pred_m2 = ifelse(Int_T_pred >= temp_pred_umbral90 | prob_alarma >= prob_umbral90, 1, 0)
  )

# TABLA DE CONFUSIÓN
conf_m2 <- table(Predicho = test_data$alarma_pred_m2,
                 Real = test_data$alarma_real)
print(conf_m2)

# CONVERTIR A DATAFRAME PARA GRAFICO
conf_m2_df <- as.data.frame(conf_m2)
colnames(conf_m2_df) <- c("Predicho", "Real", "Freq")

# GRÁFICO MATRIZ DE CONFUSIÓN
ggplot(conf_m2_df, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Todas viviendas",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"))
