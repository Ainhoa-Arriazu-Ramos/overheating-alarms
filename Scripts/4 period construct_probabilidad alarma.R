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


#====================================================================================================


#Dividir base segun periodos constructivos->Cuatro bases de datos distintas

Viv_sinnormativa <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(1, 2, 3, 4))

Viv_ct79 <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(5, 6))

Viv_cte2006 <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(7, 8, 9))

Viv_cte2019 <- Vivtodas_diario_media %>%
  filter(dwell_numb %in% c(10, 11, 12))



#MODELO PREDICTIVO de ALARMA (1 o 0) ==============================================


#1. Viv_sinnormativa---------------------------------------------------------------------
# ================================================


# 1. Modelo logístico para probabilidad de alarma
set.seed(123)
split <- initial_split(Viv_sinnormativa, prop = 0.7)
train_data_1 <- training(split)
test_data_1 <- testing(split)

modelo_logit <- glm(alarma_real ~ Ext_T + Ext_RAD +
                      Ext_T_1 + Ext_T_2 + Ext_T_3 +
                      Int_T_1 + Int_T_2 + Int_T_3,
                    data = train_data_1,
                    family = binomial)

# Predicciones de probabilidad
test_data_1$pred_prob <- predict(modelo_logit, newdata = test_data_1, type = "response")

# Calcular umbral de probabilidad para 90% TPs
threshold_prob_90 <- test_data_1 %>%
  filter(alarma_real == 1) %>%
  summarise(corte = quantile(pred_prob, probs = 0.1)) %>%
  pull(corte)

threshold_prob_90

# Graficar nube de puntos: temperatura real vs probabilidad predicha
library(ggplot2)
ggplot(test_data_1, aes(x = Int_T, y = pred_prob)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = threshold_prob_90, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Temperatura real (°C)",
       y = "Probabilidad predicha de alarma",
       title = "Probabilidad de alarma vs Temperatura real (90% TPs)") +
  theme_minimal()

#=============================================
# 2. Modelo lineal para temperatura predicha
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD +
                   Ext_T_1 + Ext_T_2 + Ext_T_3 +
                   Int_T_1 + Int_T_2 + Int_T_3,
                 data = train_data_1)

# Predicciones de temperatura
test_data_1$pred_temp <- predict(modelo_rlm, newdata = test_data_1)

# Umbral de temperatura predicha para 90% TPs
threshold_temp_90 <- test_data_1 %>%
  filter(alarma_real == 1) %>%
  summarise(corte = quantile(pred_temp, probs = 0.1)) %>%
  pull(corte)

threshold_temp_90

# Graficar nube de puntos: real vs predicha
ggplot(test_data_1, aes(x = Int_T, y = pred_temp)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = threshold_temp_90, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Temperatura real (°C)",
       y = "Temperatura predicha (°C)",
       title = "Temperatura real vs predicha (90% TPs)") +
  theme_minimal()




























#2. Viv_ct79---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_ct79, prop = 0.7)  # 70% train, 30% test
train_data_2 <- training(split)
test_data_2 <- testing(split)

#Predecir posibilidades
test_data_2$prob_alarma <- predict(modelo_logit, newdata = test_data_2, type = "response")





#3. Viv_cte2006---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2006, prop = 0.7)  # 70% train, 30% test
train_data_3 <- training(split)
test_data_3 <- testing(split)

#Predecir posibilidades
test_data_3$prob_alarma <- predict(modelo_logit, newdata = test_data_3, type = "response")





#4. Viv_cte2019---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2019, prop = 0.7)  # 70% train, 30% test
train_data_4 <- training(split)
test_data_4 <- testing(split)


#Predecir posibilidades
test_data_4$prob_alarma <- predict(modelo_logit, newdata = test_data_4, type = "response")


