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

install.packages("rsample")
library(rsample)


#1. Viv_sinnormativa---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_sinnormativa, prop = 0.7)  # 70% train, 30% test
train_data_1 <- training(split)
test_data_1 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_logit <- glm(alarma_real ~ Ext_T + Ext_RAD + 
                      Ext_T_1 + Ext_T_2 + Ext_T_3 +
                      Int_T_1 + Int_T_2 + Int_T_3,
                    data = train_data_1,
                    family = binomial)

#Predecir posibilidades
test_data_1$prob_alarma <- predict(modelo_logit, newdata = test_data_1, type = "response")

#Definir distintos umbrales
test_data_1$alarma_pred_05 <- ifelse(test_data_1$prob_alarma > 0.5, 1, 0)
#Si quieres ser más conservador para reducir falsos negativos (FN) (es decir, dar más alarmas aunque haya más falsos positivos), se baja el umbral:
test_data_1$alarma_pred_03 <- ifelse(test_data_1$prob_alarma > 0.3, 1, 0)



#2. Viv_ct79---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_ct79, prop = 0.7)  # 70% train, 30% test
train_data_2 <- training(split)
test_data_2 <- testing(split)

#Predecir posibilidades
test_data_1$prob_alarma <- predict(modelo_logit, newdata = test_data_1, type = "response")

#Convertir la probabilidad en %
test_data_1$prob_alarma_pct <- round(test_data_1$prob_alarma * 100, 1)

#Visualizar
head(test_data_1[, c("alarma_real", "prob_alarma", "prob_alarma_pct")])

library(ggplot2)

ggplot(test_data_1, aes(x = prob_alarma, fill = factor(alarma_real))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  labs(fill = "Alarma real", x = "Probabilidad predicha", y = "Frecuencia") +
  theme_minimal()





#3. Viv_cte2006---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2006, prop = 0.7)  # 70% train, 30% test
train_data_3 <- training(split)
test_data_3 <- testing(split)




#4. Viv_cte2019---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2019, prop = 0.7)  # 70% train, 30% test
train_data_4 <- training(split)
test_data_4 <- testing(split)










#Predicción======
#1Dividir dataset
set.seed(123)
split <- initial_split(Viv_sinnormativa, prop = 0.7)  # 70% train, 30% test
train_data_1 <- training(split)
test_data_1 <- testing(split)


#Modelo de regresión logística
modelo_logit <- glm(alarma_real ~ Ext_T + Ext_RAD + 
                      Ext_T_1 + Ext_T_2 + Ext_T_3 +
                      Int_T_1 + Int_T_2 + Int_T_3,
                    data = train_data_1,
                    family = binomial)

#Predecir posibilidades
test_data_1$prob_alarma <- predict(modelo_logit, newdata = test_data_1, type = "response")

#Definir distintos umbrales
test_data_1$alarma_pred_05 <- ifelse(test_data_1$prob_alarma > 0.5, 1, 0)
#Si quieres ser más conservador para reducir falsos negativos (FN) (es decir, dar más alarmas aunque haya más falsos positivos), se baja el umbral:
test_data_1$alarma_pred_03 <- ifelse(test_data_1$prob_alarma > 0.3, 1, 0)










#Evaluación resultados=======
#Matriz de confusión
library(caret)

confusionMatrix(factor(test_data_1$alarma_pred_05),
                factor(test_data_1$alarma_real))

confusionMatrix(factor(test_data_1$alarma_pred_03),
                factor(test_data_1$alarma_real))



#Elegir el mejor umbral====
umbrales <- seq(0.1, 0.9, 0.05) #Umbral mínimo y máximo y el intervalo

res <- sapply(umbrales, function(u){
  pred <- ifelse(test_data_1$prob_alarma > u, 1, 0)
  cm <- table(real = test_data_1$alarma_real, pred = pred)
  FN <- cm["1","0"] # falsos negativos
  FP <- cm["0","1"] # falsos positivos
  c(FN=FN, FP=FP)
})

res <- t(res)
colnames(res) <- c("FN", "FP")
res



#2. Predicción de la probabilidad de tener alarma por sobrecalentamiento

#Modelo de regresión logística
modelo_logit <- glm(alarma_real ~ Ext_T + Ext_RAD + 
                      Ext_T_1 + Ext_T_2 + Ext_T_3 +
                      Int_T_1 + Int_T_2 + Int_T_3,
                    data = train_data_1,
                    family = binomial)

#Predecir posibilidades
test_data_1$prob_alarma <- predict(modelo_logit, newdata = test_data_1, type = "response")

#Convertir la probabilidad en %
test_data_1$prob_alarma_pct <- round(test_data_1$prob_alarma * 100, 1)

#Visualizar
head(test_data_1[, c("alarma_real", "prob_alarma", "prob_alarma_pct")])

library(ggplot2)

ggplot(test_data_1, aes(x = prob_alarma, fill = factor(alarma_real))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  labs(fill = "Alarma real", x = "Probabilidad predicha", y = "Frecuencia") +
  theme_minimal()





#Ver los datos que tienen probabilidad predicha baja=======
library(dplyr)

# Filtrar filas con probabilidad < 0.3 y seleccionar columnas relevantes
casos_baja_prob <- test_data_1 %>%
  filter(prob_alarma < 0.3) %>%
  mutate(alarma_pred = ifelse(prob_alarma >= 0.5, 1, 0)) %>%  # predicción binaria con umbral 0.5
  select(alarma_real, alarma_pred,
         Int_T, Int_T_pred,
         prob_alarma, prob_alarma_pct, limiteadap)

# Mostrar todos los casos
casos_baja_prob

View(casos_baja_prob)













