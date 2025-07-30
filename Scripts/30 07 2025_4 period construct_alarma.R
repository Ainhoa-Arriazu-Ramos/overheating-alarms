#IMPORTAR BASE DE DATOS Vivreales_Tdiario_20212022

#Renombrar
viv_verano_diario <- Vivreales_Tdiario_20212022

#CREAR ALARMA EN BASE A LIMITE ADAPTATIVO DIARIO EN 16798-1:2019 
#1. CREAR LA VARIABLE: Límite diario en base al confort adaptativo EN 16798-1:2019
library(dplyr)

viv_verano_diario <- viv_verano_diario %>%
  mutate(trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3))
viv_verano_diario <- viv_verano_diario %>%
  mutate(limiteadap = (0.33 * trm)+21.8)

#2. CREAR VARIABLE: Alarma
#si:
#trm > 30 o
#Int_T > limiteadap 
viv_verano_diario <- viv_verano_diario %>%
  mutate(
    alarma_real = if_else(trm > 30 | Int_T > limiteadap, 1, 0)
  )





#Dividir base segun periodos constructivos->Cuatro bases de datos distintas

Viv_sinnormativa <- viv_verano_diario %>%
  filter(dwell_numb %in% c(1, 2, 3, 4))

Viv_ct79 <- viv_verano_diario %>%
  filter(dwell_numb %in% c(5, 6))

Viv_cte2006 <- viv_verano_diario %>%
  filter(dwell_numb %in% c(7, 8, 9))

Viv_cte2019 <- viv_verano_diario %>%
  filter(dwell_numb %in% c(10, 11, 12))



#MODELO PREDICTIVO de ALARMA (1 o 0) ==============================================

#1. Viv_sinnormativa---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_sinnormativa, prop = 0.7)  # 70% train, 30% test
train_data_1 <- training(split)
test_data_1 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_1 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_1)

# Predecir Int_T sobre los datos test
test_data_1$Int_T_pred <- predict(modelo_rlm_1, newdata = test_data_1)

#Calcular alarma usando la fórmula, pero con las variables de test:
test_data_1$alarma_test <- ifelse(test_data_1$trm > 30 | test_data_1$Int_T_pred > test_data_1$limiteadap, 1, 0)

#Residuos
test_data_1$residuos_alarma_1 <- test_data_1$alarma_real - test_data_1$alarma_test

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data_1$alarma_real, test_data_1$alarma_test)

# Crear tabla de contingencia
conf_mat_1 <- table(Real = test_data_1$alarma_real, Predicho = test_data_1$alarma_test) %>% 
  as.data.frame()

# Graficar heatmap
ggplot(conf_mat_1, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Viv_sin normativa",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))






#2. Viv_ct79---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_ct79, prop = 0.7)  # 70% train, 30% test
train_data_2 <- training(split)
test_data_2 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_2 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_2)

# Predecir Int_T sobre los datos test
test_data_2$Int_T_pred <- predict(modelo_rlm_2, newdata = test_data_2)

#Calcular alarma usando la fórmula, pero con las variables de test:
test_data_2$alarma_test <- ifelse(test_data_2$trm > 30 | test_data_2$Int_T_pred > test_data_2$limiteadap, 1, 0)

#Residuos
test_data_2$residuos_alarma_2 <- test_data_2$alarma_real - test_data_2$alarma_test

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data_2$alarma_real, test_data_2$alarma_test)

# Crear tabla de contingencia
conf_mat_2 <- table(Real = test_data_2$alarma_real, Predicho = test_data_2$alarma_test) %>% 
  as.data.frame()

# Graficar heatmap
ggplot(conf_mat_2, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Viv_ct79",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))






#3. Viv_cte2006---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2006, prop = 0.7)  # 70% train, 30% test
train_data_3 <- training(split)
test_data_3 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_3 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_3)

# Predecir Int_T sobre los datos test
test_data_3$Int_T_pred <- predict(modelo_rlm_3, newdata = test_data_3)

#Calcular alarma usando la fórmula, pero con las variables de test:
test_data_3$alarma_test <- ifelse(test_data_3$trm > 30 | test_data_3$Int_T_pred > test_data_3$limiteadap, 1, 0)

#Residuos
test_data_3$residuos_alarma_3 <- test_data_3$alarma_real - test_data_3$alarma_test

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data_3$alarma_real, test_data_3$alarma_test)

# Crear tabla de contingencia
conf_mat_3 <- table(Real = test_data_3$alarma_real, Predicho = test_data_3$alarma_test) %>% 
  as.data.frame()

# Graficar heatmap
ggplot(conf_mat_3, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Viv_cte2006",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))



#4. Viv_cte2019---------------------------------------------------------------------
set.seed(123)
split <- initial_split(Viv_cte2019, prop = 0.7)  # 70% train, 30% test
train_data_4 <- training(split)
test_data_4 <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm_4 <- lm(Int_T ~ Ext_T + Ext_RAD + 
                     Ext_T_1 + Ext_T_2 + Ext_T_3 
                   + Int_T_1 + Int_T_2 + Int_T_3, 
                   data = train_data_4)

# Predecir Int_T sobre los datos test
test_data_4$Int_T_pred <- predict(modelo_rlm_4, newdata = test_data_4)

#Calcular alarma usando la fórmula, pero con las variables de test:
test_data_4$alarma_test <- ifelse(test_data_4$trm > 30 | test_data_4$Int_T_pred > test_data_4$limiteadap, 1, 0)

#Residuos
test_data_4$residuos_alarma_4 <- test_data_4$alarma_real - test_data_4$alarma_test

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data_4$alarma_real, test_data_4$alarma_test)

# Crear tabla de contingencia
conf_mat_4 <- table(Real = test_data_4$alarma_real, Predicho = test_data_4$alarma_test) %>% 
  as.data.frame()

# Graficar heatmap
ggplot(conf_mat_4, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Viv_cte2019",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))



#REVISIÓN DE LOS FALSOS NEGATIVOS

#1--------------------------------------------------------------------
falsos_negativos_1 <- test_data_1 %>%
  filter(alarma_real == 1 & alarma_test == 0)

# Ver cuántos son:
n_falsos_negativos <- nrow(falsos_negativos_1)
print(paste("Número de falsos negativos:", n_falsos_negativos))

# Mostrar las filas con falsos negativos:
View(falsos_negativos_1)




#2--------------------------------------------------------------------
falsos_negativos_2 <- test_data_2 %>%
  filter(alarma_real == 1 & alarma_test == 0)

# Ver cuántos son:
n_falsos_negativos <- nrow(falsos_negativos_2)
print(paste("Número de falsos negativos:", n_falsos_negativos))

# Mostrar las filas con falsos negativos:
View(falsos_negativos_2)




#3--------------------------------------------------------------------
falsos_negativos_3 <- test_data_3 %>%
  filter(alarma_real == 1 & alarma_test == 0)

# Ver cuántos son:
n_falsos_negativos <- nrow(falsos_negativos_3)
print(paste("Número de falsos negativos:", n_falsos_negativos))

# Mostrar las filas con falsos negativos:
View(falsos_negativos_3)


#4--------------------------------------------------------------------
falsos_negativos_4 <- test_data_4 %>%
  filter(alarma_real == 1 & alarma_test == 0)

# Ver cuántos son:
n_falsos_negativos <- nrow(falsos_negativos_4)
print(paste("Número de falsos negativos:", n_falsos_negativos))

# Mostrar las filas con falsos negativos:
View(falsos_negativos_4)

