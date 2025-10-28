#Importar base de datos: Vivtodas_verano_horario==========================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#1. Limpieza inicial======================================================================================

#Quitar grados hora
Vivtodas_verano_horario <- Vivtodas_verano_horario %>% select(-grados_hora)

#Quitar las entradas del año 2021 (nos quedamos solo con 2022, verano extremo)
#Se deja: Año 2022 (Junio, Julio y Agosto)
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  filter(year != 2021)

#Crear columna de fecha
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day))


#===============================================================================================================
#DESARROLLO: calcular si la temperatura de cada franja supera los limites determinados (fijos o adaptativos)
#===============================================================================================================

#1. Definir las franjas horarias, sus pesos y tipo de límite ===============================================
franjas <- tibble(
  hour = 0:23,
  franja = case_when(
    hour %in% 0:3   ~ "Noche_0_3",
    hour %in% 4:7   ~ "Noche_4_7",
    hour %in% 8:11  ~ "Manana",
    hour %in% 12:15 ~ "Mediodia",
    hour %in% 16:19 ~ "Tarde",
    hour %in% 20:23 ~ "Noche_20_23"
  ),
  peso = case_when(
    hour %in% 0:7   ~ 3,     # noches
    hour %in% 8:11  ~ 1,
    hour %in% 12:15 ~ 2,
    hour %in% 16:19 ~ 2,
    hour %in% 20:23 ~ 1.5
  ),
  limite = case_when(
    hour %in% 0:3   ~ "fijo",
    hour %in% 4:7   ~ "fijo",
    hour %in% 20:23 ~ "fijo",
    TRUE             ~ "adaptativo"  # resto de franjas
  )
)

#Asignar franja a cada observación horaria
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")


#2. Crear dataset con medias por franja y por vivienda===========================================================
T_franja_mean <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date, franja, limite) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),  # hace media de todas las variables numéricas
    .groups = "drop"
  )


#3. Crear limite adaptativo diario===========================================================

Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),     # media diaria exterior (por vivienda)
    .groups = "drop"
  ) %>%
  
  arrange(dwell_numb, date) %>%
  group_by(dwell_numb) %>%
  mutate(
    Ext_T_1 = lag(Ext_T_mean, 1),
    Ext_T_2 = lag(Ext_T_mean, 2),
    Ext_T_3 = lag(Ext_T_mean, 3),
    trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3),
    limiteadap = (0.33 * trm) + 21.8
  ) %>%
  ungroup() %>%
  filter(!is.na(limiteadap))   # Eliminamos las filas de los primeros días sin info suficiente

Vivtodas_diario <- Vivtodas_diario %>%
  mutate(limite = "adaptativo")



#4. Asignar limites a las franjas ===========================================================

T_franja_mean_limit <- T_franja_mean %>%
  left_join(
    Vivtodas_diario %>% select(dwell_numb, date, limiteadap),
    by = c("dwell_numb", "date")
  ) %>%
  mutate(
    limite_tipo = case_when(
      franja %in% c("Noche_0_3", "Noche_4_7", "Noche_20_23") ~ "fijo",
      TRUE ~ "adaptativo"
    ),
    limite_valor = case_when(
      limite_tipo == "fijo"       ~ 26,
      limite_tipo == "adaptativo" ~ limiteadap
    )
  ) %>%
  select(-limiteadap)  # eliminar columna temporal

  T_franja_mean_limit <- T_franja_mean_limit %>%
  select(-hour)
  
  T_franja_mean_limit <- T_franja_mean_limit %>% drop_na()


  
#5. Calcular si la temperatura supera el límite en cada franja ===========================================================
  T_franja_mean_limit <- T_franja_mean_limit %>%
    mutate(
      deltaT = limite_valor - Int_T,
      alarma = if_else(deltaT < 0, 1, 0)
    )

  T_franja_mean_limit %>%
    count(alarma) #Está bastante balanceado
  

#6. Base de datos agregada por día ===========================================================
#Asegurarnos que peso es numero
T_franja_mean_limit <- T_franja_mean_limit %>%
  mutate(peso = as.numeric(peso))

sobrecalentamiento_diario <- T_franja_mean_limit %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Int_T   = mean(Int_T, na.rm = TRUE),
    Int_RH  = mean(Int_RH, na.rm = TRUE),
    Ext_T   = mean(Ext_T, na.rm = TRUE),
    Ext_RH  = mean(Ext_RH, na.rm = TRUE),
    Ext_RAD = mean(Ext_RAD, na.rm = TRUE),
    alarma  = max(alarma, na.rm = TRUE),
    .groups = "drop"
  )


sobrecalentamiento_diario_1 <- T_franja_mean_limit %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Int_T_ponderada = sum(Int_T * peso, na.rm = TRUE) / sum(peso, na.rm = TRUE),
    .groups = "drop"
  )

sobrecalentamiento_diario_final <- left_join(
  sobrecalentamiento_diario,
  sobrecalentamiento_diario_1,
  by = c("dwell_numb", "date")
)

rm(sobrecalentamiento_diario, sobrecalentamiento_diario_1)

  

  
#7. Clasificar los tipos de sobrecalentamiento diario ===========================================================
alarma_tipo_diaria <- T_franja_mean_limit %>%
  group_by(dwell_numb, date) %>%
  summarise(
    alarma_tipo = case_when(
      max(alarma[limite_tipo == "fijo"], na.rm = TRUE) == 0 &
        max(alarma[limite_tipo == "adaptativo"], na.rm = TRUE) == 0 ~ 0,  # sin alarma
      max(alarma[limite_tipo == "fijo"], na.rm = TRUE) == 1 &
        max(alarma[limite_tipo == "adaptativo"], na.rm = TRUE) == 0 ~ 1,  # nocturno
      max(alarma[limite_tipo == "fijo"], na.rm = TRUE) == 0 &
        max(alarma[limite_tipo == "adaptativo"], na.rm = TRUE) == 1 ~ 2,  # diurno
      max(alarma[limite_tipo == "fijo"], na.rm = TRUE) == 1 &
        max(alarma[limite_tipo == "adaptativo"], na.rm = TRUE) == 1 ~ 3   # mixto
    ),
    .groups = "drop"
  )

# Revisión rápida de distribución
table(alarma_tipo_diaria$alarma_tipo)


#8. Incorporar el tipo a la dataset principal
sobrecalentamiento_diario_final <- sobrecalentamiento_diario_final %>%
  left_join(alarma_tipo_diaria, by = c("dwell_numb", "date"))
  




  
#===============================================================================================================
#GRÁFICOS EXPLORATORIOS
#===============================================================================================================

library(ggplot2)

ggplot(sobrecalentamiento_diario_final, aes(x = factor(dwell_numb), fill = factor(alarma_tipo))) +
  geom_bar(position = "stack") +
  scale_fill_manual(
    values = c("0" = "grey80", "1" = "blue", "2" = "orange", "3" = "red"),
    labels = c("0" = "Sin alarma", "1" = "Nocturno", "2" = "Diurno", "3" = "Ambos"),
    name = "Tipo de alarma"
  ) +
  labs(
    title = "Distribución de tipos de alarma por vivienda",
    x = "Vivienda",
    y = "Número de días"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Hay muy pocos días en los que la alarma se dispare solo por el sobrecalentamiento nocturno- tiene sentido: cuando la noche es cálida, normalmente el día lo ha sido tambien.  
  







#===============================================================================================================
#PREDICCIÓN
#===============================================================================================================
# Carga de librerías
install.packages("tidyverse")
library(tidyverse)
library(caret)
library(nnet)
library(pROC)

#1. crear lags=============================================================================================
# Crear las variables retardadas por vivienda
sobrecalentamiento_diario_final <- sobrecalentamiento_diario_final %>%
  arrange(dwell_numb, date) %>%    # Orden correcto
  group_by(dwell_numb) %>%
  mutate(
    across(
      c(Ext_T, Int_T_ponderada),
      list(
        lag1 = ~lag(., 1),
        lag2 = ~lag(., 2),
        lag3 = ~lag(., 3),
        lag4 = ~lag(., 4),
        lag5 = ~lag(., 5),
        lag6 = ~lag(., 6),
        lag7 = ~lag(., 7),
        lag8 = ~lag(., 8),
        lag9 = ~lag(., 9)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()
#Eliminar los dias con NA
sobrecalentamiento_diario_final <- sobrecalentamiento_diario_final %>%
  filter(!is.na(Ext_T_lag9), !is.na(Int_T_ponderada_lag9))


#2. Dividir dataset (respetando viviendas)=============================================================================================
# División del dataset por viviendas
train_viviendas <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_viviendas  <- c(2, 4, 8, 11)

train_data <- sobrecalentamiento_diario_final %>% 
  filter(dwell_numb %in% train_viviendas)

test_data <- sobrecalentamiento_diario_final %>% 
  filter(dwell_numb %in% test_viviendas)


#=================================================
#3. Modelos predictivos
#=================================================

#3.1. Modelo binario===================================================================================
modelo_binario <- glm(alarma ~ Int_T + Int_RH + Ext_T + Ext_RAD + Int_T_ponderada +
                    Ext_T_lag1 + Ext_T_lag2 + Ext_T_lag3 + Ext_T_lag4 + Ext_T_lag5 +
                    Ext_T_lag6 + Ext_T_lag7 + Ext_T_lag8 + Ext_T_lag9 +
                    Int_T_ponderada_lag1 + Int_T_ponderada_lag2 + Int_T_ponderada_lag3 +
                    Int_T_ponderada_lag4 + Int_T_ponderada_lag5 + Int_T_ponderada_lag6 +
                    Int_T_ponderada_lag7 + Int_T_ponderada_lag8 + Int_T_ponderada_lag9,
                  data = train_data, family = binomial)

summary(modelo_binario)

# Predicción sobre test
test_data$pred_prob <- predict(modelo_binario, newdata = test_data, type = "response")
test_data$alarma_pred <- ifelse(test_data$pred_prob > 0.5, 1, 0)  # umbral 0.5

# Matriz de confusión
confusionMatrix(as.factor(test_data$alarma_pred),
                as.factor(test_data$alarma),
                positive = "1")

# Crear la matriz de confusión tipo heatmap===================
cm <- confusionMatrix(as.factor(test_data$alarma_pred),
                      as.factor(test_data$alarma),
                      positive = "1")

# Convertir a dataframe para graficar
cm_table <- as.data.frame(cm$table)

# Crear el heatmap
ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(title = "Matriz de confusión – Modelo Binario (alarma sí/no)",
       x = "Predicción", y = "Valor real") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#3.2. Modelo multinomial================================================================================
library(nnet)

# Asegurar tipo factor
train_data$alarma_tipo <- as.factor(train_data$alarma_tipo)
test_data$alarma_tipo  <- as.factor(test_data$alarma_tipo)

modelo_multi <- multinom(alarma_tipo ~ Int_T + Int_RH + Ext_T + Ext_RAD + Int_T_ponderada +
                           Ext_T_lag1 + Ext_T_lag2 + Ext_T_lag3 + Ext_T_lag4 + Ext_T_lag5 +
                           Ext_T_lag6 + Ext_T_lag7 + Ext_T_lag8 + Ext_T_lag9 +
                           Int_T_ponderada_lag1 + Int_T_ponderada_lag2 + Int_T_ponderada_lag3 +
                           Int_T_ponderada_lag4 + Int_T_ponderada_lag5 + Int_T_ponderada_lag6 +
                           Int_T_ponderada_lag7 + Int_T_ponderada_lag8 + Int_T_ponderada_lag9,
                         data = train_data)

# Predicción de clases
test_data$alarma_tipo_pred <- predict(modelo_multi, newdata = test_data)

# Matriz de confusión==========================
table(Real = test_data$alarma_tipo, Predicho = test_data$alarma_tipo_pred)

# Crear la matriz de confusión como dataframe
cm_multi <- table(Real = test_data$alarma_tipo, Predicho = test_data$alarma_tipo_pred)
cm_df <- as.data.frame(cm_multi)

# Añadir columna indicando si la predicción es correcta
cm_df <- cm_df %>%
  mutate(Correcta = ifelse(Real == Predicho, "Sí", "No"))

# Crear el heatmap
ggplot(cm_df, aes(x = Predicho, y = Real, fill = Correcta, alpha = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Sí" = "green", "No" = "red")) +
  scale_alpha(range = c(0.4, 1)) +
  labs(title = "Matriz de confusión – Modelo Multinomial (tipo de alarma)",
       x = "Predicción", y = "Valor real") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#4. Evaluación de los modelos =============================================================================================





  









  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  