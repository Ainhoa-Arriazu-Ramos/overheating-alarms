#Importar base de datos: Vivtodas_verano_horario==========================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(caret)


# Crear variable fecha
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
    alarma_real = if_else(deltaT < 0, 1, 0)
  )

T_franja_mean_limit %>%
  count(alarma_real) #Está bastante balanceado



#========================================
#Predecir temperatura de cada franja
#========================================








#==========================================
#MODELO PREDICTIVO
#==========================================
#Dividir dataset
# División del dataset por viviendas
train_viviendas <- c(1, 3, 5, 6, 7, 9, 10, 12, 13)
test_viviendas  <- c(2, 4, 8, 11)

train_franja <- T_franja_lags %>% filter(dwell_numb %in% train_viviendas)
test_franja  <- T_franja_lags %>% filter(dwell_numb %in% test_viviendas)


#Modelo binario=====================
modelo_binario_franja <- glm(
  alarma ~ Int_T + Ext_T + Ext_RAD +
    Ext_T_lag1 + Ext_T_lag2 + Ext_T_lag3 + Ext_T_lag4 + Ext_T_lag5 +
    Ext_T_lag6 + Ext_T_lag7 + Ext_T_lag8 + Ext_T_lag9 +
    Int_T_lag1 + Int_T_lag2 + Int_T_lag3 + Int_T_lag4 + Int_T_lag5 +
    Int_T_lag6 + Int_T_lag7 + Int_T_lag8 + Int_T_lag9,
  data = train_franja,
  family = binomial
)

summary(modelo_binario_franja)

#Predecir la probabilidad de alarma y tranformarla en resultado binario
test_franja$pred_prob <- predict(modelo_binario_franja,
                                 newdata = test_franja,
                                 type = "response")

test_franja$alarma_pred <- if_else(test_franja$pred_prob > 0.5, 1, 0)


#Matriz de confusion
confusionMatrix(as.factor(test_franja$alarma_pred),
                as.factor(test_franja$alarma),
                positive = "1")



cm <- confusionMatrix(as.factor(test_franja$alarma_pred),
                      as.factor(test_franja$alarma),
                      positive = "1")

cm_table <- as.data.frame(cm$table)

ggplot(cm_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Matriz de confusión – Modelo Binario (alarma sí/no) por FRANJA",
       x = "Predicción", y = "Valor real") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

