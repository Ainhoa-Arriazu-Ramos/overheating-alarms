#Importar base de datos: Vivtodas_verano_horario==========================================================
install.packages("nnet")


#Cargar librerías
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(caret)
library(nnet)

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








#==================================================================.==================================================================








#=========================================================
#MODELO ARX: PREDECIR TEMPERATURA DIARIA DE CADA FRANJA
#=========================================================

#Preparar dataset por franja con lags (12 lags)==========================================================
# Para el ARX usaremos la variable 'Int_T' media por franja renombrada a 'Int_T_franja'
Vivtodas_franja <- T_franja_mean_limit %>%
  rename(Int_T_franja = Int_T) %>%
  arrange(dwell_numb, franja, date)

# Crear lags por franja (12 lags)
Vivtodas_franja_lags <- Vivtodas_franja %>%
  group_by(dwell_numb, franja) %>%
  arrange(date) %>%
  mutate(
    Int_T_franja_lag1  = lag(Int_T_franja, 1),
    Int_T_franja_lag2  = lag(Int_T_franja, 2),
    Int_T_franja_lag3  = lag(Int_T_franja, 3),
    Int_T_franja_lag4  = lag(Int_T_franja, 4),
    Int_T_franja_lag5  = lag(Int_T_franja, 5),
    Int_T_franja_lag6  = lag(Int_T_franja, 6),
    Int_T_franja_lag7  = lag(Int_T_franja, 7),
    Int_T_franja_lag8  = lag(Int_T_franja, 8),
    Int_T_franja_lag9  = lag(Int_T_franja, 9),
    Int_T_franja_lag10 = lag(Int_T_franja, 10),
    Int_T_franja_lag11 = lag(Int_T_franja, 11),
    Int_T_franja_lag12 = lag(Int_T_franja, 12)
  ) %>%
  ungroup()

# Variables exteriores diarias y sus lags (para ARX)
Vivtodas_diario_ext <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dwell_numb, date) %>%
  group_by(dwell_numb) %>%
  mutate(
    Ext_T_mean_lag1 = lag(Ext_T_mean, 1),
    Ext_T_mean_lag2 = lag(Ext_T_mean, 2)
  ) %>%
  ungroup()

# Unión final: franja + variables externas (por date)
Vivtodas_final <- Vivtodas_franja_lags %>%
  left_join(Vivtodas_diario_ext, by = c("dwell_numb", "date"))


# Quitar filas con entradas NA
Vivtodas_final <- na.omit(Vivtodas_final)





#División del dataset===========================================================
# División del dataset por viviendas
train_viviendas <- c(1,3,5,6,7,9,10,12,13)
test_viviendas  <- c(2,4,8,11)

train_data <- Vivtodas_final %>% filter(dwell_numb %in% train_viviendas)
test_data  <- Vivtodas_final %>% filter(dwell_numb %in% test_viviendas)



# MODELO ARX (lm) 
modelo_franja <- lm(
  Int_T_franja ~
    Int_T_franja_lag1 + Int_T_franja_lag2 + Int_T_franja_lag3 + Int_T_franja_lag4 +
    Int_T_franja_lag5 + Int_T_franja_lag6 + Int_T_franja_lag7 + Int_T_franja_lag8 +
    Int_T_franja_lag9 + Int_T_franja_lag10 + Int_T_franja_lag11 + Int_T_franja_lag12 +
    Ext_T_mean + Ext_T_mean_lag1 + Ext_T_mean_lag2,
  data = train_data
)
summary(modelo_franja)

# PREDICCIÓN ARX
test_data <- test_data %>%
  mutate(Int_T_pred = predict(modelo_franja, newdata = test_data))

train_data <- train_data %>%
  mutate(Int_T_pred = predict(modelo_franja, newdata = train_data))





# PROCESADO PARA TEST (FRANJA → DÍA) ===================================================================

franjas_pred_test <- test_data %>%
  select(dwell_numb, date, franja, Int_T_pred) %>%
  left_join(
    T_franja_mean_limit %>% select(dwell_numb, date, franja, limite_valor, alarma_real),
    by = c("dwell_numb", "date", "franja")
  ) %>%
  drop_na(Int_T_pred, limite_valor)

franjas_pred_test <- franjas_pred_test %>%
  mutate(
    alarma_pred_franja = if_else(Int_T_pred > limite_valor, 1, 0),
    periodo = case_when(
      franja %in% c("Noche_0_3", "Noche_4_7", "Noche_20_23") ~ "noche",
      TRUE ~ "dia"
    )
  )

# Agregados para TEST
alarma_diaria_real <- franjas_pred_test %>%
  group_by(dwell_numb, date) %>%
  summarise(alarma_real_dia = as.integer(max(alarma_real, na.rm = TRUE)), .groups = "drop")

alarma_diaria_pred <- franjas_pred_test %>%
  group_by(dwell_numb, date) %>%
  summarise(alarma_pred_dia = as.integer(max(alarma_pred_franja, na.rm = TRUE)), .groups = "drop")

daily_test <- franjas_pred_test %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Int_T_pred_max = max(Int_T_pred, na.rm = TRUE),
    limite_min = min(limite_valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(alarma_diaria_real, by = c("dwell_numb", "date")) %>%
  left_join(alarma_diaria_pred, by = c("dwell_numb", "date"))




#  PROCESADO PARA TRAIN (FRANJA → DÍA) ===================================================================

franjas_pred_train <- train_data %>%
  select(dwell_numb, date, franja, Int_T_pred) %>%
  left_join(
    T_franja_mean_limit %>% select(dwell_numb, date, franja, limite_valor, alarma_real),
    by = c("dwell_numb", "date", "franja")
  ) %>%
  drop_na(Int_T_pred, limite_valor)

franjas_pred_train <- franjas_pred_train %>%
  mutate(
    alarma_pred_franja = if_else(Int_T_pred > limite_valor, 1, 0),
    periodo = case_when(
      franja %in% c("Noche_0_3", "Noche_4_7", "Noche_20_23") ~ "noche",
      TRUE ~ "dia"
    )
  )

# Agregados TRAIN
alarma_diaria_real_train <- franjas_pred_train %>%
  group_by(dwell_numb, date) %>%
  summarise(alarma_real_dia = as.integer(max(alarma_real, na.rm = TRUE)), .groups = "drop")

alarma_diaria_pred_train <- franjas_pred_train %>%
  group_by(dwell_numb, date) %>%
  summarise(alarma_pred_dia = as.integer(max(alarma_pred_franja, na.rm = TRUE)), .groups = "drop")

daily_train <- franjas_pred_train %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Int_T_pred_max = max(Int_T_pred, na.rm = TRUE),
    limite_min = min(limite_valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(alarma_diaria_real_train, by = c("dwell_numb", "date")) %>%
  left_join(alarma_diaria_pred_train, by = c("dwell_numb", "date"))









# ===================================================================
#  REGRESIÓN LOGÍSTICA DIARIA
# ===================================================================

train_daily <- daily_train %>%
  drop_na(Int_T_pred_max, limite_min, alarma_real_dia)

test_daily <- daily_test %>%
  drop_na(Int_T_pred_max, limite_min, alarma_real_dia)

modelo_logistico <- glm(
  alarma_real_dia ~ Int_T_pred_max + limite_min,
  data = train_daily,
  family = binomial
)

summary(modelo_logistico)

# PREDECIR PROBABILIDAD DE ALARMA EN TEST (DIARIO)
test_daily$prob_alarma <- predict(modelo_logistico, newdata = test_daily, type = "response")

# Probabilidad en porcentaje
test_daily$prob_alarma_pct <- round(test_daily$prob_alarma * 100, 1)



# PERCENTIL CONDICIONADO A ALARMAS REALES (DIARIO)

# Extraer solo días con alarma real (1)
prob_alarmas_reales <- test_daily$prob_alarma[test_daily$alarma_real_dia == 1]

# Percentil 10 (o el que quieras)
percentil_cond <- quantile(prob_alarmas_reales, probs = 0.10)
percentil_cond
# Guarda el umbral real
umbral_pct10 <- as.numeric(percentil_cond * 100)


#BUSCAR FILA MÁS CERCANA AL UMBRAL (%)
fila_cercana <- test_daily %>%
  mutate(dif = abs(prob_alarma_pct - umbral_pct10)) %>%
  arrange(dif) %>%
  slice(1) %>%
  select(Int_T_pred_max, limite_min, prob_alarma_pct, alarma_real_dia)

fila_cercana


#  DICOTOMIZAR PROBABILIDAD SEGÚN PERCENTIL 10
test_daily <- test_daily %>%
  mutate(
    alarma_pred_P10 = ifelse(prob_alarma_pct >= umbral_pct10, 1, 0)
  )


# MATRIZ DE CONFUSIÓN DIARIA (HEATMAP)
plot_conf_matrix <- function(data, pred_col, label) {
  
  # Fuente Times New Roman
  windowsFonts(TimesNR = windowsFont("Times New Roman"))
  
  conf_mat <- table(
    Real = factor(data$alarma_real_dia, levels = c(0, 1)),
    Predicho = factor(data[[pred_col]], levels = c(0, 1))
  ) %>% as.data.frame()
  
  p <- ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Freq), size = 14, family = "TimesNR") +
    scale_fill_gradient(low = "white", high = "steelblue", name = "") +
    labs(
      x = "Alarm_predicted",
      y = "Alarm_real"
    ) +
    theme_minimal(base_family = "TimesNR") +
    theme(
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    )
  
  print(p)
  return(conf_mat)
}

# Ejecutar matriz de confusión para el percentil 10
conf_diario_P10 <- plot_conf_matrix(test_daily, "alarma_pred_P10", "Percentil 10")







#============================================
#PREDECIR EL TIPO DE ALARMA
#============================================

# Función que genera el tipo diario
crear_tipo_diario <- function(df) {
  df %>%
    group_by(dwell_numb, date) %>%
    summarise(
      alarma_dia = ifelse(
        any(periodo == "dia"),
        max(alarma_real[periodo == "dia"], na.rm = TRUE),
        0
      ),
      alarma_noche = ifelse(
        any(periodo == "noche"),
        max(alarma_real[periodo == "noche"], na.rm = TRUE),
        0
      ),
      .groups = "drop"
    ) %>%
    mutate(
      alarma_diaria_tipo = case_when(
        alarma_dia == 0 & alarma_noche == 0 ~ 0,
        alarma_dia == 0 & alarma_noche == 1 ~ 1,
        alarma_dia == 1 & alarma_noche == 0 ~ 2,
        alarma_dia == 1 & alarma_noche == 1 ~ 3
      )
    )
}

# Aplicar en train y en test
  alarma_tipo_train <- crear_tipo_diario(franjas_pred_train)
  alarma_tipo_test  <- crear_tipo_diario(franjas_pred_test)

  
  
  
#Unir esta nueva variable al dataset diario
  daily_train <- daily_train %>%
    left_join(alarma_tipo_train, by = c("dwell_numb","date"))
  
  daily_test <- daily_test %>%
    left_join(alarma_tipo_test, by = c("dwell_numb","date"))

  
#Preparar dataset para modelo
  daily_train$alarma_diaria_tipo <- factor(daily_train$alarma_diaria_tipo,
                                           levels = c(0,1,2,3))
  daily_test$alarma_diaria_tipo  <- factor(daily_test$alarma_diaria_tipo,
                                           levels = c(0,1,2,3))

#Ajustar el modelo multinomial (tipo de alarma)
  modelo_multinom <- multinom(
    alarma_diaria_tipo ~ Int_T_pred_max + limite_min,
    data = daily_train
  )
  
  daily_test$alarma_pred_tipo <- predict(modelo_multinom, newdata = daily_test)
  
  
#Matriz de confusión
  conf_mat <- table(
    Real = daily_test$alarma_diaria_tipo,
    Predicho = daily_test$alarma_pred_tipo
  )
  
  conf_mat
  
  library(ggplot2)
  library(dplyr)
  
  # Formatear tabla para ggplot
  conf_df <- as.data.frame(conf_mat)
  
  # Asegurar fuente (en Windows)
  windowsFonts(TimesNR = windowsFont("Times New Roman"))
  
  ggplot(conf_df, aes(Predicho, Real, fill = Freq)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Freq), size = 10, family = "TimesNR") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      x = "Predicted alarm type",
      y = "Real alarm type",
      title = "Confusion matrix (multinomial model)"
    ) +
    theme_minimal(base_family = "TimesNR") +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    )
  
