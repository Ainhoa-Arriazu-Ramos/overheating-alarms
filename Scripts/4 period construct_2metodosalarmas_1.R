# ============================================================
# 0️⃣ Importar y cargar librerías
# ============================================================
library(readxl)
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)
library(pROC)

#Importar base de datos: Vivtodas_diario_media.xlsx

# ============================================================
# 1️⃣ Preparar la base de datos
# ============================================================
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(fecha = make_date(year, month, day)) %>%
  group_by(dwell_numb) %>%
  arrange(dwell_numb, fecha) %>%
  mutate(
    Int_T_1 = lag(Int_T, 1),
    Int_T_2 = lag(Int_T, 2),
    Int_T_3 = lag(Int_T, 3),
    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3)
  ) %>%
  ungroup() %>%
  na.omit() %>%
  mutate(
    trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3),
    limiteadap = (0.33 * trm) + 21.8,
    alarma_real = if_else(trm > 30 | Int_T > limiteadap, 1, 0)
  )

# ============================================================
# 2️⃣ Dividir por periodos constructivos
# ============================================================
Viv_sinnormativa <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(1, 2, 3, 4))
Viv_ct79        <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(5, 6))
Viv_cte2006     <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(7, 8, 9))
Viv_cte2019     <- Vivtodas_diario_media %>% filter(dwell_numb %in% c(10, 11, 12))

# ============================================================
# 3️⃣ Función para procesar cada periodo con Método 1 y Método 2
# ============================================================
procesar_viviendas <- function(df){
  set.seed(123)
  split <- initial_split(df, prop = 0.7)
  train_data <- training(split)
  test_data  <- testing(split)
  
  # Modelo de regresión lineal múltiple
  modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD +
                     Ext_T_1 + Ext_T_2 + Ext_T_3 +
                     Int_T_1 + Int_T_2 + Int_T_3,
                   data = train_data)
  
  # Predicción de temperatura
  test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)
  
  # Método 1: alarma predicha binaria
  test_data$Alarma_pred <- ifelse(test_data$trm > 30 | test_data$Int_T_pred > test_data$limiteadap, 1, 0)
  
  # Método 2: probabilidad de alarma
  sigma <- sd(resid(modelo_rlm))
  test_data$prob_alarma <- 1 - pnorm(test_data$limiteadap, mean = test_data$Int_T_pred, sd = sigma)
  test_data$prob_alarma_pct <- round(test_data$prob_alarma * 100, 1)
  
  return(test_data)
}

# ============================================================
# 4️⃣ Aplicar función a cada periodo
# ============================================================
test_sinnormativa <- procesar_viviendas(Viv_sinnormativa)
test_ct79        <- procesar_viviendas(Viv_ct79)
test_cte2006     <- procesar_viviendas(Viv_cte2006)
test_cte2019     <- procesar_viviendas(Viv_cte2019)

# ============================================================
# 5️⃣ Función de recompensa y evaluación
# ============================================================
calcular_recompensa <- function(y_real, y_pred, 
                                recompensa_tp = 1, castigo_fn = -3, 
                                castigo_fp = -1, neutro_tn = 0){
  cm <- table(Real = y_real, Predicho = y_pred)
  
  TP <- cm["1","1"]; TN <- cm["0","0"]; 
  FP <- cm["0","1"]; FN <- cm["1","0"]
  
  TP <- ifelse(is.na(TP), 0, TP)
  TN <- ifelse(is.na(TN), 0, TN)
  FP <- ifelse(is.na(FP), 0, FP)
  FN <- ifelse(is.na(FN), 0, FN)
  
  recompensa_total <- TP*recompensa_tp + FN*castigo_fn + FP*castigo_fp + TN*neutro_tn
  
  return(data.frame(
    TP = TP, TN = TN, FP = FP, FN = FN,
    Recompensa = recompensa_total
  ))
}

evaluar_periodo <- function(df, nombre_periodo, umbral = 0.5){
  # Método 1
  res1 <- calcular_recompensa(df$alarma_real, df$Alarma_pred)
  res1$Metodo <- "Metodo 1"
  res1$Periodo <- nombre_periodo
  
  # Método 2
  alarma_pred2 <- ifelse(df$prob_alarma >= umbral, 1, 0)
  res2 <- calcular_recompensa(df$alarma_real, alarma_pred2)
  res2$Metodo <- "Metodo 2"
  res2$Periodo <- nombre_periodo
  
  return(rbind(res1, res2))
}

# ============================================================
# 6️⃣ Aplicar evaluación a los 4 periodos
# ============================================================
resultados <- bind_rows(
  evaluar_periodo(test_sinnormativa, "sinnormativa"),
  evaluar_periodo(test_ct79, "ct79"),
  evaluar_periodo(test_cte2006, "cte2006"),
  evaluar_periodo(test_cte2019, "cte2019")
)

print(resultados)

# ============================================================
# 7️⃣ Visualización comparativa
# ============================================================
ggplot(resultados, aes(x = Periodo, y = Recompensa, fill = Metodo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Recompensa entre Métodos",
       x = "Periodo",
       y = "Recompensa total") +
  theme_minimal(base_size = 14)
