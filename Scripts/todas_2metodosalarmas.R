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
# 2️⃣ Procesar viviendas completas con Método 1 y Método 2
# ============================================================
set.seed(123)
split <- initial_split(Vivtodas_diario_media, prop = 0.7)
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

# Método 2: probabilidad de alarma (umbral = 0.3)
sigma <- sd(resid(modelo_rlm))
test_data$prob_alarma <- 1 - pnorm(test_data$limiteadap,
                                   mean = test_data$Int_T_pred,
                                   sd = sigma)
test_data$Alarma_pred2 <- ifelse(test_data$prob_alarma >= 0.3, 1, 0)

# ============================================================
# 3️⃣ Función de recompensa y evaluación
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

# Evaluación Método 1
res1 <- calcular_recompensa(test_data$alarma_real, test_data$Alarma_pred)
res1$Metodo <- "Metodo 1"

# Evaluación Método 2 (umbral 0.3)
res2 <- calcular_recompensa(test_data$alarma_real, test_data$Alarma_pred2)
res2$Metodo <- "Metodo 2 (umbral 0.3)"

# Unir resultados
resultados <- bind_rows(res1, res2)
print(resultados)

# ============================================================
# 4️⃣ Visualización comparativa
# ============================================================
ggplot(resultados, aes(x = Metodo, y = Recompensa, fill = Metodo)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Comparación de Recompensa entre Métodos (umbral Método 2 = 0.3)",
       x = "Método",
       y = "Recompensa total") +
  theme_minimal(base_size = 14)
