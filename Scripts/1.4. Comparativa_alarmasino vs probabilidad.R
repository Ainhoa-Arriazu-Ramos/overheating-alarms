# ======================================================
# COMPARACIÓN MÉTODO 1 vs MÉTODO 2
# ======================================================

#Cargar bibliotecas
library(caret)
library(pROC)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# Asegúrate de tener en test_data:
  # Alarma_real las alarmas reales (0/1)
  # Método 1: alarma_test (0/1)
  # Método 2: alarma_pred_P10 (0/1), prob_alarma (probabilidad continua)
    
    #Para ello, hay que haber corrido el script "3. Alarma_probabilidad". 



# FUNCION PARA MÉTRICAS ============================================================================

# Función para calcular métricas
calcular_metricas <- function(real, predicho) {
  conf_mat <- table(Real = factor(real, levels = c(0,1)),
                    Predicho = factor(predicho, levels = c(0,1)))
  
  TP <- conf_mat["1","1"]
  TN <- conf_mat["0","0"]
  FP <- conf_mat["0","1"]
  FN <- conf_mat["1","0"]
  
  sensibilidad <- TP / (TP + FN)
  especificidad <- TN / (TN + FP)
  precision <- TP / (TP + FP)          # También llamado PPV
  accuracy <- (TP + TN) / sum(conf_mat)
  
  data.frame(
    Sensibilidad = round(sensibilidad,3),
    Especificidad = round(especificidad,3),
    Precision = round(precision,3),
    Accuracy = round(accuracy,3)
  )
}

# -------------------------------------------------
# Método 1: alarma_real vs alarma_test
# -------------------------------------------------
metricas_metodo1 <- calcular_metricas(test_data$alarma_real, test_data$alarma_test)
metricas_metodo1$Metodo <- "alarma_test"

# -------------------------------------------------
# Método 2: alarma_real vs alarma_pred_P10
# -------------------------------------------------
metricas_metodo2 <- calcular_metricas(test_data$alarma_real, test_data$alarma_pred_P10)
metricas_metodo2$Metodo <- "alarma_pred_P10"

# -------------------------------------------------
# Combinar resultados
# -------------------------------------------------
metricas <- bind_rows(metricas_metodo1, metricas_metodo2)
metricas
