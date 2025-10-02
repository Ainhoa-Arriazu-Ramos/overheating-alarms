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

# Asegúrate de tener en test_data$alarma_real las alarmas reales (0/1)
# Método 1: alarma_test (0/1)
# Método 2: alarma_pred_m2 (0/1), prob_alarma (probabilidad continua)
  #Para ello, hay que haber corrido el script "3. Alarma_probabilidad". 



# FUNCION PARA MÉTRICAS ============================================================================

get_metrics <- function(real, pred, prob = NULL, metodo = "M1") {
  
  cm <- confusionMatrix(
    factor(real, levels = c(0,1)),
    factor(pred, levels = c(0,1)),
    positive = "1"
  )
  
  out <- data.frame(
    Metodo = metodo,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],   # Recall
    Specificity = cm$byClass["Specificity"],
    Precision   = cm$byClass["Pos Pred Value"],
    F1          = cm$byClass["F1"],
    FN_rate     = 1 - cm$byClass["Sensitivity"],  # Tasa de falsos negativos
    FP_rate     = 1 - cm$byClass["Specificity"]   # Tasa de falsos positivos
  )
  
  # AUC solo si hay probabilidades
  if(!is.null(prob)){
    roc_obj <- roc(real, prob)
    out$AUC <- as.numeric(auc(roc_obj))
  } else {
    out$AUC <- NA
  }
  
  return(out)
}

# CALCULAR MÉTRICAS ============================================================

# Método 1
res_m1 <- get_metrics(test_data$alarma_real,
                      test_data$alarma_test,
                      metodo = "M1")

# Método 2
res_m2 <- get_metrics(test_data$alarma_real,
                      test_data$alarma_pred_m2,
                      prob = test_data$prob_alarma,
                      metodo = "M2")


resumen <- bind_rows(res_m1, res_m2)
print(resumen)


# GRÁFICO ROC (Método 2) + Punto Método 1 =================================================

roc_obj <- roc(test_data$alarma_real, test_data$prob_alarma)
plot(roc_obj, col = "blue", lwd = 2, main = "ROC - Método 2 (logístico)")
abline(a=0, b=1, lty=2, col="gray")

# Si quieres marcar el punto del método 1 en la curva:
sens_m1 <- as.numeric(res_m1$Sensitivity)
spec_m1 <- as.numeric(res_m1$Specificity)
points(1-spec_m1, sens_m1, col="red", pch=19, cex=1.5)
legend("bottomright", legend=c("Método 2 ROC", "Método 1 (punto)"),
       col=c("blue","red"), lty=c(1,NA), pch=c(NA,19))



# BARPLOT COMPARATIVO DE MÉTRICAS ======================================================================

# Seleccionar solo métricas relevantes
resumen_plot <- resumen %>%
  select(Metodo, Sensitivity, Specificity, Accuracy, F1) %>%
  pivot_longer(cols = -Metodo,
               names_to = "Metrica",
               values_to = "Valor")

# Crear barplot
ggplot(resumen_plot, aes(x = Metrica, y = Valor, fill = Metodo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Valor, 2)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("M1" = "steelblue", "M2" = "darkorange")) +
  ylim(0, 1) +
  labs(title = "Comparación de métricas entre Métodos",
       x = "Métrica",
       y = "Value (0–1)",
       fill = "Method") +
  theme_minimal(base_size = 14)
