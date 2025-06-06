#IMPORTAR BASE DE DATOS Vivreales_Tdiario_20212022
viv_verano_diario <- Vivreales_Tdiario_20212022_7


#CREAR ALARMA EN BASE A LIMITE ADAPTATIVO DIARIO EN 16798-1:2019 =========================
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

#Miramos cómo es la distribución dentro de la variable dicotómica "alarma"
table(viv_verano_diario$alarma_real)

#MODELO PREDICTIVO VARIABLE ALARMA (DICOTOMICA) ==============================================
library(rsample)

set.seed(123)  # Para reproducibilidad
split <- initial_split(viv_verano_diario, prop = 0.7)  # 70% train, 30% test
train_data <- training(split)
test_data <- testing(split)

#Modelo de RLM con los datos de entrenamiento
modelo_rlm <- lm(Int_T ~ Ext_T + Ext_RAD + 
                   Ext_T_1 + Ext_T_2 + Ext_T_3 
                 + Int_T_1 + Int_T_2 + Int_T_3, 
                 data = train_data)

# Predecir Int_T sobre los datos test
test_data$Int_T_pred <- predict(modelo_rlm, newdata = test_data)

#Calcular alarma usando la fórmula, pero con las variables de test:
test_data$alarma_test <- ifelse(test_data$trm > 30 | test_data$Int_T_pred > test_data$limiteadap, 1, 0)


#DESEMPEÑO

#Residuos
test_data$residuos_alarma <- test_data$alarma_real - test_data$alarma_test

# Tabla de contingencia para variable dicotomica (Alarma)
table(test_data$alarma_real, test_data$alarma_test)

# Crear tabla de contingencia
conf_mat <- table(Real = test_data$alarma_real, Predicho = test_data$alarma_test) %>% 
  as.data.frame()

# Graficar heatmap
ggplot(conf_mat, aes(x = Predicho, y = Real, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de Confusión Heatmap",
       x = "Alarma Predicha",
       y = "Alarma Real") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18, face="bold"))

#Métricas para ver cómo funciona la predicción de alarma
conf_matrix <- table(test_data$alarma_real, test_data$alarma_test)

# Extraer los valores
TN <- conf_matrix[1, 1]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

# Calcular métricas
accuracy  <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * (precision * recall) / (precision + recall)

# Mostrar resultados con 3 decimales
cat("=== MÉTRICAS DE DESEMPEÑO ===\n")
cat("Accuracy :", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall   :", round(recall, 3), "\n")
cat("F1 Score :", round(f1_score, 3), "\n")


