#IMPORTAR DATASET =====================================================================================================


#PREPARAR EL MODELO =====================================================================================================

install.packages("caTools")
library(caTools)

# Dividir el conjunto de datos en entrenamiento y prueba (70%-30%)
set.seed(123)  # Para asegurar la replicabilidad
split <- sample.split(dataset_tem_daily_ARX$Temp, SplitRatio = 0.7)

# Crear los conjuntos de entrenamiento y prueba
train_data <- subset(dataset_tem_daily_ARX, split == TRUE)
test_data <- subset(dataset_tem_daily_ARX, split == FALSE)

# 1. ENTRENAR EL MODELO COMPLETO
modelo_lr <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                  In_temp_1 + In_temp_2 + In_temp_3 + 
                  Out_temp_1 + Out_temp_2 + Out_temp_3, 
                data = train_data)

# 2. CALCULAR LA MEDIA DE CADA VARIABLE INTERIOR EN TRAINING
in_temp_1_media <- mean(train_data$In_temp_1, na.rm = TRUE)
in_temp_2_media <- mean(train_data$In_temp_2, na.rm = TRUE)
in_temp_3_media <- mean(train_data$In_temp_3, na.rm = TRUE)

# 3. CREAR UNA COPIA DEL TEST SET Y REEMPLAZAR VARIABLES INTERIORES POR SU MEDIA
test_data_sin_interior <- test_data
test_data_sin_interior$In_temp_1 <- in_temp_1_media
test_data_sin_interior$In_temp_2 <- in_temp_2_media
test_data_sin_interior$In_temp_3 <- in_temp_3_media

# 4. HACER PREDICCIONES CON EL MODELO COMPLETO USANDO VARIABLES INTERIORES SIMULADAS
predicciones_sin_interior <- predict(modelo_lr, newdata = test_data_sin_interior)

# 5. CALCULAR MÉTRICAS DE ERROR
rmse_sin_interior <- sqrt(mean((test_data$Temp - predicciones_sin_interior)^2))
mae_sin_interior <- mean(abs(test_data$Temp - predicciones_sin_interior))

# 6. MOSTRAR RESULTADOS
cat("Modelo completo simulando que no se conocen las temperaturas interiores previas:\n")
cat("  RMSE:", rmse_sin_interior, "\n")
cat("  MAE :", mae_sin_interior, "\n")

# Ver distribución real en test y valor simulado
boxplot(test_data$In_temp_1,
        horizontal = TRUE,
        main = "Distribución real de In_temp_1 en test_data",
        col = "lightblue")
abline(v = in_temp_1_media, col = "red", lty = 2)

