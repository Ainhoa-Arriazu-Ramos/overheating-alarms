#SCRIPT CON OTROS CONJUNTOS DE ENTRENAMIENTO ===================================================================================
#Train= Enero, Febrero, Marzo, Abril, Mayo, Junio, Septiembre, Octubre, Noviembre y Diciembre
#Test= Julio y Agosto

# Extrae el mes en una nueva columna "Month"
dataset_tem_daily_ARX$Month <- format(dataset_tem_daily_ARX$Date, "%m")

# Define los meses para entrenamiento y test
train_months <- c("01", "02", "03", "04", "05", "06", "09", "10", "11")
test_months <- c("07", "08")

# Filtra los conjuntos
train_data_year <- subset(dataset_tem_daily_ARX, Month %in% train_months)
test_data_summer <- subset(dataset_tem_daily_ARX, Month %in% test_months)


#REGRESIÓN LINEAL: Modelo completo=====================================

# Entrenar un modelo de regresión lineal
modelo_lr_grupnoal <- lm(Temp ~ Outdoor_Temperature + Outdoor_GlobRadiation + 
                           In_temp_1 + In_temp_2 + In_temp_3 + 
                           Out_temp_1 + Out_temp_2 + Out_temp_3, 
                         data = train_data_year)


# Resumen del modelo
summary(modelo_lr_grupnoal)


# Hacer predicciones con el conjunto de prueba
predicciones_lr_grupnoal <- predict(modelo_lr_grupnoal, newdata = test_data_summer)


# Ver las primeras predicciones
head(predicciones_lr_grupnoal)


# Valores reales (soin los valores reales de temperatura del subdataset test)
valores_reales_lr_grupnoal <- test_data_summer$Temp


# Calcular residuos (diferencia entre valores reales y las predicciones)
residuos_lr_grupnoal <- valores_reales_lr_grupnoal  - predicciones_lr_grupnoal 


# Calcular métricas de error
mse_lr_grupnoal  <- mean(residuos_lr_grupnoal ^2)         # Error Cuadrático Medio (MSE)
rmse_lr_grupnoal  <- sqrt(mse_lr_grupnoal )               # Raíz del Error Cuadrático Medio (RMSE)
mae_lr_grupnoal  <- mean(abs(residuos_lr_grupnoal ))      # Error Absoluto Medio (MAE)


# Imprimir resultados
cat("MSE:", mse_lr_grupnoal , "\n")
cat("RMSE:", rmse_lr_grupnoal , "\n")
cat("MAE:", mae_lr_grupnoal , "\n")


# Graficar predicciones vs. valores reales
plot(valores_reales_lr_grupnoal , predicciones_lr_grupnoal , 
     main="Regresión lineal: Predicciones vs Valores Reales (grupos no aleatorios)",
     xlab="Valores Reales (Temperatura)", 
     ylab="Predicciones (Temperatura)",
     pch=19, col="blue")

# Agregar una línea de referencia (y = x) para ver el ajuste perfecto
abline(a=0, b=1, col="red", lwd=2)  # Línea roja de referencia
