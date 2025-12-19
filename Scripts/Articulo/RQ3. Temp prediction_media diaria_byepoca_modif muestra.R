#==============================================================================================
#PREDICCIÓN DIARIA (2 dias previos) - Separando segun época constructiva y modificando muestra
#==============================================================================================

#Importar la base de datos: Vivtodas_diario_media.xlsx

#Cargar librerías
library(dplyr)
library(lubridate)
library(rsample)
library(ggplot2)



#Variables desfasadas; dentro de la misma vivienda y desfasando respecto a la fecha======================
Vivtodas_diario_media <- Vivtodas_diario_media %>%
  mutate(fecha = make_date(year, month, day)) %>%
  group_by(dwell_numb) %>%
  arrange(fecha) %>%
  mutate(
    Int_T_1 = if_else(as.integer(fecha - lag(fecha, 1)) == 1, lag(Int_T, 1), NA_real_),
    Int_T_2 = if_else(as.integer(fecha - lag(fecha, 2)) == 2, lag(Int_T, 2), NA_real_),
    
    Ext_T_1 = if_else(as.integer(fecha - lag(fecha, 1)) == 1, lag(Ext_T, 1), NA_real_),
    Ext_T_2 = if_else(as.integer(fecha - lag(fecha, 2)) == 2, lag(Ext_T, 2), NA_real_),
    
  ) %>%
  ungroup()

# Quitar filas con entradas NA
Vivtodas_diario_media <- na.omit(Vivtodas_diario_media)









#División en dos subdatasets=================================================================
Viv_antescte <- subset(Vivtodas_diario_media, dwell_numb %in% c(1,2,3,4,5,6)) #Antes del CTE
Viv_despuesscte <- subset(Vivtodas_diario_media, dwell_numb %in% c(7,8,9,10,11,12)) #Después del CTE









#==============================================================================================
#ANTES DEL CTE: 1-2-3-4-5-6
#==============================================================================================

#ARX con todos los datos de antes del CTE =====================================================

# Dividimos el dataset según dwell_numb
train_viviendas_antescte <- c(1, 3, 5, 6)
test_viviendas_antescte  <- c(2, 4)

# Crear subconjuntos
train_data_antescte <- Viv_antescte %>%
  filter(dwell_numb %in% train_viviendas_antescte)

test_data_antescte <- Viv_antescte %>%
  filter(dwell_numb %in% test_viviendas_antescte)

#Modelo de ARX con los datos de entrenamiento
modelo_arx_antescte <- lm(Int_T ~ Ext_T + 
                            Ext_T_1 + Ext_T_2 
                          + Int_T_1 + Int_T_2, 
                          data = train_data_antescte)

summary(modelo_arx_antescte)


# Predecir Int_T sobre los datos test
test_data_antescte$Int_T_pred <- predict(modelo_arx_antescte, newdata = test_data_antescte)

#Gráfico de dispersion para variable Int_T 
windowsFonts(Times = windowsFont("Times New Roman"))

ggplot(test_data_antescte, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(
    x = "Real Mean Indoor Temperature (°C)",
    y = "Predicted Mean Indoor Temperature (°C)"
  ) +
  xlim(20, 32) +
  ylim(20, 32) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )











#===============================================================================
# FILTRADO DE DATOS BASADO EN ERRORES (ANTES DEL CTE)
#===============================================================================

# 1. Predecir en TODA la base Viv_antescte y calcular el error
Viv_antescte <- Viv_antescte %>%
  mutate(
    Int_T_pred = predict(modelo_arx_antescte, newdata = Viv_antescte),
    error = Int_T - Int_T_pred
  )

# 2. Histograma de errores (solo TEST, para diagnóstico)
test_data_antescte <- Viv_antescte %>%
  filter(dwell_numb %in% test_viviendas_antescte)

ggplot(test_data_antescte, aes(x = error)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.25,
    color = "black",
    fill = "grey80"
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(test_data_antescte$error, na.rm = TRUE),
      sd   = sd(test_data_antescte$error, na.rm = TRUE)
    ),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    x = "Prediction Error (Real − Predicted) (°C)",
    y = "Density"
  ) +
  theme_minimal(base_family = "Times")


# 3. Calcular estadísticos
error_train <- Viv_antescte %>%
  filter(dwell_numb %in% train_viviendas_antescte) %>%
  pull(error)

med_error <- median(error_train, na.rm = TRUE)
sd_error  <- sd(error_train, na.rm = TRUE)

# Definir límites
lim_inf <- med_error - 2 * sd_error
lim_sup <- med_error + 2 * sd_error

# 4. Gráfico con límites de filtrado (sobre TEST)
ggplot(test_data_antescte, aes(x = error)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.25,
    fill = "grey80",
    color = "black"
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = med_error,
      sd   = sd_error
    ),
    color = "blue",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = c(lim_inf, lim_sup),
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  labs(
    x = "Prediction Error (Real − Predicted) (°C)",
    y = "Density"
  ) +
  theme_minimal(base_family = "Times")

#===============================================================================
# CREAR BASE DE DATOS FILTRADA (TODA Viv_antescte)
#===============================================================================

Viv_antescte_filtrado <- Viv_antescte %>%
  filter(error >= lim_inf & error <= lim_sup)

# NUEVA DIVISIÓN TRAIN / TEST SOBRE LA BASE FILTRADA
train_data_antescte_filtrado <- Viv_antescte_filtrado %>%
  filter(dwell_numb %in% train_viviendas_antescte)

test_data_antescte_filtrado <- Viv_antescte_filtrado %>%
  filter(dwell_numb %in% test_viviendas_antescte)

# ARX REENTRENADO CON LA BASE FILTRADA
modelo_arx_antescte_filtrado <- lm(
  Int_T ~ Ext_T + Ext_T_1 + Ext_T_2 + Int_T_1 + Int_T_2,
  data = train_data_antescte_filtrado
)

summary(modelo_arx_antescte_filtrado)

#Predecir sobre test
test_data_antescte_filtrado <- test_data_antescte_filtrado %>%
  mutate(
    Int_T_pred = predict(
      modelo_arx_antescte_filtrado,
      newdata = test_data_antescte_filtrado
    )
  )


#Métricas de error
test_data_antescte_filtrado <- test_data_antescte_filtrado %>%
  mutate(
    Int_T_pred = predict(
      modelo_arx_antescte_filtrado,
      newdata = test_data_antescte_filtrado
    )
  )


# Errores
errores_filtrado <- test_data_antescte_filtrado$Int_T -
  test_data_antescte_filtrado$Int_T_pred

# MAE
MAE_filtrado <- mean(abs(errores_filtrado), na.rm = TRUE)

# RMSE
RMSE_filtrado <- sqrt(mean(errores_filtrado^2, na.rm = TRUE))

MAE_filtrado
RMSE_filtrado


ggplot(test_data_antescte_filtrado, aes(x = Int_T, y = Int_T_pred)) +
  geom_point(color = "black") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "blue"
  ) +
  labs(
    x = "Real Mean Indoor Temperature (°C)",
    y = "Predicted Mean Indoor Temperature (°C)"
  ) +
  xlim(20, 32) +
  ylim(20, 32) +
  coord_fixed(ratio = 1) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )
