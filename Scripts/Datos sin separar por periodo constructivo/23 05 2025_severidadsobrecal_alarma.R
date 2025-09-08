#IMPORTAR EL DATASET HORARIO =========================================================================================
#Dataset horario

#1: Convertir Date a POSIXct
Temp_int_hourly$datetime <- as.POSIXct(
  Temp_int_hourly$Date,
  format = "%d/%m/%Y %H:%M:%S",
  tz = "UTC"
)

#2: Crear variable binaria de sobrecalentamiento y extraer la fecha.
#Se considera sobrecalentamiento cuando la temperatura interior supera los 26ºC
library(dplyr)

Temp_int_hourly <- Temp_int_hourly %>%
  mutate(
    riesgo_sobrec = ifelse(Indoor_T > 26, 1, 0),
    dia = as.Date(datetime)
  )





##CONSIDERANDO LA MEDIA DIARIA## ============================
#Agregar por día: contar horas con sobrecalentamiento en el día y promediar condiciones exteriores y temp interior
riesgo_diario <- Temp_int_hourly %>%
  group_by(dia) %>%
  summarise(
    horas_riesgo = sum(riesgo_sobrec, na.rm = TRUE),
    T_ext_media = mean(Outdoor_T, na.rm = TRUE),
    Rad_media = mean(Outdoor_GR, na.rm = TRUE),
    T_int_media = mean(Indoor_T, na.rm = TRUE)
  )

#Mostrar resultado
print(riesgo_diario)


#A partir de aquí se usa el dataframe=riesgo_diario =========


#Crear las variables de días previos
library(dplyr)

riesgo_diario <- riesgo_diario %>%
  arrange(dia) %>%
  mutate(
    T_ext_lag1 = lag(T_ext_media, 1),
    T_ext_lag2 = lag(T_ext_media, 2),
    T_ext_lag3 = lag(T_ext_media, 3),
    Rad_lag1 = lag(Rad_media, 1),
    Rad_lag2 = lag(Rad_media, 2),
    Rad_lag3 = lag(Rad_media, 3),
    T_int_lag1 = lag(T_int_media, 1),
    T_int_lag2 = lag(T_int_media, 2),
    T_int_lag3 = lag(T_int_media, 3)
  ) %>%
  filter(!is.na(T_ext_lag3))  # Eliminar los primeros días con NA por lag

#Modelo de RLM
modelo_riesgo <- lm(horas_riesgo ~ T_ext_media + T_ext_lag1 + T_ext_lag2 + T_ext_lag3 +
                      Rad_media + Rad_lag1 + Rad_lag2 + Rad_lag3 +
                      T_int_lag1 + T_int_lag2 + T_int_lag3,
                    data = riesgo_diario)

summary(modelo_riesgo)

#Validación cruzada

library(caret)

set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
modelo_cv <- train(horas_riesgo ~ T_ext_media + T_ext_lag1 + T_ext_lag2 + T_ext_lag3 +
                     Rad_media + Rad_lag1 + Rad_lag2 + Rad_lag3 +
                     T_int_lag1 + T_int_lag2 + T_int_lag3,
                   data = riesgo_diario,
                   method = "lm",
                   trControl = train_control)
print(modelo_cv)

#Obterner predicciones

riesgo_diario$predicciones <- predict(modelo_riesgo, newdata = riesgo_diario)

#Comparar visualmente (opcional pero útil)
#TABLA
comparacion <- riesgo_diario %>%
  select(dia, horas_riesgo, predicciones)

head(comparacion, 10)  # Muestra las primeras 10 filas

#GRÁFICO
library(ggplot2)

ggplot(riesgo_diario, aes(x = dia)) +
  geom_line(aes(y = horas_riesgo, color = "Real")) +
  geom_line(aes(y = predicciones, color = "Predicho")) +
  labs(title = "Horas de riesgo: Real vs Predicho",
       y = "Horas de riesgo",
       color = "") +
  theme_minimal()









##CONSIDERANDO LA MÁXIMA DIARIA## ============================

#Agregar por día: contar horas con sobrecalentamiento en el día y promediar condiciones exteriores y temp interior
riesgo_diario_max <- Temp_int_hourly %>%
  group_by(dia) %>%
  summarise(
    horas_riesgo = sum(riesgo_sobrec, na.rm = TRUE),
    T_ext_max = max(Outdoor_T, na.rm = TRUE),
    Rad_max = max(Outdoor_GR, na.rm = TRUE),
    T_int_max = max(Indoor_T, na.rm = TRUE)
  )

#Mostrar resultado
print(riesgo_diario_max)


#A partir de aquí se usa el dataframe=riesgo_diario_max =========



#Crear las variables de días previos
library(dplyr)

riesgo_diario_max <- riesgo_diario_max %>%
  arrange(dia) %>%
  mutate(
    T_ext_lag1 = lag(T_ext_max, 1),
    T_ext_lag2 = lag(T_ext_max, 2),
    T_ext_lag3 = lag(T_ext_max, 3),
    Rad_lag1 = lag(Rad_max, 1),
    Rad_lag2 = lag(Rad_max, 2),
    Rad_lag3 = lag(Rad_max, 3),
    T_int_lag1 = lag(T_int_max, 1),
    T_int_lag2 = lag(T_int_max, 2),
    T_int_lag3 = lag(T_int_max, 3)
  ) %>%
  filter(!is.na(T_ext_lag3))  # Eliminar los primeros días con NA por lag

#Modelo de RLM
modelo_riesgo_max <- lm(horas_riesgo ~ T_ext_max + T_ext_lag1 + T_ext_lag2 + T_ext_lag3 +
                      Rad_max + Rad_lag1 + Rad_lag2 + Rad_lag3 +
                      T_int_lag1 + T_int_lag2 + T_int_lag3,
                    data = riesgo_diario_max)

summary(modelo_riesgo_max)

#Obterner predicciones

riesgo_diario_max$predicciones_max <- predict(modelo_riesgo_max, newdata = riesgo_diario_max)

#Comparar visualmente (opcional pero útil)
#TABLA
comparacion <- riesgo_diario_max %>%
  select(dia, horas_riesgo, predicciones_max)

head(comparacion, 10)  # Muestra las primeras 10 filas

#GRÁFICO
library(ggplot2)

ggplot(riesgo_diario_max, aes(x = dia)) +
  geom_line(aes(y = horas_riesgo, color = "Real")) +
  geom_line(aes(y = predicciones_max, color = "Predicho")) +
  labs(title = "Horas de riesgo: Real vs Predicho",
       y = "Horas de riesgo",
       color = "") +
  theme_minimal()








