#Importar base de datos: Vivtodas_verano_horario==========================================================

#Importar librerias
library(dplyr)
library(lubridate)


#0. Quitar grados hora
Vivtodas_verano_horario <- Vivtodas_verano_horario %>% select(-grados_hora)

#1. Definiciones de franjas
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
    hour %in% 0:7   ~ 3,     # 0-7 (dos bloques noche 0-3 y 4-7), peso 3
    hour %in% 8:11  ~ 1,
    hour %in% 12:15 ~ 2,
    hour %in% 16:19 ~ 2,
    hour %in% 20:23 ~ 1.5
  ),
  tipo_umbral = case_when(
    hour %in% c(0:7, 20:23) ~ "fijo",         # nocturnas -> fijo 26
    TRUE                    ~ "adaptativo"    # diurnas -> adaptativo
  )
)


#2. Añadir columna date y asociar franja a cada fila horaria
Vivtodas_verano_horario2 <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day)) %>%
  left_join(franjas, by = "hour")



#2. Calcular la media por franja (media de Int_T sobre las horas de cada franja)
#      por vivienda y por día
Vivtodas_franja_mean <- Vivtodas_verano_horario2 %>%
  group_by(dwell_numb, date, franja, peso, tipo_umbral) %>%
  summarise(
    T_mean_franja = mean(Int_T, na.rm = TRUE),
    .groups = "drop"
  )



#3. Calcular medias diarias externas por vivienda para calcular limite adaptativo
Vivtodas_diario_media <- Vivtodas_verano_horario2 %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),     # media diaria exterior (por vivienda)
    Int_T_mean = mean(Int_T, na.rm = TRUE),     # opcional
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



# 4. Unir la información de franja (media) con el limite adaptativo diario por vivienda ---
# Resultado intermedio: por cada dwell_numb + date + franja tenemos la T_mean_franja
# y además el limiteadap (por vivienda+date).
final_umbral_franja <- Vivtodas_franja_mean %>%
  left_join(
    Vivtodas_diario_media %>% select(dwell_numb, date, limiteadap),
    by = c("dwell_numb", "date")
  ) %>%
  mutate(
    valor_umbral = if_else(tipo_umbral == "fijo", 26, limiteadap)
  ) %>%
  # seleccionar sólo las columnas necesarias
  select(dwell_numb, date, franja, peso, tipo_umbral, valor_umbral, T_mean_franja) %>%
# eliminar los días 21/06, 22/06 y 23/06
  filter(!date %in% as.Date(c("2021-06-21", "2021-06-22", "2021-06-23")))



#5. Superación umbral
final_umbral_franja <- final_umbral_franja %>%
  mutate(
    diferencia = valor_umbral - T_mean_franja,        # diferencia entre umbral y temperatura media
    supera = if_else(T_mean_franja > valor_umbral, 1, 0)  # 1 si se supera, 0 si no
  )



#6. Calcular temperatura ponderada diaria por vivienda 
# Usamos T_mean_franja y pesos para obtener T_ponderada diaria
# También calculamos si el día supera algún umbral (sobrecalentamiento)

final_diario <- final_umbral_franja %>%
  group_by(dwell_numb, date) %>%
  summarise(
    T_ponderada = sum(T_mean_franja * peso) / sum(peso),    # temperatura ponderada diaria
    sobrecalentamiento_dia = if_else(any(supera == 1), 1, 0),           # 1 si alguna franja supera su umbral
    .groups = "drop"
  )

#Para ver cuántos 0 y 1 hay en una columna:
table(final_diario$sobrecalentamiento_dia)







