#Importar base de datos: Vivtodas_verano_horario==========================================================

library(dplyr)
library(lubridate)

# Paso 1: Definir franjas horarias con pesos y tipo de umbral
franjas <- tibble(
  hour = 0:23,
  franja = c(rep("Noche_1", 4),     # 0-3
             rep("Noche_2", 4),     # 4-7
             rep("Mañana", 4),      # 8-11
             rep("Mediodia", 4),    # 12-15
             rep("Tarde", 4),       # 16-19
             rep("Noche_3", 4)),    # 20-23
  peso = c(rep(3, 8),   # Noche_1 y Noche_2
           rep(1, 4),   # Mañana
           rep(2, 4),   # Mediodia
           rep(2, 4),   # Tarde
           rep(1.5, 4)),# Noche_3
  tipo_umbral = c(rep("fijo", 8), rep("adaptativo", 12), rep("fijo", 4))
)

# Paso 2: Juntar franjas al dataset horario
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(hour = hour(datetime)) %>%   # Aseguramos columna de hora
  left_join(franjas, by = "hour")

# Paso 3: Calcular temperatura ponderada diaria por vivienda
Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date = as_date(datetime), franja, peso, tipo_umbral) %>%
  summarise(T_mean_franja = mean(T_int, na.rm = TRUE), .groups = "drop") %>%
  group_by(dwell_numb, date) %>%
  summarise(
    T_ponderada = sum(T_mean_franja * peso) / sum(peso),
    .groups = "drop"
  )

# Paso 4: Calcular umbral adaptativo por vivienda
Vivtodas_diario <- Vivtodas_diario %>%
  group_by(dwell_numb) %>%
  arrange(date) %>%
  mutate(
    Ext_T_1 = lag(T_ponderada, 1),
    Ext_T_2 = lag(T_ponderada, 2),
    Ext_T_3 = lag(T_ponderada, 3),
    trm = (1 - 0.8) * (Ext_T_1 + 0.8 * Ext_T_2 + 0.8^2 * Ext_T_3),
    limite_adapt = (0.33 * trm) + 21.8
  ) %>%
  ungroup()

# Paso 5: Asignar umbral diario (mínimo entre fijo y adaptativo según franja)
Vivtodas_diario <- Vivtodas_diario %>%
  mutate(
    limite_fijo = 26,
    T_alerta = pmin(limite_adapt, limite_fijo) # depende de si quieres el más restrictivo
  )
