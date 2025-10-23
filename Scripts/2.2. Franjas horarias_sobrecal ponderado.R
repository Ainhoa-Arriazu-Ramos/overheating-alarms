#Importar base de datos: Vivtodas_verano_horario==========================================================

#Cargar librerías
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#1. Limpieza inicial======================================================================================

#Quitar grados hora
Vivtodas_verano_horario <- Vivtodas_verano_horario %>% select(-grados_hora)

#Quitar las entradas del año 2021 (nos quedamos solo con 2022, verano extremo)
#Se deja: Año 2022 (Junio, Julio y Agosto)
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  filter(year != 2021)

#Crear columna de fecha
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate(date = make_date(year, month, day))


#===============================================================================================================
#DESARROLLO: calcular si la temperatura de cada franja supera los limites determinados (fijos o adaptativos)
#===============================================================================================================

#1. Definir las franjas horarias, sus pesos y tipo de límite ===============================================
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
    hour %in% 0:7   ~ 3,     # noches
    hour %in% 8:11  ~ 1,
    hour %in% 12:15 ~ 2,
    hour %in% 16:19 ~ 2,
    hour %in% 20:23 ~ 1.5
  ),
  limite = case_when(
    hour %in% 0:3   ~ "fijo",
    hour %in% 4:7   ~ "fijo",
    hour %in% 20:23 ~ "fijo",
    TRUE             ~ "adaptativo"  # resto de franjas
  )
)

#Asignar franja a cada observación horaria
Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  left_join(franjas, by = "hour")


#2. Crear dataset con medias por franja y por vivienda===========================================================
T_franja_mean <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date, franja, limite) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),  # hace media de todas las variables numéricas
    .groups = "drop"
  )


#3. Crear limite adaptativo diario===========================================================

Vivtodas_diario <- Vivtodas_verano_horario %>%
  group_by(dwell_numb, date) %>%
  summarise(
    Ext_T_mean = mean(Ext_T, na.rm = TRUE),     # media diaria exterior (por vivienda)
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

Vivtodas_diario <- Vivtodas_diario %>%
  mutate(limite = "adaptativo")



#4. Asignar limites a las franjas ===========================================================

T_franja_mean_limit <- T_franja_mean %>%
  left_join(
    Vivtodas_diario %>% select(dwell_numb, date, limiteadap),
    by = c("dwell_numb", "date")
  ) %>%
  mutate(
    limite_tipo = case_when(
      franja %in% c("Noche_0_3", "Noche_4_7", "Noche_20_23") ~ "fijo",
      TRUE ~ "adaptativo"
    ),
    limite_valor = case_when(
      limite_tipo == "fijo"       ~ 26,
      limite_tipo == "adaptativo" ~ limiteadap
    )
  ) %>%
  select(-limiteadap)  # eliminar columna temporal

  T_franja_mean_limit <- T_franja_mean_limit %>%
  select(-hour)
  
  T_franja_mean_limit <- T_franja_mean_limit %>% drop_na()


  
#5. Calcular si la temperatura supera el límite en cada franja ===========================================================
  T_franja_mean_limit <- T_franja_mean_limit %>%
    mutate(
      deltaT = limite_valor - Int_T,
      alarma = if_else(deltaT < 0, 1, 0)
    )

  T_franja_mean_limit %>%
    count(alarma) #Está bastante balanceado
  