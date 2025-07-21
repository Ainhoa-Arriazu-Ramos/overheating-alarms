#CREAR LA BASE DE DATOS============================

#Importar las bases de datos: interior("ClimateReady_10_min_interval") y exterior ("Ext_2021_2022")

#Copiar dataset
viv_todo <- ClimateReady_10_min_interval

#Reordenar columnas
#Borrar columnas no necesarias
viv_todo <- viv_todo[, !(names(viv_todo) %in% c("tout", "co2", "tg", "ta2", "pm25"))]
viv_todo <- viv_todo[, c("10_minute_interval", "year", "month", "day", "hour",
                             "city", "dwell_numb", "room", "dwelling", 
                             "ta1", "rh")]

#Fusionamos la base de datos interior con exterior
# Convertir fechas
viv_todo$`10_minute_interval` <- as.POSIXct(viv_todo$`10_minute_interval`, format = "%Y-%m-%d %H:%M:%S")
Ext_2021_2022$Fecha <- as.POSIXct(Ext_2021_2022$Fecha, format = "%d-%m-%Y %H:%M")

# Fusionar por columna de fecha
viv_todo_completo <- merge(viv_todo, Ext_2021_2022, by.x = "10_minute_interval", by.y = "Fecha", all.x = TRUE)

# Borramos los dias que no tengan variables exteriores
viv_todo_completo <- na.omit(viv_todo_completo)

#Extraemos solo el verano
viv_verano <- subset(viv_todo_completo, 
                     month %in% c(5, 6, 7, 8, 9) & city == "Pamplona" & room == "Salon")

viv_verano <- viv_verano[, !(names(viv_verano) %in% c("10_minute_interval", "dwelling"))]

#Convertir en numero el contenido de esas columnas
viv_verano$dwell_numb <- as.numeric(viv_verano$dwell_numb)
viv_verano$Ext_T <- as.numeric(viv_verano$Ext_T)
viv_verano$Ext_RAD <- as.numeric(viv_verano$Ext_RAD)

#Rename
library(dplyr)
viv_verano <- viv_verano %>%
  rename(Int_RH = rh)
viv_verano <- viv_verano %>%
  rename(Int_T = ta1)

#Combinamos para sacar las variables horariamente

viv_verano_horario <- viv_verano %>%
  group_by(year, month, day, hour, dwell_numb) %>%
  summarise(
    Int_T = mean(Int_T, na.rm = TRUE),
    Int_RH = mean(Int_RH, na.rm = TRUE), 
    Ext_T = mean(Ext_T, na.rm = TRUE),
    Ext_HR = mean(Ext_HR, na.rm = TRUE), 
    Ext_RAD = mean(Ext_RAD, na.rm = TRUE)
  ) %>%
  ungroup()



#SEVERIDAD DEL SOBRECALENTAMIENTO: grados_hora por dia por encima del 26ºC============================

#1: Calcular la variable: Grado-horas >26°C
library(dplyr)
library(tidyr)
library(lubridate)

viv_verano_horario <- viv_verano_horario %>%
  mutate( grados_hora = pmax(Int_T - 26, 0)  # si está por debajo de 26, cuenta como 0
  )

#2: Agrupar todas las variables por dia
viv_verano_diario <- viv_verano_horario %>%
  group_by(year, month, day, dwell_numb) %>%
   summarise(
    Int_T = mean(Int_T, na.rm = TRUE),
    Int_RH = mean(Int_RH, na.rm = TRUE), 
    Ext_T = mean(Ext_T, na.rm = TRUE),
    Ext_HR = mean(Ext_HR, na.rm = TRUE), 
    Ext_RAD = mean(Ext_RAD, na.rm = TRUE), 
    grados_hora = sum(grados_hora, na.rm = TRUE)
  ) %>%
  ungroup()

#3: Variables desfasadas
viv_verano_diario_7 <- viv_verano_diario %>%
  arrange(dwell_numb, year, month, day) %>%  # ordenar por vivienda y tiempo
  group_by(dwell_numb) %>%
  mutate(
    Int_T_1 = lag(Int_T, 1),
    Int_T_2 = lag(Int_T, 2),
    Int_T_3 = lag(Int_T, 3),
    Int_T_4 = lag(Int_T, 4),
    Int_T_5 = lag(Int_T, 5),
    Int_T_6 = lag(Int_T, 6), 
    Int_T_7 = lag(Int_T, 7),
    
    Ext_T_1 = lag(Ext_T, 1),
    Ext_T_2 = lag(Ext_T, 2),
    Ext_T_3 = lag(Ext_T, 3),
    Ext_T_4 = lag(Ext_T, 4),
    Ext_T_5 = lag(Ext_T, 5),
    Ext_T_6 = lag(Ext_T, 6), 
    Ext_T_7 = lag(Ext_T, 7)
  ) %>%
  ungroup()

#4: Quitar filas con entradas NA
viv_verano_diario_7 <- na.omit(viv_verano_diario_7)


#GUARDAR EL DATASET EN EXCEL
# Instala el paquete si no lo tienes
install.packages("writexl")

# Carga el paquete
library(writexl)

write_xlsx(viv_verano_diario_7, "C:/Users/ainhoa.arriazu/Desktop/Vivreales_Tdiario_20212022_7.xlsx")


