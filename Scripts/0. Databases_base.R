#CREAR BASE DE DATOS HORARIA DE VERANO TODAS LAS VIVIENDAS ==========================================================================

#Importar las bases de datos: interior("ClimateReady_10_min_interval.txt") y exterior ("Ext_2021_2022")

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
                     month %in% c(6, 7, 8) & city == "Pamplona" & room == "Salon")

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
Vivtodas_verano_horario <- viv_verano %>%
  group_by(year, month, day, hour, dwell_numb) %>%
  summarise(
    Int_T = mean(Int_T, na.rm = TRUE),
    Int_RH = mean(Int_RH, na.rm = TRUE), 
    Ext_T = mean(Ext_T, na.rm = TRUE),
    Ext_HR = mean(Ext_HR, na.rm = TRUE), 
    Ext_RAD = mean(Ext_RAD, na.rm = TRUE)
  ) %>%
  ungroup()



#Nueva variable: grados_hora por dia por encima del 26ºC

#: Calcular la variable: Grado-horas >26°C
library(tidyr)
library(lubridate)

Vivtodas_verano_horario <- Vivtodas_verano_horario %>%
  mutate( grados_hora = pmax(Int_T - 26, 0)  # si está por debajo de 26, cuenta como 0
  )


#EXPORTAR DATASET
install.packages("writexl")
library(writexl)
write_xlsx(Vivtodas_verano_horario, "C:/Users/ainhoa.arriazu/Desktop/Vivtodas_verano_horario.xlsx")



#===================================================================================================




# Agrupar todas las variables por dia (MEDIA)
Vivtodas_diario_media <- Vivtodas_verano_horario %>%
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

write_xlsx(Vivtodas_diario_media, "C:/Users/ainhoa.arriazu/Desktop/Vivtodas_diario_media.xlsx")




#===================================================================================================


# Agrupar todas las variables por dia (MÁXIMA)
Vivtodas_diario_máxima <- Vivtodas_verano_horario %>%
  group_by(year, month, day, dwell_numb) %>%
  summarise(
    Int_T = max(Int_T, na.rm = TRUE),
    Int_RH = max(Int_RH, na.rm = TRUE), 
    Ext_T = max(Ext_T, na.rm = TRUE),
    Ext_HR = max(Ext_HR, na.rm = TRUE), 
    Ext_RAD = max(Ext_RAD, na.rm = TRUE), 
    grados_hora = sum(grados_hora, na.rm = TRUE)
  ) %>%
  ungroup()

write_xlsx(Vivtodas_diario_máxima, "C:/Users/ainhoa.arriazu/Desktop/Vivtodas_diario_máxima.xlsx")
