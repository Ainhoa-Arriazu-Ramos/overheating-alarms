#IMPORTAR DATASET=======================================================================================================

dataset_tem_hourly <- Tipología11_Temp_int_hourly
rm(Tipología11_Temp_int_hourly)


dataset_tem_daily  <- dataset_tem_hourly

#MODIFICACIONES EN EL DATASET =========================================================================================

#Agrupar por días y promedio del resto de variables

install.packages("dplyr")
library(dplyr)

dataset_tem_daily$Date <- as.Date(dataset_tem_daily$Date)

dataset_tem_daily_avg <- dataset_tem_daily %>%
  group_by(Date) %>%  # Agrupar por la columna Date
  summarise(across(everything(), ~mean(. , na.rm = TRUE)))  # Calcular el promedio de todas las columnas

head(dataset_tem_daily_avg)

rm(dataset_tem_daily)

dataset_tem_daily   <-  dataset_tem_daily_avg
rm(dataset_tem_daily_avg)



#Borrar columnas/variables del dataset diario
dataset_tem_daily <- dataset_tem_daily %>%
  select(-Indoor_PB_Temp, -Indoor_P1_Temp, -Indoor_P2_Temp)


#Renombrar la columna de indoor temperature
dataset_tem_daily <- dataset_tem_daily %>%
  rename(Temp = Indoor_P3_Temp)


#Crear el dataset desplazado=====================

#Copiamoos el dataset (por si acaso)
dataset_tem_daily_ARX  <- dataset_tem_daily

#Nuevas columnas dias anteriores (temperatura interior)
dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  mutate(Temp_minus_1 = lag(Temp, n = 1))

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  mutate(Temp_minus_2 = lag(Temp, n = 2))

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  mutate(Temp_minus_3 = lag(Temp, n = 3))

#Nuevas columnas dias anteriores (temperatura exterior)
dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  mutate(Outdoor_Temperature_1_previous = lag(Outdoor_Temperature, n = 1)  )

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  mutate(Outdoor_Temperature_2_previous = lag(Outdoor_Temperature, n = 2)  )

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  mutate(Outdoor_Temperature_3_previous = lag(Outdoor_Temperature, n = 3)  )

#Eliminar las filas con NA
dataset_tem_daily_ARX <- na.omit(dataset_tem_daily_ARX)

#Renombrar columnas 
dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  rename(Out_temp_1 = Outdoor_Temperature_1_previous)

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  rename(Out_temp_2 = Outdoor_Temperature_2_previous)

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  rename(Out_temp_3 = Outdoor_Temperature_3_previous)

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  rename(In_temp_1 = Temp_minus_1)

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  rename(In_temp_2 = Temp_minus_2)

dataset_tem_daily_ARX <- dataset_tem_daily_ARX %>%
  rename(In_temp_3 = Temp_minus_3)

#EXPORTAR EL DATASET =========================================================================================
install.packages("writexl")
library(writexl)

write_xlsx(dataset_tem_daily_ARX, "C:/Users/ainhoa.arriazu/Desktop/DOCS Ainhoa/mi_dataset.xlsx")
















