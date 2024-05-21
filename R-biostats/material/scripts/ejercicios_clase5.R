# Ejercicio 1: El dataset `who` que hemos usado en ejercicios anteriores,
# expórtalo a un formato nativo de `R` en la carpeta `datos` del proyecto
library(tidyr)
save(who, file = "./datos/who.RData")

# Ejercicio 2: carga el dataset `who` pero desde la carpeta de datos
# (importa el archivo creado en el ejercicio anterior)
load("./datos/who.RData")

# Ejercicio 3: repite lo mismo (exportar e importar) en 4 formatos:
# `.csv`, `.xlsx`, `.sav` (spss) y `.dta` (stata)
# csv
library(readr)
write_csv(who, file = "./datos/who.csv")
who_data <- read_csv(file = "./datos/who.csv")

# excel
library(openxlsx)
write.xlsx(who, file = "./datos/who.xlsx")
who_data <- read_xlsx(path = "./datos/who.xlsx")

# sas y stata
library(haven)
write_sav(who, path = "./datos/who.sav")
who_data <- read_spss(path = "./datos/who.sav")

write_dta(who, path = "./datos/who.dta")
who_data <- read_dta(path = "./datos/who.dta")

# Ejercicio 4: repite la carga del `who.csv` pero solo selecciona
# ya en la carga las 4 primeras columnas
who_select <-
  read_csv(file = "./datos/who.csv",
           col_select = c("country", "iso2", "iso3", "year"))
