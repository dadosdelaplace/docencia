
# ----- PARTE I -----

# Ejercicio 1: define una variable que guarde tu edad (llamada `edad`)
# y otra con tu nombre (llamada `nombre`)
edad <- 33
nombre <- "Javi"

# Ejercicio 2: define otra variable llamada `hermanos` que responda la
# pregunta «¿tienes hermanos?» y otra con la fecha de tu nacimiento (llamada `fecha_nacimiento`).
hermanos <- TRUE
library(lubridate)
fecha_nacimiento <- as_date("1989-09-10")

# Ejercicio 3: define otra variable con tus apellidos (llamada `apellidos`)
# y usa `glue()` para tener una sola variable `nombre_completo`
# (separando nombre y apellido por una coma)
apellidos <- "Álvarez Liébana"
glue("{nombre}, {apellidos}")

# Ejercicio 4: calcula los días que han pasado desde la fecha de tu
# nacimiento hasta hoy (con la fecha de nacimiento definida en el ejercicio 2).
today() - fecha_nacimiento

# ----- PARTE II -----

# Ejercicio 1: define el vector `x` como la concatenación de los
# 5 primeros números impares, y calcula su suma.
x <- seq(1, 9, by = 2)
sum(x)

# Ejercicio 2: obtén los elementos de `x` mayores que 4.
# Calcula el número de elementos de `x` mayores que 4.
x[x > 4]
sum(x > 4)

# Ejercicio 3: calcula el vector `1/x` y obtén la versión ordenada (de menor a mayor).
z <- 1/x
sort(z)
z[order(z)]

# Ejercicio 4: encuentra el máximo y el mínimo del vector `x`
min(x)
max(x)

# Ejercicio 5: encuentra del vector `x` los elementos
# mayores (estrictos) que 1 y menores (estrictos) que 7.
# Encuentra una forma de averiguar si todos los elementos son o no positivos.
x[x > 1 & x < 7]
all(x > 0)

# Ejercicio 6: dado el vector `x <- c(1, -5, 8, NA, 10, -3, 9)`,
# extrae los elementos que ocupan los lugares 1, 2, 5, 6.
# Elimina del vector el segundo elemento. Tras eliminarlo determina su suma y su media
x <- c(1, -5, 8, NA, 10, -3, 9)
x[c(1, 2, 5, 6)]
y <- x[-2]
sum(y, na.rm = TRUE)
mean(y, na.rm = TRUE)
