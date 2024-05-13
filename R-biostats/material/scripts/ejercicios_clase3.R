
# ----- MATRICES -----

# Ejercicio 1: modifica el código para definir una matriz `x` de ceros
# de 3 filas y 7 columnas de unos.
x <- matrix(1, nrow = 3, ncol = 7)
x

# Ejercicio 2: la matriz anterior, suma un 1 a cada número de la
# matriz y divide el resultado entre 5. Tras ello calcula su
# transpuesta y obtén sus dimensiones
new_matrix <- (x + 1)/5
t(new_matrix)
dim(new_matrix)

# Ejercicio 3: define la matriz `x <- matrix(1:12, nrow = 4)`.
# Obtén la primera fila, la tercera columna, y el elemento (4, 1).
x <- matrix(1:12, nrow = 4)
x[1, ] # primera fila
x[, 3] # tercera columna
x[4, 1] # elemento (4, 1)

# Ejercicio 4: con la matriz anterior definida como
# `x <- matrix(1:12, nrow = 4)`, calcula la media de todos los
# elementos, la media de cada fila y la media de cada columna.
# Calcula la suma de de cada fila y de cada columna
x <- matrix(1:12, nrow = 4)
mean(x) # de todos
apply(x, MARGIN = 1, FUN = "mean") # media por filas
apply(x, MARGIN = 2, FUN = "mean") # media por columnas
apply(x, MARGIN = 1, FUN = "sum") # suma por filas
apply(x, MARGIN = 2, FUN = "sum") # suma por columnas

# ----- TIBBLE -----

# Ejercicio 1: carga del paquete `{datasets}` el conjunto de datos
# `airquality` (variables de la calidad del aire de Nueva York desde
# mayo hasta septiembre de 1973).
# ¿Es el conjunto de datos airquality de tipo tibble?
# En caso negativo, conviértelo a tibble
# (busca en la documentación del paquete en <https://tibble.tidyverse.org/index.html>).
library(tibble)
class(datasets::airquality)
airquality_tb <- as_tibble(datasets::airquality)

# Ejercicio 2: una vez convertido a `tibble` obtén el nombre de las
# variables y las dimensiones del conjunto de datos.
# ¿Cuántas variables hay? ¿Cuántos días se han medido?
names(airquality_tb)
ncol(airquality_tb)
nrow(airquality_tb)

# Ejercicio 3: filtra solo los datos del mes de agosto.
airquality_tb[Month == 8, ]

# Ejercicio 4: selecciona aquellos datos que no sean ni de julio ni de agosto.
airquality_tb[Month %in% c(7, 8), ]

# Ejercicio 5: modifica el siguiente código para quedarte solo con
# las variable de ozono y temperatura.
airquality_tb[, c("Ozone", "Temp")]

# Ejercicio 6: selecciona los datos de temperatura y viento de agosto.
# Traduce a castellano el nombre de las columnas del conjunto filtrado.
airquality_tb[Month == 8, c("Temp", "Wind")]
names(airquality_tb) <- c("ozono", "rad_solar", "viento", "temp", "mes", "dia") 
