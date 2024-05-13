# Ejercicio 1: echa un vistazo a la tabla `table4b` del paquete `{tidyr}`. 
# ¿Es tidydata? En caso negativo, ¿qué falla?
# ¿Cómo convertirla a tidy data en caso de que no lo sea ya?
table4b |>
  pivot_longer(cols = "1999":"2000", names_to = "year",
               values_to = "cases")

# Ejercicio 2: echa un vistazo a la tabla `relig_income` del paquete `{tidyr}`.
# ¿Es tidydata? En caso negativo, ¿qué falla?
# ¿Cómo convertirla a tidy data en caso de que no lo sea ya?
relig_income |>
  pivot_longer(cols = "<$10k":"Don't know/refused",
               names_to = "income",
               values_to = "people")

# Ejercicio 3: echa un vistazo a la tabla `billboard` del paquete `{tidyr}`.
# ¿Es tidydata? En caso negativo, ¿qué falla?
# ¿Cómo convertirla a tidy data en caso de que no lo sea ya?
billboard |>
  pivot_longer(cols = "wk1":"wk76",
               names_to = "week",
               names_prefix = "wk",
               values_to = "position",
               values_drop_na = TRUE)