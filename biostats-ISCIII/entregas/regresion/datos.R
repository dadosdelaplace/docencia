library(tidyverse)

generar_datos <- function(semilla, n){

  # Variables relacionadas con el colesterol
  set.seed(semilla)
  Edad <- floor(rnorm(n = n, mean = 55, sd = 10))
  Edad <- if_else(Edad < 20, 20, if_else(Edad > 90, 90, Edad))
  
  # Nivel de Actividad Física: ordinal
  Nivel_Actividad <- sample(c("Baja", "Moderada", "Alta"), 
                            n, replace = TRUE, prob = c(0.35, 0.5, 0.15))
  
  # Estatura (en metros) y Peso
  Estatura <- rnorm(n, mean = 1.7, sd = 0.1)
  Estatura <-
    if_else(Estatura < 1.4, 1.4, if_else(Estatura > 2.15, 2.15, Estatura))

  Peso <- 5 + 50 * Estatura + rnorm(n, mean = 5, sd = 5)
  Peso <- if_else(Peso < 35, 35, if_else(Peso > 130, 130, Peso))
  IMC <- (5 + 35 * Estatura + rnorm(n, mean = 5, sd = 10)) /  (Estatura^2)
  
  # Consumo de grasas saturadas
  Consumo_Grasas <- sample(c("Bajo", "Moderado", "Alto"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2))
  
  # Relación cuadrática entre IMC y colesterol
  Colesterol <- 0.5 * Edad + 0.2 * (IMC^2) + 2 * (Nivel_Actividad == "Baja") +
    8 * (Consumo_Grasas == "Alto") + rnorm(n, mean = 0, sd = 0.5)
  
  # Tabaquismo
  Tabaquismo <- sample(c("Sí", "No"), n, replace = TRUE, prob = c(0.3, 0.7))
  Colesterol <- Colesterol + 8 * (Tabaquismo == "Sí")
  
  # Sexo con efecto según edad y menopausia
  Sexo <- rep(NA, n)
  Sexo[Peso > 110] <- "Hombre"
  Sexo[Peso < 40] <- "Mujer"
  
  n1 <- length(Sexo[is.na(Sexo) & Peso > 70])
  n2 <- length(Sexo[is.na(Sexo) & Peso <= 70])
  Sexo[is.na(Sexo) & Peso > 70] <-
    sample(c("Hombre", "Mujer"), n1, prob = c(0.7, 0.3), replace = TRUE)
  Sexo[is.na(Sexo) & Peso <= 70] <-
    sample(c("Hombre", "Mujer"), n2, prob = c(0.4, 0.6), replace = TRUE)

  # Antecedentes familiares
  Antecedentes <- sample(c("Sí", "No"), n, replace = TRUE, prob = c(0.2, 0.8))
  Colesterol <- Colesterol + 5 * (Antecedentes == "Sí")
  
  # Otras variables no relacionadas
  Presion_Sistolica <- runif(n, 85, 145) 
  Presion_Diastolica <- Presion_Sistolica * 0.6 + rnorm(n, mean = 0, sd = 8) 
  # Horas_Sueno <- sample(c(5, 6, 7, 8, 9, 10), n, replace = TRUE, prob = c(0.05, 0.25, 0.4, 0.2, 0.05, 0.05))
  Glucosa <- rnorm(n, mean = 120, sd = 10)
  Glucosa <- if_else(Glucosa < 60, 60, Glucosa)
  
  # Limitar el rango de colesterol entre 150 y 230
  #Colesterol <- pmin(pmax(Colesterol, 150), 230)
  
  # Crear el DataFrame
  datos <- tibble(
    Colesterol,
    Edad,
    Nivel_Actividad,
    Estatura,
    Peso,
    Consumo_Grasas,
    Tabaquismo,
    Sexo,
    Antecedentes,
    Presion_Sistolica,
    Presion_Diastolica,
    Glucosa
  )
  return(datos)
  
}

datos <- generar_datos(semilla = 2412, 2500)
datos <- datos |> janitor::clean_names()

datos <- datos |> mutate(imc = peso/(estatura^2))
modelo_saturado <- lm(data = datos, formula = colesterol ~ .)
modelo_saturado |> summary()
performance::check_collinearity(modelo_saturado)

datos <- datos |> select(-estatura)
modelo_saturado <- lm(data = datos, formula = colesterol ~ .)
MASS::stepAIC(modelo_saturado)
modelo_BIC <-
  lm(formula = colesterol ~ edad + peso + consumo_grasas + tabaquismo + 
       antecedentes + imc, data = datos)
performance::check_heteroscedasticity(modelo_BIC)


# Introducir NA
library(messy)
datos_NA <-
  datos |>
  messy::make_missing(cols = c("colesterol", "nivel_actividad", "estatura"),
                      messiness = 0.06) |> 
  messy::make_missing(cols = c("edad", "peso", "consumo_grasas", "sexo"),
                      messiness = 0.01) |> 
  messy::make_missing(cols = c("glucosa", "tabaquismo"),
                      messiness = 0.03) |> 
  rename(COLESTEROL = colesterol, nivel.ACTIVIDAD = nivel_actividad,
         Peso = peso)



# write_csv(datos_NA, file = "./datos_colesterol.csv")

