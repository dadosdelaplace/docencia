
library(tidyverse)
datos <- read_csv(file = "./datos/wine.csv")
datos

# 1. Visualiza tu variable objetivo (Price)
ggplot(datos) +
  geom_boxplot(aes(x = Price)) +
  theme_minimal()
# 2. De todas las posibles predictoras (X),
# si solo pudieses usar una para una reg. lineal
# ¿cuál usarías y por qué? (piensa antes si alguna variable sobra)
datos |> 
  select(where(is.numeric)) |> 
  cor()
# 3. La que decidas usar, visualiza X vs Y. Cuantifica su 
# posible asociación lineal.
ggplot(datos) +
  geom_point(aes(x = AGST, y = Price)) +
  theme_minimal()
library(GGally)
GGally::ggpairs(datos)

cor.test(datos$AGST, datos$Price)
# 4. haz una ajuste lineal univariante e interpreta
ajuste_lineal <- lm(data = datos, formula = Price ~ -1 + AGST)
ajuste_lineal |> summary()
