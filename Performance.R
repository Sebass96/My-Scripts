## Modelos de regresion ##

# Load packages

#install.packages("performance")
library(performance)
library(tidyverse)

# Citation

citation("performance")

# Calidad del modelo R2

data(mtcars)

# Example 1

model <- lm(mpg ~ wt + cyl, data = mtcars)
r2(model)

# Example 2

model <- glm(am ~ wt + cyl, data = mtcars, family = binomial)
r2(model)

# Example 3

library(MASS)
data(housing)

model <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
r2(model)

# Example 4 Bayesian R2

set.seed(123)
install.pa
library(rstanarm)

model <- stan_glmer(
  Petal.Length ~ Petal.Width + (1 | Species),
  data = iris,
  cores = 4
)

r2(model)


# Example 4 Mixed models

library(lme4)

model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
r2(model)


# Coeficiente de correlación intraclase (ICC)

# Similar a R-cuadrado, el ICC brinda información sobre la varianza explicada y puede interpretarse como 
#"la proporción de la varianza explicada por la estructura de agrupación en la población" (Hox 2010).

library(lme4)

model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
icc(model)

# Sobredimension

# La sobredispersión ocurre cuando la varianza observada en los datos es mayor que la varianza esperada de la suposición del modelo

library(glmmTMB)
data(Salamanders)
model <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_overdispersion(model)


# Inflacion Cero

# La inflación cero (en los modelos (cuasi) de Poisson) se indica cuando la cantidad de ceros observados 
# es mayor que la cantidad de ceros pronosticados, por lo que el modelo no se ajusta a los ceros.

model <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_zeroinflation(model)


# Modelos singulares 

# Un ajuste de modelo "singular" significa que algunas dimensiones de la matriz de varianza-covarianza 
# se han estimado como exactamente cero. 
#Esto ocurre a menudo en modelos mixtos con estructuras de efectos aleatorios demasiado complejas.

library(lme4)
data(sleepstudy)

# prepare data

set.seed(123)
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$mygrp == i
  sleepstudy$mysubgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}

# fit strange model

model <- lmer(
  Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
  data = sleepstudy
)

check_singularity(model)

# Heteroscedasticidad

data(cars)
model <- lm(dist ~ speed, data = cars)

check_heteroscedasticity(model)

# Visualizacion

# defining a model

model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)

# checking model assumptions

dev.new()

check_model(model)


# Resumen de rendimiento del modelo

# Modelo linear

m1 <- lm(mpg ~ wt + cyl, data = mtcars)
model_performance(m1)

# Regresion logistica

m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
model_performance(m2)

# Modelo Mixto linear

library(lme4)
m3 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
model_performance(m3)

# Comparacion de modelos

counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
m4 <- glm(counts ~ outcome + treatment, family = poisson())

compare_performance(m1, m2, m3, m4, verbose = FALSE)

# Índice general de rendimiento del modelo

compare_performance(m1, m2, m3, m4, rank = TRUE, verbose = FALSE)

# Visualización de índices de desempeño de los modelos

plot(compare_performance(m1, m2, m4, rank = TRUE, verbose = FALSE))
