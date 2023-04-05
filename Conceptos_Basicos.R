### Conceptos basicos ###

# Lectura de datos de la web #

library(tidyverse)

# Example 1

berkley <- read_csv('http://bit.ly/barkley18', comment = '#')

# Lectura de datos de tablas HTML #

library(rvest)

# Example 1: All Tables

all_tables <-
  read_html("https://en.wikipedia.org/wiki/Aviation_accidents_and_incidents") %>%
  html_table(fill = TRUE, header = TRUE)

# Example 2: Select Table

out_table <-
  all_tables %>%
  magrittr::extract2(2)

# Example 3

url <- 'http://en.wikipedia.org/wiki/World_population'

tbls <- read_html(url) %>%
  html_table(fill = TRUE, header = TRUE)

tbl <- tbls %>%
  magrittr::extract2(6)

# Mode and Class #

d <- as.Date("2010-03-15")

mode(d)

length(d)

class(d)

# Eliminacion de NULL #

lst <- list("Moe", NULL, "Curly")
lst

compact(lst)

# Eliminacion de elementos de la lista #

# Example 1

lst <- list(NA, 0, NA, 1, 2)

lst %>%
  discard(is.na)

# Example 2

lst <- list(3, "dog", 2, "cat", 1)

lst %>%
  discard(is.character)

# Funcion por fila #

fun <- function(a, b, c) {
  sum(seq(a, b, c))
}

# Example 1

df <- data.frame(mn = c(1, 2, 3),
                 mx = c(8, 13, 18),
                 rng = c(1, 2, 3))

df %>%
  rowwise %>% # Use rowwise para operar fila por fila
  mutate(output = fun(a = mn, b = mx, c = rng))

df %>%
  mutate(output = fun(a = mn, b = mx, c = rng))

# Aplicar una funcion a un grupo de datos

df <- tibble(
  my_group = c("A", "B","A", "B","A", "B"),
  values = 1:6
)

df %>%
  group_by(my_group) %>%
  summarize(
    avg_values = mean(values),
    tot_values = sum(values),
    count_values = n()
  )

# Crear nueva columna basada en condiciones

df <- data.frame(vals = 1:5)

df %>%
  mutate(new_vals = case_when(vals <= 2 ~ "2 or less",
                              vals > 2 & vals <= 4 ~ "2 to 4",
                              TRUE ~ "over 4"))

# Fecha actual #

Sys.Date()

format(Sys.Date(), format = "%m/%d/%Y")

# Generar numeros aleatorios #

# Example 1

runif(1, min = -3, max = 3)      # One uniform variate between -3 and +3

rnorm(1)                         # One standard Normal variate

rnorm(1, mean = 100, sd = 15)    # One Normal variate, mean 100 and SD 15

rbinom(1, size = 10, prob = 0.5) # One binomial variate

rpois(1, lambda = 10)            # One Poisson variate

rexp(1, rate = 0.1)              # One exponential variate

rgamma(1, shape = 2, rate = 0.1) # One gamma variate

# Example 2

rnorm(3, mean = c(-10, 0, +10), sd = 1)

means <- rnorm(30, mean = 0, sd = 0.2)
rnorm(30, mean = means, sd = 1)
