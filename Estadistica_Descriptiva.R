### Estadisitica Descriptivas ###

setwd("C:/Users/pc/Desktop/Estadistica Espacial")

# Charge packages

library(readxl)
library(tidyverse)
install.packages("mosaic")
library(mosaic)

# Charge data

data <- read_excel("Base+de+Datos++Plantas.xlsx")
attach(data)

# Descriptivos Altura

# Mode 1

favstats(Altura, data)

#Mode 2

install.packages("FSA")
library(FSA)

Summarize(Altura, data, digits = 2, na.rm = TRUE)

# Mode 3

install.packages("Hmisc")
library(Hmisc)

Hmisc::describe(Altura)

# Mode 4

install.packages("pastecs")
require(pastecs)

round(stat.desc(Altura),2)

# Mode 5

install.packages("psych")
require(psych)

psych::describe(Altura)

# Moda

install.packages("modeest") 
library(modeest)
mlv(Altura, method = "mfv")


# Histograma

ggplot(data, aes(x = Altura))+
  geom_histogram(color = "white", bins = 45) +
  scale_x_continuous(breaks=seq(100, 700, 100))

ggplot(data, aes(x = Altura))+
  geom_histogram(aes(y =..density..), color = "white", bins = 45) +
  geom_density()
  scale_x_continuous(breaks=seq(100, 700, 100))
  

# Boxplot

ggplot(data, aes(y = Altura)) +
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25)+
  geom_boxplot() +
  scale_y_continuous(breaks=seq(100, 700, 100))
 

    
# Descriptivos DAP

Summarize(DAP, data, digits = 2, na.rm = TRUE)  

round(stat.desc(DAP),2)

# Intervalo de confianza Altura

data %>%
  summarise(mean_alt = mean(Altura), sd_alt = sd(Altura), num= n()) %>%
  mutate(len = 1.96*sd_alt/sqrt(num)) %>% 
  mutate(ymin=mean_alt - len, ymax= mean_alt + len)


# Intervalo de confianza DAP

data %>%
  summarise(mean_dap = mean(DAP), sd_dap = sd(DAP), num= n()) %>%
  mutate(len = 1.96*sd_dap/sqrt(num)) %>% 
  mutate(ymin=mean_dap - len, ymax= mean_dap + len)  

# Normalidad

shapiro.test(DAP)

shapiro.test(Altura)


# Estadistica Parametrica y No parametrica

# Charge dataset

setwd("C:/Users/pc/Desktop/Estadistica Espacial")

data <- read_excel("Base_datos.xlsx", sheet = 2)

attach(data)

shapiro.test(`Abund total`)
shapiro.test(datalog)


data <- data %>% 
  mutate(datalog = log10(`Abund total`))


ggplot(data, aes(x = `Abund total`)) +
  geom_histogram()


ggplot(data, aes(x = datalog)) +
  geom_histogram(aes(y =..density..), color = "white", bins = 35) +
  geom_density()


# Data 2

data2 <- read_excel("Base_datos.xlsx", sheet = 3)

# T.test

t.test(data2$`Abund total`~ data2$`Codigo crucero`, var.equal = T)

t.test(data2$`Abund total`~ data2$`Codigo crucero`, var.equal = F)

# U Man whitney

wilcox.test(data2$`Abund total`~ data2$`Codigo crucero`)

# Data 3

data3 <- read_excel("Base_datos.xlsx", sheet = 1)


# Shapiro

shapiro.test(data3$DAB)
shapiro.test(data3$ALTURA)
shapiro.test(data3$DAB_2)
shapiro.test(data3$ALTURA_2)


## Tamaño de muestra ##

# Instalar Paquetes
  
install.packages ("pwr")


# Cargar Paquetes

library(usethis)  
library(tidyverse)
library(pwr)
library(devtools)


# Escenario 1: Muestra para estimar incrementos de una media en una misma población

  # Supuestos: 2 colas, Ho: m=m0 vs Ha: m>=m0, m=20, diff=10 (o ma=30), sd=10, p=0.05, power=0.80 
  
# Tamaño de muestra
  
pwr.t.test(d=1,sig.level=0.05, power=0.80,type="one.sample")

# Poder del estudio
  
pwr.t.test(n=25,d=1,sig.level=0.05,type="one.sample")

# Efecto de Diseño
  
pwr.t.test(n=25,sig.level=0.05, power=0.80,type="one.sample")



# Escenario 2: Muestra para estimar diferencia de medias de dos poblaciones independientes con varianzas desiguales
  
# Supuestos: 2 colas, Ho: m2=m1 vs Ha: m2>= m1, sd1=5.5, sd2=5, p=0.05, power=0.80 

# Cálculo de tamaño del efecto.

efecto_u2_u1 <- cohen.ES(test="t",size="medium") 
efecto_u2_u1

# Tamaño de muestra
  
  
pwr.t.test(d=0.5,power=0.80,sig.level=0.20,type="two.sample",alternative="two.sided") 

# poder del estudio 
  
pwr.t.test(d=0.5, n=70,sig.level=0.05,type="two.sample",alternative="two.sided")

# Efecto de Diseño
  
pwr.t.test(n=70,power=0.80,sig.level=0.05,type="two.sample",alternative="two.sided")


# Escenario 3: Muestra para estimar diferencia de medias de dos muestras dependientes con varianzas iguales
# Supuestos: 2 colas, Ho: d=d0 vs Ha: d!= d0, sd1=sd2=sd, p=0.05, power=0.80

efecto_u2p_u1p <-(0-5)/5
efecto_u2p_u1p

# Tamaño de muestra
  
pwr.t.test(d=0.5,power=0.80,sig.level=0.05,type="paired",alternative="two.sided")

# Poder del estudio
  
pwr.t.test(d=1, n=20,sig.level=0.05,type="paired",alternative="two.sided")

# Efecto de Diseño
  
pwr.t.test(n=1000,power=0.80,sig.level=0.25,type="paired",alternative="two.sided")


# Escenario 7: Muestra para análisis de tipo anova unidireccional (comparación de más de dos medias) 
# Supuestos: 2 colas, Ho: delta =0 vs Ha: delta !=0, m1=50, m2=60 m3=70, var error=900, p=0.05, power=0.80

# Cálculo Tamaño de efecto Anova

efecto_anova <- cohen.ES(test="anov",size="medium")  
efecto_anova

# Tamaño de muestra
  
pwr.anova.test(f=0.25,k=3,sig.level = 0.05,power=.80)

# Poder del estudio 
  
pwr.anova.test(n=60,k=3,sig.level = 0.25,f=0.25)

# Efecto de Diseño
  
pwr.anova.test(n=60,k=3,sig.level = 0.05,power=.80)


### Easystats Performance ####

# LIBRARIES ----

install.packages("performance", dependencies = TRUE)
# Installs all dependencies

library(tidyverse)
library(performance)

# DATA ----

mpg

# 1.0 PERFORMANCE ----

model_lm <- lm(hwy ~ displ + class, data = mpg)

model_lm

x11()

check_model(model_lm, check = "linearity")
check_model(model_lm, check = "homogeneity")
check_model(model_lm, dot_size = 0.5, line_size = 0.5)


# 2.0 TIDYMODELS ----

install.packages("tidymodels")

library(tidymodels)

# * Linear Regression ----
model_lm_tidy <- linear_reg() %>%
  set_engine("lm") %>%
  fit(hwy ~ displ + class, data = mpg)

check_model(model_lm_tidy)

x11()

#### SKIMR ###

# Install packages

library(devtools)

devtools::install_github("ropensci/skimr", ref = "develop")

library(pillar)

library(skimr)

# Data 1

data(chickwts)

str(chickwts)

skim(chickwts)

# Data 2

data(iris)

str(iris)

skim(iris)

summary(iris)

# Summary

skim(iris) %>%
  summary()

# Select columns

skim(iris, Sepal.Length, Petal.Length)

# Group data

iris %>%
  dplyr::group_by(Species) %>%
  skim()

iris %>%
  skim() %>%
  dplyr::filter(numeric.sd > 1)


# Data 3

library(tidyverse)

data("starwars")

skim(dplyr::starwars)


# Data 4

faithful %>%
  skim()


# Create function

my_skim <- skim_with(numeric = sfl(mad))
my_skim(iris, Sepal.Length)

my_skim <- skim_with(
  numeric = sfl(
    iqr = IQR,
    p01 = ~ quantile(.x, probs = .01)
    p99 = ~ quantile(., probs = .99)
  ),
  append = FALSE)

my_skim(iris, Sepal.Length)


my_skim <- skim_with(numeric = sfl(hist = NULL))
my_skim(iris, Sepal.Length)


#### Change Color Interactive ####

install.packages("colourpicker")

library(colourpicker)

library(tidyverse)

# Data 1

data(iris)

CPCOLS <- c("#1FB55B", "#73351D", "#B5BF45")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = Species), size = 3) +
  scale_color_manual(values = CPCOLS) +
  theme_bw()

# Data 2

CPCOLS <- c("#1F24B5", "#827A40", "#6467B3")

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(aes(col = as.factor(cyl))) +
  scale_colour_manual(values = CPCOLS)

#### Explore Data ####

install.packages("explore")

library(explore)

# Explore interactive 

data(iris)

explore(iris)

# Generate report

iris |> report(output_dir = tempdir())

# Report con parametro objetivo

iris |> report(output_dir = tempdir(), target = Species)

# define a target (is Species versicolor?)
iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
iris$Species <- NULL

# create report
iris |> report(output_dir = tempdir(),
               target = is_versicolor,
               targetpct = TRUE)

# Exploration manual

# load packages
library(explore)

# use iris dataset
data(iris)

# explore Species
iris |> explore(Species)

# explore Sepal.Length
iris |> explore(Sepal.Length)

# define a target (is Species versicolor?)
iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)

# explore relationship between Sepal.Length and the target
iris |> explore(Sepal.Length, target = is_versicolor)

# explore relationship between all variables and the target
iris |> explore_all(target = is_versicolor)

# explore correlation between Sepal.Length and Petal.Length
iris |> explore(Sepal.Length, Petal.Length)

# explore correlation between Sepal.Length, Petal.Length and a target
iris |> explore(Sepal.Length, Petal.Length, target = is_versicolor)

# describe dataset
describe(iris)

# describe Species
iris |> describe(Species)

# explain target using a decision tree
iris$Species <- NULL
iris |> explain_tree(target = is_versicolor)

# explain target using a logistic regression
iris |> explain_logreg(target = is_versicolor)

### More functions ###

# load packages
library(tibble)
library(explore)

# use titanic dataset
# n = number of observations
titanic <- as_tibble(Titanic)

# describe data
describe(titanic)

# describe Class
titanic |> describe(Class, n = n)

# explore Class
titanic |> explore(Class, n = n)

# explore relationship between Class and the target
titanic |> explore(Class, n = n, target = Survived)

# explore relationship between all variables and the target
titanic |> explore_all(n = n, target = Survived)

# explain target using a decision tree
titanic |> explain_tree(n = n, target = Survived)

### More functions 2 ###

# create dataset and explore it
data <- create_data_app(obs = 1000)
explore(data)

data <- create_data_buy(obs = 1000)
explore(data)

data <- create_data_churn(obs = 1000)
explore(data)

data <- create_data_person(obs = 1000)
explore(data)

data <- create_data_unfair(obs = 1000)
explore(data)

# create random dataset with 100 observarions and 5 random variables
# and explore it
data <- create_data_random(obs = 100, vars = 5)
explore(data)

# create your own random dataset and explore it
data <- create_data_empty(obs = 1000) |> 
  add_var_random_01("target") |> 
  add_var_random_dbl("age", min_val = 18, max_val = 80) |> 
  add_var_random_cat("gender", 
                     cat = c("male", "female", "other"), 
                     prob = c(0.4, 0.4, 0.2)) |> 
  add_var_random_starsign() |> 
  add_var_random_moon()

explore(data)

# create an RMarkdown template to explore your own data
# set output_dir (existing file may be overwritten)
create_notebook_explore(
  output_dir = tempdir(),
  output_file = "notebook-explore.Rmd")


