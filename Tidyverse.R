### Data wrangling

# It is the process of cleaning and unifying messy and complex data sets for easy access and analysis
# Data wrangling is the process of gathering, selecting, and transforming data to answer and analytical 

#tibble#

library(tidyverse)

install.packages("wordcloud")
install.packages("formattable")
install.packages("leaflet")

data(mpg)
library("tibble")
mpg

#dataframe to tibble

data("iris")
head(iris)
class(iris)

iris <- as_tibble(iris)

iris

#creating subsets

#one way without dplyr

library("gapminder")
data(gapminder)
gapminder[gapminder$country == "Colombia" | gapminder$country == "Peru", ]

#using dplyr

gapminder %>% 
  filter(country == "Colombia" | country =="Peru") #filter is for rows

#from library dplyr

# the package dpylr provides convenient tools for the most common data manipulations tasks

gapminder %>% 
  subset(country == "China") #subset == filter

china <- gapminder %>% 
  filter(country == "China")

#selecting and grouping

china <- china %>% 
  select(year, lifeExp, pop, gdpPercap) #select is for columns

# summarize and group by

gapminder %>% select(country, continent, gdpPercap) %>% 
  group_by(country) %>% 
  summarize(counting = n(), median_gdpPercap = median(gdpPercap))

gapminder %>% 
  summarise_if(is.numeric, mean, na.rm =T)

gapminder %>% 
  summarise_all(mean, na.rm =T)

# More examples

library(tidyverse)

gapminder %>% 
  select(country, lifeExp) %>% 
  filter(country == "Mexico")

gapminder %>% 
  select(country, lifeExp) %>% 
  filter(lifeExp > 80)

gapminder %>% 
  group_by(country) %>% 
  summarise(n = n())

#counting

gapminder %>% 
  count(continent)

gapminder %>% 
  count(country, continent)

gapminder %>% 
  count(continent, country)

gapminder %>% 
  distinct(country, continent) %>% # unica vez, contar esta pareja
  count(continent)

gapminder %>% 
  distinct(country) %>% 
  count(country)

gapminder %>% 
  distinct(country) %>% 
  summarise(number = n())

# group by and arrange

gapminder %>% group_by(country) %>% 
  summarise(life_avg = mean(lifeExp), life_sd = sd(lifeExp)) %>% 
  arrange(desc(life_avg))   #ordernar descendente

# filtering

gapminder %>% select(country, year, pop) %>% 
  filter(pop > 100000000)

# read and run by parts

gapminder %>% select(country, year, pop) %>% 
  filter(pop > 100000000) %>% 
group_by(country) %>% 
  summarise(max_pop = max(pop)) %>% 
  arrange(desc(max_pop))


# country with more population

gapminder %>% filter(pop == max(pop))

gapminder %>% group_by(country) %>% 
  filter(pop == max(pop), pop > 100000000) %>% 
  arrange(desc(pop))

gapminder %>% group_by(country) %>% 
  filter(pop == max(pop)) %>% 
  group_by(continent) %>% 
  summarise(total_continental = sum(pop))


#visualization with ggplot

gapminder %>% group_by(continent, country) %>% 
  filter(pop == max(pop)) %>% 
  ungroup(country) %>% 
  summarise(total_continental = sum(pop)) %>% 
  ggplot(aes(x = continent, y = total_continental))+
  geom_col()+
  labs(title = "Population by continent",
       x = "Continent",
       y = "Population")

# Example 2

china <- gapminder %>% 
  filter(country == "China")

china %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_line() +
  labs(title = "China's life expectancy per year",
       x = "Year",
       y = "Life expectancy")

# same plot in one single step

ggplot(data = gapminder %>% filter(country == "Bolivia"),
       aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_line() +
  labs(title = "Bolivia's life expectancy per year",
       x = "Year",
       y = "Life expectancy")

gapminder %>% 
  filter(lifeExp >= 80) %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_line(aes(color = country)) +
  labs(title = "Paises con Esperanza de vida superior a 80 a?os",
       x = "A?o",
       y = "Esperanza de vida")+
  ggrepel::geom_text_repel(aes(label = country))

## Mutate

#Create new variables

# example 1 using mean

gapminder %>% group_by(country) %>% 
  mutate(ch_life_year = lifeExp - mean(lifeExp)) %>% 
  filter(country == "Colombia") %>% 
  ggplot(aes(x = year, y = ch_life_year)) +
  geom_point() +
  geom_line() +
  labs(title = "Cambios respecto a la media de esperanza de vida")


# example 2 using lag

gapminder %>% group_by(country) %>% 
  mutate(lag_pop = lag(pop))  #saca el valor anterior

gapminder %>% group_by(country) %>% 
  mutate(lag_pop = lag(pop)) %>% 
  mutate(ch_pop = (pop-lag_pop)/pop*100) %>% 
  filter(country == "Colombia") %>% 
  ggplot(aes(x = year, y = ch_pop)) +
  geom_point() +
  geom_line() +
  labs(title = "Cambios porcentuales de poblacion a?o a a?o")

# More options

gapminder %>% 
  filter(continent == "Americas") %>% 
  mutate(latinoamerica = ifelse(country != "United States" | country!= "Canada", 1,0))


#More mutates

gapminder %>% 
  mutate_if(is.double, as.integer) #if: si cumple con la premisa

#powerful moves with mutate_at

data(mtcars)

mtcars <- as.tibble(mtcars)
str(mtcars)
glimpse(mtcars)

mtcars %>% 
  mutate_at(c("cyl", "vs", "am", "gear", "carb"), as.factor)

# Applying a function

scala <- function(x, na.rm = F) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

mtcars %>% 
  mutate_at(c("mpg", "disp", "drat", "wt", "qsec"), scala)

mtcars %>% 
  mutate_all(scala)

# save as factor first

mtcars <- mtcars %>% 
  mutate_at(c("cyl", "vs", "am", "gear", "carb"), as.factor)

mtcars %>% 
  mutate_if(is.numeric, scala)


## Example with Starwars ##

library(tidyverse)
data("starwars")
head(starwars)
dim(starwars)

# select information

starwars %>% 
  select(name,sex,homeworld,species)

starwars %>% 
  filter(name == "Yoda")

#descriptive statistics 

starwars %>% 
  group_by(homeworld) %>% 
  summarise(height_m = mean(height), mass_m = mean(mass, na.rm = T), n = n()) %>% 
  arrange(desc(height_m))

starwars %>% 
  select(name, height, sex, homeworld, species) %>% 
  filter(homeworld == "Quermia")
         
# more than one measurement

starwars %>% 
  group_by(homeworld) %>% 
  summarise(height_m = mean(height), mass_m = mean(mass, na.rm = T), n = n()) %>% 
  filter(n > 1) %>% 
  arrange(desc(height_m))

starwars %>% 
  select(name, height, sex, homeworld, species) %>% 
  filter(homeworld == "Kashyyyk")

# oldest species

starwars %>% 
  group_by(homeworld) %>% 
  summarise(year_m = mean(birth_year, na.rm = T), n = n()) %>% 
  filter(n > 1) %>% 
  arrange(desc(year_m))

starwars %>% 
  select(name, height, sex, birth_year, homeworld, species) %>% 
  filter(homeworld == "Tatooine")

# Counting
# Count: agrupa y cuenta

starwars %>% 
  count(species) %>% 
  arrange(desc(n))

starwars %>% 
  count(homeworld) %>% 
  arrange(desc(n))

starwars %>% 
  count(sex) %>% 
  arrange(desc(n))

starwars %>% 
  count(skin_color) %>% 
  arrange(desc(n))

starwars %>% 
  count(eye_color) %>% 
  arrange(desc(n))

starwars %>% 
  summarise_if(is.numeric, mean)

starwars %>% 
  summarise_if(is.numeric, mean, na.rm = T)

starwars %>% 
  filter(height == max(height))

starwars %>% 
  filter(height == max(height, na.rm = T))

#How to unnest list in dataframe

starwars_unnested <- starwars %>% 
  unnest(films)

starwars_unnested %>% 
  group_by(name) %>% 
  count(name) %>% 
  arrange(desc(n))

# distinct : eliminate row distinct

starwars_unnested %>%
  distinct(name, height, mass, hair_color, skin_color, sex, homeworld)

starwars_unnested_ship <- starwars %>% 
  unnest(starships)

starwars_unnested_ship %>% 
  count(name) %>% 
  arrange(desc(n))
  
starwars_unnested_ship %>% 
  filter(is.character(starships))
  count(name) %>% 
  arrange(desc(n))  
  

## Pivot longer ##
  
candidate_grades <- tibble(
  candidate = c("A", "B", "C"),
  judge_1 = c(15, 19, 13),
  judge_2 = c(13, 12, 14),
  judge_3 = c(16, 11, 14)
)  

candidate_grades 

exce <- candidate_grades %>% 
  pivot_longer(
    cols = c("judge_1", "judge_2", "judge_3"),
    names_to = "judge",
    values_to = "grade"
  )

ggplot(exce, aes(x = candidate, y = grade, fill = judge)) + geom_col(position = "dodge")

ggplot(exce, aes(x = judge, y = grade, group = candidate, color = candidate)) +
  geom_line() + geom_point()

m1 <- matrix(c(
  108, 96, 110, 122,
  103, 117, 127, 133,
  96, 107, 106, 107,
  84, 85, 92, 99,
  118, 125, 125, 116,
  110, 107, 96, 91,
  129, 128, 123, 128,
  90, 84, 101, 113,
  96, 100, 103, 105,
  105, 114, 105, 112,
  113, 117, 132, 130),
  c(12,4), byrow = T)

d1 <- data.frame(m1)
colnames(d1) <- c("Months30", "Months36","Months42","Months48")
d1$ID <- as.factor(1:12)
head(d1)

d1_long <- d1 %>% 
  pivot_longer(cols = starts_with("Month"),
               names_to = "Month",
               values_to = "Values")

ggplot(d1_long, aes(x = Month, y = Values, group =10, color = 10)) +
  geom_line() + geom_point()


## Pivot Wider ##

library(tidyverse)
d1_long

d1_wide <- d1_long %>% 
  pivot_wider(names_from = Month, values_from = Values)

#Example

data(us_rent_income)

us_rent_income

us_rent_income %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

library(gapminder)

head(gapminder)

gapminder %>% 
  group_by(year) %>% 
  summarise(mean_lifexp = mean(lifeExp))

gapminder %>% 
  filter(year > 1997) %>% 
  pivot_wider(names_from = year, values_from = c(pop, lifeExp, gdpPercap), values_fill = 0)

gap_wide <- gapminder %>% 
  select(country, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp) %>% 
  summarise(across(.cols = everything(), .fns = mean))

## Database Iris

# Dataset

data(iris)
head(iris)

# Summary

iris %>% 
  group_by(Species) %>% 
  summarise_all(mean)

# Max

iris %>% 
  group_by(Species) %>%
  filter(Sepal.Length == max(Sepal.Length)) %>%
  arrange(Species)

# Other form

iris %>% 
  group_by(Species) %>% 
  slice(which.max(Sepal.Length))

# Aplica una función a cada fila de una tabla

iris %>% 
  rowwise() %>% 
  mutate(Max.Len= max(Sepal.Length,Petal.Length))

# Obtén las frecuencias relativas o proporciones

iris %>% 
  group_by(Species) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

# Elimina filas duplicadas

iris %>% 
  distinct(Species)

# Cambia el nombre de las variables

iris %>% 
  rename(SL = Sepal.Length) %>% 
  head()

# Selecciona casos completos

df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df

# Descartar todos los casos incompletos

df %>% drop_na()

# Descartar en funcion a una variable

df %>% drop_na(x)

# Suma a través de múltiples filas/columnas

df=data.frame(
  x1=c(1,0,0,NA,0,1,1,NA,0,1),
  x2=c(1,1,NA,1,1,0,NA,NA,0,1),
  x3=c(0,1,0,1,1,0,NA,NA,0,1))

df

df %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum))

df %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(.[1:3]))

# Cuenta el número de casos o filas

iris %>% 
  group_by(Species) %>% 
  count()

# Cuenta el número de casos o filas únicas (con NAs)

data=data.frame(aa=c(1,2,3,4,NA), bb=c('a', 'b', 'a', 'c', 'c')) 
data

data %>%                    
  filter(!is.na(aa)) %>%    
  group_by(bb) %>%          
  summarise(Unique_Elements = n_distinct(aa))  


## Time series

seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 month")

#class time series

ts(data = NA, start = 1, end = numeric(0), frequency = 1,
   deltat = 1, ts.eps = getOption("ts.eps"), class, names)

#year

ts(13:22, start = 1959, frequency = 1)

#month

ts(13:22, start = 1959, frequency = 12)

#quarter

ts(data = 2:48, frequency = 4, start = c(1959,2))

#day

ts(data = 1:47, start = c(1997,10), frequency = 365)

# plotting time series

xrand <- ts(data = runif(100,0,50), start = c(1997, 10), frequency = 365)

class(xrand)
time(xrand)
plot(xrand)
plot.ts(xrand)

plot(runif(100,0,50))
plot.ts(runif(100,0,50))

start(xrand)
end(xrand)
frequency(xrand)
summary(xrand)

# More examples

install.packages("tswge")

library(tswge)

# Example 1

data("wtcrude")
help(wtcrude)

wtcrude

plot(wtcrude, xlab = "Month")
lines(wtcrude)

plot.ts(wtcrude)

start(wtcrude)
end(wtcrude)
frequency(wtcrude)
summary(wtcrude)

# example 2

data("patemp")

plot(patemp, xlab = "Month")
lines(patemp)

#Example 3

library(astsa)

data(jj)

plot(jj, ylab = "Earnings")





























































































