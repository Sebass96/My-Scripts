# Animated grafic

library(tidyverse)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)

#Data

library(gapminder)
data("gapminder")

head(gapminder)

# Grafic

gapminder %>% 
  group_by(year, continent) %>% 
  summarize(mean_life = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = mean_life, color = continent)) +
  geom_line()

# Animated

# One

gapminder %>% 
  group_by(year, continent) %>% 
  summarize(mean_life = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = mean_life, color = continent)) +
  geom_line() +
  transition_reveal(year)

# Two

gapminder %>% 
  group_by(year, continent) %>% 
  summarize(mean_life = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = mean_life, color = continent)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Esperanza de vida en {frame_along}",
       x = "Fecha",
       y = "Años de vida")+
  theme_minimal() +
  transition_reveal(year)

# Data manipulate

gapminder %>% 
  filter(country == "Mexico", year == 2002)

gapminder %>% 
  filter(lifeExp <= 40,
         year == 2002)

# Count

gapminder %>% 
  filter(continent == "Asia",
         year == 2007) %>% 
  summarise(conteo = n())


# Summarise

gapminder %>% 
  group_by(year) %>%
  summarise(prom_vid = mean(lifeExp))
  

# Example Mtcars

data(mtcars)

dim(mtcars)

head(mtcars)

summary(mtcars)

library(tidyverse)

glimpse(mtcars)

# Classify variables

# cyl -> ordinal
# vs -> categorical -> factor
# am -> categorical -> factor
# gear -> ordinal
# carb -> ordinal

attach(mtcars)

mtcars <- as.tibble(mtcars)

data <- mtcars %>% 
  mutate_at(c("vs", "am"), as.factor) %>% 
  mutate_at(c("cyl", "gear", "carb"), ordered)

str(data)

data %>% 
  summarise_if(is.numeric, mean, na.rm = T)

data.frame(lapply(lapply(data, is.na), sum))

data %>% 
  count(gear) %>% 
  arrange(desc(n))


## GGplot 2 ##

library(ggplot2)
library(tidyverse)

data("economics")

head(economics)
summary(economics)
str(economics)

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(size = 1) +
  labs(title = "Unemployment")

ggplot(economics, aes(x = date, y = pop)) +
  geom_line(size = 1) +
  labs(title = "Evolution of population")

# Add two axis y

ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy), size = 1, color = "red") +
  geom_line(aes(y = pop/30), size = 1, color = "blue") +   # 30 es la proporcion entre las escalas
  scale_y_continuous(sec.axis = sec_axis(~./30, name = "Population(millions)"))


# Second example #

library(gapminder)

data("gapminder")
head(gapminder)

ggplot(gapminder %>% filter(country == "Mexico"), aes(x = year)) +
  geom_line(aes(y = lifeExp, colour = "Life Exp"), size = 1) +
  geom_line(aes(y = gdpPercap/100, colour = "GDP Per Capita"), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "GPD per Capita (x100 USD)")) +
  scale_colour_manual(values = c("blue", "red")) +
  theme(legend.position = c(0.2, 0.8))























