### Exploratory analysis with ggplot2 ###

# Charge data #

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)  #String: toma cadenas como factores

# To avoid any extra work

my_titanic <- my_titanic[complete.cases(my_titanic), ]

library(tidyverse)

as.tibble(my_titanic)

str(my_titanic)

class(my_titanic)

# Using numerical variables

my_titanic$Survived <- factor(my_titanic$Survived)

my_titanic$Pclass <- factor(my_titanic$Pclass)

glimpse(my_titanic)


# Line

mydata_r <- my_titanic %>% 
  group_by(Pclass) %>% 
  count(Survived)

ggplot() + 
  geom_line(data = mydata_r, aes(x = Pclass, y = n, color = Survived))

# Solution 1: Change as. numeric

mydata_r$Pclass <- as.numeric(mydata_r$Pclass)

ggplot() + 
  geom_line(data = mydata_r, aes(x = Pclass, y = n, color = Survived))


# Dots

ggplot(data = mydata_r, aes(x = Pclass, y = n, color = Survived)) + 
  geom_point()

# Together

ggplot(data = mydata_r, aes(x = Pclass, y = n, color = Survived)) + 
  geom_point() +
  geom_line()

# Solution 2: group

mydata_r

ggplot(data = mydata_r, aes(x = Pclass, y = n, color = Survived)) + 
  geom_line() +
  geom_point()

# Solution 

ggplot(data = mydata_r, aes(x = Pclass, y = n, group = Survived)) + 
  geom_line() +
  geom_point()


### Barplot ###

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)

head(my_titanic)

attach(my_titanic)

attach?

str(my_titanic)

# To avoid any extra work

my_titanic <- my_titanic[complete.cases(my_titanic), ]

# Change as.factor

is.factor(Pclass)

my_titanic$Pclass <- factor(my_titanic$Pclass)

str(my_titanic)

# Graphic

ggplot() + geom_bar(data = my_titanic, aes(x = Pclass)) #geom_bar:realiza un conteo

# adding terms

ggplot(data = my_titanic, aes(x = Pclass)) + 
  geom_bar(stat = "count") +  # identity: asigna un peso al eje y (numero exacto del eje y)
  labs(title = "Comparison by Sex and Passenger Class",
       x = "Pclass",
       y = "Count by class")

# Use other categorical variable

ggplot(data = my_titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(stat = "count") +  
  labs(title = "Comparison by Sex and Passenger Class",
       x = "Pclass",
       y = "Count by class")

# Creating two plots in one

ggplot(data = my_titanic, aes(x = Pclass)) + 
  geom_bar(stat = "count") +  
  labs(title = "Comparison by Sex and Passenger Class",
       x = "Pclass",
       y = "Count by class") +
  facet_wrap(vars(Sex))


# Creating two rows of plot by sex

ggplot(data = my_titanic, aes(x = Pclass)) + 
  geom_bar(stat = "count") +  
  labs(title = "Comparison by Sex and Passenger Class",
       x = "Pclass",
       y = "Count by class") +
  facet_grid(vars(Sex))

# Dodge

ggplot(data = my_titanic, aes(x = Pclass, fill = Sex)) + 
  geom_bar(stat = "count", position = "dodge") +  
  labs(title = "Comparison by Sex and Passenger Class",
       x = "Pclass",
       y = "Count by class")

## Geomcol

#geombar(stat = "identity") = geom_col

my_data <- my_titanic %>% 
  count(Pclass)

ggplot(data = my_data, aes(x = Pclass, y =n))+
  geom_col()+
  labs(title = "Comparison by Sex and Passenger Class",
       x = "Pclass",
       y = "Count by class")


## Pie chart ##

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)

head(my_titanic)
str(my_titanic)
library(tidyverse)

# Change as.factor

my_titanic$Pclass <- factor(my_titanic$Pclass)

str(my_titanic)

# Graphic

mydata <- my_titanic %>% 
  count(Pclass) %>% 
  mutate(percentage = n/ sum(n)*100,
         pos_pie = round(cumsum(percentage) - 0.5*percentage,2)) #cumsum: suma acumulada #pospie: posicion del texto

head(mydata)


ggplot(mydata)+
  geom_col(aes(x = "", y = percentage, fill = Pclass))+
  coord_polar(theta = "y")+ #coorpolar: da forma de pie
  geom_text(aes(x = "", y = pos_pie, label = scales::percent(percentage, scale = 1)))


# rewriting mydata

mydata <- my_titanic %>% 
  count(Pclass) %>% 
  arrange(desc(Pclass)) %>% 
  mutate(percentage = n/ sum(n)*100,
         pos_pie = round(cumsum(percentage) - 0.5*percentage,2))

ggplot(mydata)+
  geom_col(aes(x = "", y = percentage, fill = Pclass))+
  coord_polar(theta = "y")+ #coorpolar: da forma de pie
  geom_text(aes(x = "", y = pos_pie, label = scales::percent(percentage, scale = 1)))


## Boxplot ##

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)  #String: toma cadenas como factores

# To avoid any extra work

my_titanic <- my_titanic[complete.cases(my_titanic), ]

library(tidyverse)

as.tibble(my_titanic)

str(my_titanic)

class(my_titanic)

# Using numerical variables

my_titanic$Survived <- factor(my_titanic$Survived)

my_titanic$Pclass <- factor(my_titanic$Pclass)

glimpse(my_titanic)


# unique category

ggplot(my_titanic, aes(y = Age)) +
  geom_boxplot() +
  labs(title = "Distribution of age", y = "Age")

# using more than one category

ggplot(my_titanic, aes(x = Sex, y = Age)) +
  geom_boxplot() +
  labs(title = "Age comparison by Gender", x = "Gender", y = "Age")

# combining more than two variables

ggplot(my_titanic, aes(x = Sex, y = Age, color = Sex)) +
  geom_boxplot() +
  labs(title = "Age comparison by Gender grouped by Class", x = "Gender", y = "Age") +
  facet_wrap(vars(Pclass))

## Violin ##

# unique category

ggplot(my_titanic, aes(x = Age, y = Age)) +
  geom_violin() +
  labs(title = "Distribution of age", y = "Age")

# using more than one category

ggplot(my_titanic, aes(x = Sex, y = Age)) +
  geom_violin() +
  labs(title = "Age comparison by Gender", x = "Gender", y = "Age")

# combining more than two variables

ggplot(my_titanic, aes(x = Sex, y = Age, fill = Pclass)) +
  geom_violin() +
  labs(title = "Age comparison by Gender grouped by Class", x = "Gender", y = "Age") 

## Dotplot

ggplot(my_titanic, aes(x = Pclass, y = Age)) +
  geom_dotplot(binaxis = "y", stackdir = "center",
               binwidth = 1.2, fill = "lightgray")


# Dotplot + boxplot

ggplot(my_titanic, aes(x = Pclass, y = Age)) +
  geom_boxplot(width = 0.5) +
  geom_dotplot(binaxis = "y", stackdir = "center",
               binwidth = 1.2, fill = "lightgray")



## Scatterplot ##

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)  #String: toma cadenas como factores

# To avoid any extra work

my_titanic <- my_titanic[complete.cases(my_titanic), ]

library(tidyverse)

as.tibble(my_titanic)

str(my_titanic)

# Using numerical variables

my_titanic$Survived <- factor(my_titanic$Survived)

my_titanic$Pclass <- factor(my_titanic$Pclass)

glimpse(my_titanic)

# Graphic

ggplot(my_titanic, aes(x = Age, y = Fare)) +
  geom_point() +
  labs(title = "Comparison by Age and Fare",
       x = "Age",
       y = "Fare")


ggplot(my_titanic, aes(x = Age, y = Fare)) +
  geom_point(aes(color = Pclass)) +
  labs(title = "Comparison by Age and Fare",
       x = "Age",
       y = "Fare",
       caption = "Source: My own design")


# Mtcars data #

data("mtcars")

str(mtcars)

# Basic scatter plot

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# coloring dots by category

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(am))) +
  geom_point() +
  labs(title = "Scatterplot weight vs Miles per gallon",
       x = "weight",
       y = "Miles per gallon")


# Change the point size and shape

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 4, shape = 23)

# Using other variable

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(size = qsec), shape = 23) # size = qsec : los puntos aumentan su tamaño en torno a la variable qsec

## Scatterplot for regression

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = lm)

# Remove the confidence interval

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# method: lm, glm, gam, loess(predeterminado), rlm 

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth()


## Histogram ##

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)  #String: toma cadenas como factores

# To avoid any extra work

my_titanic <- my_titanic[complete.cases(my_titanic), ]

library(tidyverse)

as.tibble(my_titanic)

str(my_titanic)

# Using numerical variables

my_titanic$Survived <- factor(my_titanic$Survived)

my_titanic$Pclass <- factor(my_titanic$Pclass)

glimpse(my_titanic)

# Graphic

ggplot(my_titanic, aes(x = Age)) +
  geom_histogram() +
  labs(title = "Age distribution",
       x = "Age",
       y = "Frequency")

# Change color

ggplot(my_titanic, aes(x = Age)) +
  geom_histogram(color = "black", fill = "white") +
  labs(title = "Age distribution",
       x = "Age",
       y = "Frequency")

# changing bin's width

ggplot(my_titanic, aes(x = Age)) +
  geom_histogram(color = "black", fill = "white", binwidth = 5) +
  labs(title = "Age distribution",
       x = "Age",
       y = "Frequency")

# bins:cantidad de barras
# binwidth:ancho de barras

library(ggthemes)

# comparing categories

ggplot(my_titanic, aes(x = Age)) +
  geom_histogram(aes(fill = Sex), bins = 10, position = "dodge") +
  labs(title = "Age distribution",
       x = "Age",
       y = "Frequency")+
  theme_fivethirtyeight()

# add mean line

ggplot(my_titanic, aes(x = Age)) +
  geom_histogram(fill = "green") +
  geom_vline(aes(xintercept = mean(Age)),
             color = "blue", linetype = "dashed", size = 1)

# Density plot #

# One single feature

ggplot(my_titanic, aes(x = Age)) +
  geom_density() +
  labs(title = "Age distribution",
       x = "Age", y = "Frequency") +
  theme_excel()

# one single feature by category

ggplot(my_titanic, aes(x = Age)) +
  geom_density(aes(fill = Sex)) +
  labs(title = "Age distribution",
       x = "Age", y = "Frequency") +
  theme_excel()


# using facet

ggplot(my_titanic, aes(x = Age)) +
  geom_density(aes(fill = Sex)) +
  labs(title = "Age distribution",
       x = "Age", y = "Frequency") +
  facet_grid(vars(Sex)) +
  theme_excel()

# Histogram and density plot

ggplot(my_titanic, aes(x = Age)) +
  geom_histogram(aes(y =..density..), color = "black", fill = "white", binwidth = 7) +
  geom_density(alpha = 0.2, fill = "orange")

## QQplot - normality

ggplot(my_titanic, aes(sample = Age)) +
  stat_qq()

ggplot(my_titanic, aes(sample = Age)) +
  stat_qq(aes(color = Sex)) +
  labs(y = "Age")


## Advanced plots with ggplot2 ##

my_titanic <- read.csv("titanic.csv", stringsAsFactors = T)  #String: toma cadenas como factores

# To avoid any extra work

my_titanic <- my_titanic[complete.cases(my_titanic), ]

library(tidyverse)

as.tibble(my_titanic)

str(my_titanic)

# Using numerical variables

my_titanic$Survived <- factor(my_titanic$Survived)

my_titanic$Pclass <- factor(my_titanic$Pclass)

glimpse(my_titanic)

# Indicating position of mean with geom_pint

ggplot(my_titanic, aes(x = Survived, y = Age)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1) +
  stat_summary(fun = "mean", colour = "red", size = 5, geom = "point") +
  stat_summary(fun = "median", colour = "blue", size = 5, geom = "point")

# with barplot

ggplot(my_titanic, aes(x = Pclass)) +
  geom_bar()

# However adding stat_summary

ggplot(my_titanic, aes(x = Pclass)) +
  stat_summary(aes(y = Age), fun = "mean", geom = "bar") #promedio de edad por clase

# Using lines

ggplot(my_titanic, aes(x = Pclass, y = Fare, group = Sex, color = Sex)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line")

ggplot(my_titanic, aes(x = Pclass, y = Age, group = Sex, color = Sex)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line")

# Standard errors

mean_se(my_titanic$Age, mult = 1) #mult: z = 1 (numero de desv estandar)
mean_se(my_titanic$Age[my_titanic$Survived == 0], mult = 1)

ggplot(my_titanic, aes(x = Pclass, y = Age)) +
  stat_summary(aes(y = Age), fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "blue") 

# For survived 

ggplot(my_titanic, aes(x = Survived, y = Age)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar")

# Modify desv standar

ggplot(my_titanic, aes(x = Survived, y = Age)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", fun.args = list(mult = 1.96)) # 1.96: 95% de confianza

## Text in graphics

library(tidyverse)

data(mpg)

head(mpg)
str(mpg)

ggplot(mpg) +
  geom_bar(aes(x = manufacturer))

# Option 1

ggplot(mpg) +
  geom_bar(aes(x = manufacturer)) +
  coord_flip() # cordflip: invierte los ejes

# Option 2

ggplot(mpg) +
  geom_bar(aes(x = manufacturer)) +
  theme(axis.text.x = element_text(angle = 45))

# Text in bars

ggplot(mpg, aes(x = manufacturer)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", color = "blue", vjust = -1) +
  theme(axis.text.x = element_text(angle = 45))

# with geom_col

ggplot(mpg %>% count(manufacturer), aes(x = manufacturer, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.2) +
  theme(axis.text.x = element_text(angle = 45))

# Annotate text

#Solution 1

ggplot(mpg) +
  geom_boxplot(aes(y = cty)) +
  geom_text(x = 0.1, y = 35, label = "Must be an outlier")

#Solution 2

ggplot(mpg) +
  geom_boxplot(aes(y = cty)) +
  annotate(geom = "text", x = 0.05, y = 35, label = "Outlier",
           color = "red")

ggplot(mpg) +
  geom_boxplot(aes(y = cty)) +
  annotate(geom = "text", x = rep(0.05, 4), y = c(27.5,29.5,33,35), 
           label = c("Outlier","Outlier", "Outlier", "Outlier"),
           color = "red")

# Labeling points

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_text(aes(label = model))

# modify

ggplot(mpg, aes(x = cty, y = hwy, label = model)) +
  geom_point() +
  geom_text(size = 2)

ggplot(mpg, aes(x = cty, y = hwy, label = model)) +
  geom_point() +
  geom_text(size = 2) +
  geom_smooth(method = lm)

ggplot(mpg, aes(x = cty, y = hwy, label = model)) +
  geom_point() +
  geom_label()

# Using less points

summary(mpg$displ)

ggplot(mpg[mpg$displ > 4.6, ], aes(x = cty, y = hwy, label = model)) +
  geom_point(color = "red")

ggplot(mpg[mpg$displ > 4.6, ], aes(x = cty, y = hwy, label = model)) +
  geom_point(color = "red") +
  geom_label()

# Still overlapped

library(ggrepel)

ggplot(mpg[mpg$displ > 4.6, ], aes(x = cty, y = hwy, label = model)) +
  geom_point(color = "red") +
  ggrepel::geom_label_repel()

ggplot(mpg[mpg$displ > 4.6, ], aes(x = cty, y = hwy, label = model)) +
  geom_point(color = "red") +
  ggrepel::geom_text_repel()


# Using colors for categories

ggplot(mpg[mpg$displ > 4.6, ], aes(x = cty, y = hwy)) +
  geom_point() +
  geom_label_repel(aes(label = model, fill = factor(class)), size = 3) +
  ggtitle("Miles per gallon Comparrison",
          subtitle = "City and Highway") +
  labs(x = "City (mpg)", y = "Highway (mpg)",
       caption = "Source: ggplot2", fill = "Class") +
  theme(legend.position = "bottom")


## Time Series ##

library(gapminder)
data("gapminder")

str(gapminder)

library(tidyverse)

# plot 1: scatterplot with line

gapminder %>% filter(country == "Colombia")

ggplot(data = gapminder %>% filter(country == "Colombia"), aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Life Expectancy", title = "Evolution of life expectancy in Colombia")

# plot 2: scatterplot with line correcting format of axis

ggplot(data = gapminder %>% filter(country == "Colombia"), aes(x = year, y = pop)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia")

# fixing format y axis

require(scales)

ggplot(data = gapminder %>% filter(country == "Colombia"), aes(x = year, y = pop)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia")

# Adding values in the plot

ggplot(data = gapminder %>% filter(country == "Colombia"), aes(x = year, y = pop)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia") +
  geom_text(aes(label = pop), size = 3, position = position_stack(vjust = 1.1))

# Two countries

ggplot(data = gapminder %>% filter(country == "Colombia" | country == "Peru"), aes(x = year, y = pop, color = country)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia and Ecuador")
  
# Many lines

ggplot(data = gapminder %>% filter(continent == "Americas"), aes(x = year, y = pop, color = country)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia and Ecuador")

# sub set

south_america <- c("Argentina", "Ecuador", "Colombia", "Peru")

ggplot(data = gapminder[gapminder$country %in% south_america, ],
       aes(x = year, y = pop, color = country)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Comparision of the evolution of population")

# combine last plots in one simple

# Create subset with true o false

alp <- gapminder[gapminder$continent == "Americas", ]$country %in% south_america

# True: los paises que estan dentro del south_america | False: los que no estan

# let's plot it

ggplot(data = gapminder %>% filter(continent == "Americas"),
       aes(x = year, y = pop, color = country, alpha = alp)) + #alpha: pone en segundo plano los paises False en alp
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Comparision of the evolution of population in America") +
  guides(color = "none") + #quita la leyenda
  labs(alpha = "South")

# modify limits axis y

ggplot(data = gapminder %>% filter(continent == "Americas"),
       aes(x = year, y = pop, color = country, alpha = alp)) + #alpha: pone en segundo plano los paises False en alp
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Comparision of the evolution of population in America") +
  guides(color = "none") + #quita la leyenda
  labs(alpha = "South") +
  scale_y_continuous(limits = c(0, 5e+7))


## Example 2

data("sunspot.month")

str(sunspot.month)

#convert it to dataframe

library(zoo)

#v1

suns_p <- data.frame(Y = as.matrix(sunspot.month), date = as.Date(sunspot.month))
#as.matrix = convierte a dataframe
#as.Date = convierte en fechas

ggplot(suns_p, aes(x = date, y = Y)) +
  geom_line(color = "red") +
  labs(title = "Sunspots 1749 - 2014",
       x = "date",
       y = "Number of sunspots")

# changing format to x - axis

ggplot(suns_p, aes(x = date, y = Y)) +
  geom_line(color = "red") +
  labs(title = "Sunspots 1749 - 2014",
       x = "date",
       y = "Number of sunspots") +
  scale_x_date(limits = c(as.Date("2000-01-01"), NA), date_labels = "%m-%y") #%d: day, %a: weekday, %m: month, %b: abbrev month, %y: 2dig year


# display for into

ggplot(suns_p[3150:3177, ], aes(x = date, y = Y)) +
  geom_line(color = "red") +
  labs(title = "Sunspots 1749 - 2014",
       x = "date",
       y = "Number of sunspots") +
  scale_y_continuous(labels = as.character(suns_p$date), breaks = suns_p$date) +
  theme(axis.text.x = element_text(angle = 30))


## Two or more plots in one figure ##

library(gapminder)
data("gapminder")

str(gapminder)

library(tidyverse)
library(cowplot)
require(scales)

# save in one object

p1 <- ggplot(data = gapminder %>% filter(country == "Colombia")) +
  geom_point(aes(x = pop, y = lifeExp)) +
  labs(title = "Population vs Life expectancy",
       x = "Population") +
  scale_x_continuous(labels = comma)

p2 <- ggplot(data = gapminder %>% filter(country == "Colombia")) +
  geom_point(aes(x = pop, y = gdpPercap)) +
  labs(title = "Population vs Gross domestic\n product per capita", #title issues use: \n
       x = "Population") +
  scale_x_continuous(labels = comma)

## Ploting both graph in the same figure

plot_grid(p1,p2, labels = c("A", "B"))

# some options: label_size, label_colour

## number of columns ncol= NULL

plot_grid(p1,p2, labels = c("A", "B"), ncol = 1)

plot_grid(p1,p2, labels = c("A", "B"), ncol = 2)

# adding a general title

p1 <- ggplot(data = gapminder %>% filter(country == "Colombia")) +
  geom_point(aes(x = pop, y = lifeExp), color = "red") +
  labs(title = "Life expectancy",
       x = "Population") +
  scale_x_continuous(labels = comma)

p2 <- ggplot(data = gapminder %>% filter(country == "Colombia")) +
  geom_point(aes(x = pop, y = gdpPercap), color = "blue") +
  labs(title = "Gross domestic product per capita", 
       x = "Population") +
  scale_x_continuous(labels = comma)

p <- plot_grid(p1, p2, labels = c("A", "B"))

title <- ggdraw() + 
  draw_label("Evolution of the population in Colombia vs other indicators", fontface = "bold")

#The relative widths and heights of rows and columns can be adjusted (ajustar margenes)
#with the rel_widths and rel_heights arguments
# rel_heights values control vertical title margins

plot_grid(title, p, ncol = 1, rel_heights = c(0.1,1,1))

# what if have legends in common

two_data <- gapminder %>% 
  filter(country == "Colombia" | country == "Peru")

p1 <- ggplot(two_data) +
  geom_point(aes(x = pop, y = lifeExp, color = country)) +
  labs(title = "Life expectancy",
       x = "Population",
       y = "Life expectancy") +
  scale_x_continuous(labels = comma)

p2 <- ggplot(two_data) +
  geom_point(aes(x = pop, y = gdpPercap, color = country)) +
  labs(title = "Gross domestic product per capita",
       x = "Population",
       y = "Life expectancy") +
  scale_x_continuous(labels = comma)

legend <- get_legend(
  p1 + guides(color = guide_legend(nrow = 1))+
    theme(legend.position = "bottom"))


plot_row <- plot_grid(p1 + theme(legend.position = "none") + xlab(NULL),
                      p2 + theme(legend.position = "none") + xlab(NULL),
                      labels = c("A", "B"))

title <- ggdraw() + 
  draw_label("Evolution of the population in Colombia vs other indicators", fontface = "bold")

plot_grid(title, plot_row, legend, ncol = 1, rel_heights = c(0.1,1))


## Analysis with ggplot HEXAGON ##

library(gapminder)
data("gapminder")

str(gapminder)

library(tidyverse)

ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_point()

ggplot(gapminder, aes(x = lifeExp,)) +
  geom_histogram()

#Hexagon (Scatterplot + Histogram)

ggplot(gapminder, aes(x = lifeExp, y = gdpPercap)) +
  geom_hex()

# bins: numeric vector giving number of bins in both vertical and horizontal directions. Set to 30 by default

ggplot(gapminder %>% filter(continent == "Americas"), aes(x = lifeExp, y = gdpPercap)) +
  geom_hex(bins = 10)

# Customized colors

ggplot(gapminder %>% filter(continent == "Americas"),
       aes(x = lifeExp, y =gdpPercap)) +
  geom_hex(bins = 10) +
  scale_fill_gradient(low ="lightblue1", high = "darkblue", trans = "log10")


## Example 2

data(mpg)
head(mpg)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_hex(bins = 10) +
  scale_fill_gradient(low ="lightblue1", high = "darkblue", trans = "log10")

# adding options

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_hex(bins = 10, aes(color = drv)) +
  scale_fill_viridis_c()

# Example 3

data("faithful")
str(faithful)

# Graphic

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_hex() +
  scale_fill_viridis_c()

# Labeling

library(ggthemes)

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_hex(bins = 10) +
  stat_bin_hex(aes(label = ..count..), geom = "text", bins = 10, colour = "white") +
  theme_stata() +
  ggtitle("Eruptions vs waiting time")

### Plotly ###

#Charge data

library(tidyverse)
data("mpg")
head(mpg)

# scatterplot in ggplot

p <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# Converting to plotly

library(plotly)

ggplotly(p)

# Boxplot in ggplot

p1 <- ggplot(mpg, aes(y = hwy)) +
  geom_boxplot()

# Converting to plotly

ggplotly(p1)

p2 <- ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  labs(title = "f = front - wheel drive, r = rear wheel drive, 4 = 4wd")

ggplotly(p2)

# Second method

#Boxplot

plot_ly(data = mpg, x = ~ class, y = ~ hwy, type = "box")

#Scatterplot

plot_ly(data = mpg, x = ~ displ, y = ~ hwy, type = "scatter", mode = "markers")

# markers: se usa en scatterplot

# linear regression

lm1 <- lm(data = mpg, hwy ~ displ)

y_fit <- predict(lm1, dataframe = displ)

xy <- data.frame(displ = mpg$displ, hwy = y_fit)

fig <- plot_ly(data = mpg, x = ~ displ, y = ~ hwy, type = "scatter", mode = "markers", alpha = 0.65, name = "disp-hwy")
fig <- fig %>% add_trace(data = xy, x = ~ displ, y = ~ hwy, name = "Regresion - fit", mode = "lines", alpha = 1)
fig

#Histogram

plot_ly(mpg, x = ~ cyl, color = ~ class, colors = "Accent", type = "histogram")


## GGplot2 y segundo eje ##

library(ggplot2)

data("economics")

head(economics)

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(size = 1) +
  labs(title = "Unemplloyment")

ggplot(economics, aes(x = date, y = pop)) +
  geom_line(size = 1) +
  labs(title = "Evolution of population (US)")

ggplot(economics, aes(x = date, y = unemploy/ pop)) +
  geom_line(size = 1) +
  labs(title = "Ratio of unemployment per population")

ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy), size = 1, color = "red") +
  geom_line(aes(y = pop), size = 1, color = "blue") +
  labs(title = "ratio of unemployment per population")

# no se visualiza bien porque tienen diferentes escalas en el eje y


# Añadiendo segundo eje y #

ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy), size = 1, color = "red") +
  geom_line(aes(y = pop/30), size = 1, color = "blue") + # se divide entre 30 por la relacion entre los ejes
  labs(title = "Comparision unemployment and population", y = "Unemployment") +
  scale_y_continuous(sec.axis = sec_axis(~./30, name = "Population(millions)"))

# Second example

library(gapminder)

library(tidyverse)

data("gapminder")
str(gapminder)

ggplot(gapminder %>% filter(country == "Mexico"), aes(x = year)) +
  geom_line(aes(y = lifeExp, colour = "Life Exp"), size = 1) +
  geom_line(aes(y = gdpPercap/100, colour = "GDP Per Cap"), size = 1) +
  labs(title = "Comparison life expectation and Gdp Per Capita: Mexico", y ="Life Expectancy") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "GDP per capita (x100 USD)")) +
  scale_colour_manual(values = c("blue", "red")) +
  theme(legend.position = c(0.2,0.8))










