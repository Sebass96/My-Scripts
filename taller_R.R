##Mapeo de datos##

install.packages("addinslist")
library(addinslist)
library(shinyjs)

ages<- c(23,25,25,34,40,44,14,26)
ages
names(ages)<- c("kar","lui","joe", "mark","mary","pete","lyl","josek")
ages
install.packages("hash")
library(hash)

#forma1

ex1<- hash(c("kar","lui","joe", "mark","mary","pete","lyl","josek"),
           c(23,25,25,34,40,44,14,26))
ex1
ex1[c("kar","joe")]

#forma2

mylist<- c("kar", "lui","lui","mary","mary","mark","mark","mark")
values(ex1)
keys(ex1)
values(ex1, keys=mylist)


#Limpieza y manipulacion de datos

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R")
library(readxl)
empleados<- read_excel("data.xlsx", sheet = 3, skip = 13)
x= 1+2*(1:10)
x
empleados <- empleados [, -c(x)] #remover columnas impares
year= 2005:2014
year
names(empleados)<- c("Country", year) #renombrar columns
empleados <- empleados [-c(38:43), ] #remover filas
empleados$Country[empleados$Country == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
empleados[empleados == ":"] <- NA
empleados [, -1] <-round (sapply(empleados [, -1],as.numeric),1)

###ANALISIS EXPLORATORIO DE DATOS###

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/data")
salary<- read.csv("salary.csv")
str(salary)
dim(salary)
data.class(salary)
head(salary)
library(dplyr)
glimpse(salary)
summary(salary)
summary(factor(salary$sex))
min(salary$salary)
max(salary$salary)

#Medidas de posicion

mean(salary$yd)
median(salary$yd)
mean(salary$yd, trim = 0.1) #descarta el 10% de los datos, los extremos
mean(salary$yd, na.rm = T) #descarta los valores NA

#Tabluacion de tablas

table(salary$sex)
my_table<- table(salary$sex)
prop.table(my_table) #porcentajes
table(salary$degree)
table(salary$sex, salary$degree) #tabla de contingencia
my_table<- table(salary$sex, salary$rank)
my_table
prop.table(my_table) #porcentaje del total de datos
prop.table(my_table,1) #porcentaje por filas
prop.table(my_table,2) #porcentaje por columnas

#Resumen de tablas

salary %>% select(sex,salary) %>%
  group_by(sex) %>%
  summarise(promedio= mean(salary), freq=n())

salary %>% select(rank,salary) %>%
  group_by(rank) %>%
  summarise(promedio= mean(salary), freq=n())

#Visualizacion de datos

x11()
plot(x=salary$salary, main= "Grafico de salario",
     xlab="indice", ylab="salario anual",
     col="royalblue")
plot(x=salary$yt, salary$salary, main="Grafico de salario por tiempo",
     xlab = "yt", ylab= "salario",
     col="red")
plot(salary$salary, type="b")

"p": points
"l": lines
"b": both
"c": the lines part alone of "b"
"o": both "overplotted"
"h": Histograma
"n": ninguno

#Graficos

boxplot(salary$salary, main="My boxplot")
boxplot(salary$salary ~ salary$sex) #variable numerica en funcion de la cualitativa
boxplot(salary$salary ~ salary$rank)
hist(salary$salary) #se usa breaks de Sturges
hist(salary$salary, breaks = 10, col="red")
barplot(table(salary$sex)) #informacion debe estar en tabla
barplot(table(salary$sex), horiz = T) #Horizontal
barplot(my_table, main="Rank", legend.text = c("female","male")) #barra comparativa
barplot((table(salary$sex, salary$degree)))
barplot(table(salary$sex, salary$degree),
         beside= T,
         legend.text= c("female", "male")) #barras comparativas al lado con beside
        
#Correlacion

summary(salary)
library("dplyr")
select_if(salary, is.numeric) #selecciona las variables numericas

salary %>% select_if(is.numeric)

cor(select_if(salary, is.numeric)) #correlacion de pearson #linear correlation

#Graficas

plot(salary$yt, salary$salary, main="Year vs Salary")

plot(salary$yt, salary$salary, main="Year vs Salary",
     col=as.factor(salary$sex), pch=19, cex=1.5, xlab = "Year", ylab = "Salary") #color por sexo

legend(1,35000,legend = c("Female", "Male"),
       col = c(1,2), lty = 1:2, cex = 0.8)


library(corrplot)
M <- cor(select_if(salary, is.numeric))
corrplot(M, method = "color")

#Metodo de spearman #Correlacion exponencial 

#Para variables ordinales 

cor(select_if(salary, is.numeric), method = "spearman") #para variables ordinales, ve monotonia


##Outliers y Valores perdidos

data <- data.frame(x1=c(7,2,1,NA,9),
                   x2=c(1,3,1,9,NA),
                   x3=c(NA,8,8,NA,5))

#Medidas de posicion

data

# Calculate mean suppresing NA values

mean(data$x1, na.rm = T)
median(data$x1, na.rm = T)
sd(data$x1, na.rm = T)

#Seleccionar y completar casos

library(tibble)
tibble(data)
complete.cases(data) #verifica las filas completas, sin NA

data_complete <- data[complete.cases(data), ] #retorna las filas que tienen valores completos
data_complete

is.na(data$x1) #verifica si hay valores NA

which(is.na(data$x1)) #devuelve la posiicion del valor NA

#completar datos

data$x1[is.na(data$x1)]<- mean(data$x1, na.rm = T) # completar con la media
data

#Outlayers

x <- c(32,32,32,32,32,39,39,45,47,50,51,59,61,74,75,87,87,87,105)
mean(x)
mean(x, trim = 0.2) #media con el 80% de los datos

y <- c(32,32,39,39,45,47,50,51,59,61,74,75,87)
mean(y)
boxplot.stats(x) #boxplot con estadisticas del grafico


##Codificacion de variables##

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/data")

salary<- read.csv("salary.csv")

library(dplyr)
glimpse(salary)

x11()
hist(salary$salary)

#Create categorical variable

bins <- c(-Inf,20000,30000,Inf)
salary_names<- c("low","mid", "high")

salary$salary_new <- cut(salary$salary, breaks = bins, labels=salary_names) #varibles categoricas

#Summary

summary(salary$salary) #numerica

summary(salary$salary_new) #categoria

boxplot(salary$yt ~ salary$salary_new)
summary(salary)





##Analisis exploratorio con GGplot2##

library(ggplot2)
setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T) #convierte las cadenas en factores
head(my_titanic)
attach(my_titanic)
str(my_titanic)
glimpse(my_titanic)

#Eliminar datos incompletos

my_titanic<- my_titanic[complete.cases(my_titanic),]

#Grafico de barras 
#geombar hace un conteo(frecuencia)

library(tidyverse)
is.factor(Pclass)
my_titanic$Pclass<- factor(my_titanic$Pclass)
str(my_titanic)
X11()

my_titanic %>% 
  select(Pclass) %>%
  group_by(Pclass) %>% 
  summarize(count = n())

ggplot() + geom_bar(data= my_titanic, mapping = aes(Pclass))

ggplot(data= my_titanic, mapping = aes(Pclass)) + 
  geom_bar(stat="count")+ #identity les asigna un peso a los valores
  labs(title = "comparacion del sexo y clase",
       x= "Pclas",
       y= "Count by clas")

#con dos variables categoricas 

ggplot(data= my_titanic, mapping = aes(Pclass, fill=Sex)) + 
  geom_bar(stat="count")+ #identity les asigna un peso a los valores
  labs(title = "comparacion del sexo y clase",
       x= "Pclas",
       y= "Count by clas")

ggplot(data= my_titanic, mapping = aes(Pclass, fill=Sex)) + 
  geom_bar(stat="count", position = "dodge")+ #identity les asigna un peso a los valores
  labs(title = "comparacion del sexo y clase",
       x= "Pclas",
       y= "Count by clas")

#dodge= una barra al lado de la otra
#stack= una barra arriba de otra
#fill= rellena toda la barra 

#dos plots

ggplot(data= my_titanic, mapping = aes(Pclass)) + 
  geom_bar(stat="count")+ #identity les asigna un peso a los valores
  labs(title = "comparacion del sexo y clase",
       x= "Pclas",
       y= "Count by clas")+
  facet_wrap(vars(Sex)) +
  theme_classic()

ggplot(data= my_titanic, mapping = aes(Pclass)) + 
  geom_bar(stat="count")+ #identity les asigna un peso a los valores
  labs(title = "comparacion del sexo y clase",
       x= "Pclas",
       y= "Count by clas")+
  facet_grid(vars(Sex))

#Grafico de pastel

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T)

library(dplyr)
my_titanic$Pclass<- factor(my_titanic$Pclass)

mydata<- my_titanic %>% count(Pclass) %>%
  arrange(desc(Pclass))%>% #arreglar de forma descendente
  mutate(percentage = n/sum(n)*100,
         pos_pie = round(cumsum(percentage) - 0.5*percentage,2))
mydata
X11()
ggplot(mydata)+
  geom_col(aes(x="", y= percentage, fill=Pclass))+
  coord_polar(theta = "y")+ #para dar forma de pie
  geom_text(aes(x="", y= pos_pie, label = scales::percent(percentage, scale=1)))

#Grafico de lineas y puntos

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T)
my_titanic = my_titanic[complete.cases(my_titanic),]
library(tibble)
as.tibble(my_titanic)
glimpse(my_titanic)
str(my_titanic)
class(my_titanic)
my_titanic$Survived<- factor(my_titanic$Survived)
my_titanic$Pclass<- factor(my_titanic$Pclass)
str(my_titanic)

#Grafico de lineas

library(ggplot2)
x11()
ggplot() + geom_line(mapping = aes( x= PassengerId, y= Fare), data = my_titanic)
#puntos
ggplot() + geom_point(mapping = aes( x= Age, y= Fare), data = my_titanic)

#Conteo por clase

library(dplyr)

mydata_r<- my_titanic %>%
  group_by(Pclass) %>%
  count(Survived)
mydata_r$Pclass<- as.numeric(mydata_r$Pclass) #cambiando de factor a numero
ggplot() + geom_line(data=mydata_r, mapping = aes(x=Pclass, y=n, color= Survived))

#puntos
ggplot(data=mydata_r, mapping = aes(x=Pclass, y=n, color= Survived)) + geom_point()

#puntos y lineas
ggplot(data=mydata_r, mapping = aes(x=Pclass, y=n, color= Survived)) + geom_point() + geom_line()


##GRAFICO DE CAJAS (BOXPLOT)

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T)
my_titanic = my_titanic[complete.cases(my_titanic),]
library(tibble)
as.tibble(my_titanic)
my_titanic$Survived<- factor(my_titanic$Survived)
my_titanic$Pclass<- factor(my_titanic$Pclass)

#por categorias

library(ggplot2)
x11()
ggplot(data=my_titanic, mapping= aes(x=Sex, y= Age))+
  geom_boxplot()+
  labs(title = "Age comparation",
       x="gender",
       y="Age")

#combinando mas de dos variables

ggplot(data=my_titanic, mapping= aes(x=Sex, y= Age, color= Sex))+
  geom_boxplot()+
  labs(title = "Age comparation",
       x="gender",
       y="Age")

#Separar por clases

ggplot(data=my_titanic, mapping= aes(x=Sex, y= Age, color= Sex))+
  geom_boxplot()+
  labs(title = "Age comparation",
       x="gender",
       y="Age")+
  facet_wrap(vars(Pclass))

#Comparacion por precio

ggplot(data=my_titanic, mapping= aes(x=Pclass, y= Fare, color= Pclass))+
  geom_boxplot()+
  labs(title = "Fare comparation",
       x="Pclass",
       y="Fare")

#GRAFICO DE VIOLIN

x11()
ggplot(data=my_titanic, mapping= aes(x=Sex, y= Age))+
  geom_violin()+
  labs(title = "Age comparation",
       x="gender",
       y="Age")

#por clase

ggplot(data=my_titanic, mapping= aes(x=Sex, y= Age, fill= Pclass))+
  geom_violin()+
  labs(title = "Age comparation",
       x="gender",
       y="Age")

#DOTPLOT (DISTRIBUCION POR PUNTOS)

x11()
e<- ggplot(my_titanic, aes(x=Pclass, y=Age))
e + geom_dotplot(binaxis = "y", stackdir = "center",
                 binwidth = 1.2, fill= "lightgray")
#con boxplot

e + geom_boxplot(width =0.5)+
  geom_dotplot(binaxis = "y", stackdir = "center",
                 binwidth = 1.2, fill= "lightgray")

#GREAFICO DE DISPERCION Y REGRESION

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T)
my_titanic = my_titanic[complete.cases(my_titanic),]
library(tibble)
as.tibble(my_titanic)
my_titanic$Survived<- factor(my_titanic$Survived)
my_titanic$Pclass<- factor(my_titanic$Pclass)
str(my_titanic)

#Scatterplot

library(ggplot2)
x11()
ggplot(data=my_titanic, mapping = aes(x= Age, y= Fare))+
  geom_point()+
  labs( title = "Comparaci?n por edad y precio",
        x= "Age",
        y= "Fare($)")

#Por sexo

ggplot(data=my_titanic, mapping = aes(x= Age, y= Fare))+
  geom_point(aes(col= Sex))+
  labs( title = "Comparaci?n por edad y precio",
        x= "Age",
        y= "Fare($)",
        caption = "Source: My own design")

#Por clase

ggplot(data=my_titanic, mapping = aes(x= Age, y= Fare))+
  geom_point(aes(col= Pclass))+
  labs( title = "Comparaci?n por edad y precio",
        x= "Age",
        y= "Fare($)",
        caption = "Source: My own design")

#Mtcars data

data("mtcars")
str(mtcars)
glimpse(mtcars)

#Scaterplot

ggplot(mtcars, aes(x=wt, y=mpg, color= am))+ geom_point()+
  labs(title = "Scatterplot",
       x="Weight",
       y="Miles por gallon")

#cambiando a variable binaria

ggplot(mtcars, aes(x=wt, y=mpg, color= factor(am)))+ geom_point()+
  labs(title = "Scatterplot",
       x="Weight",
       y="Miles por gallon")

#cambiando tama?o y forma

ggplot(mtcars, aes(x=wt, y=mpg))+ geom_point(size=4, shape= 23)

#usando otra variable continua

ggplot(mtcars, aes(x=wt, y=mpg))+ geom_point(aes(size=qsec), shape=22) #tama?o de puntos de acuerdo a la variable continua

#Regresion

ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(method = lm)

#sin intervalo de confianza

ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(method = lm, se=F)

#HISTOGRAMAS Y NORMALIDAD#

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T)
my_titanic = my_titanic[complete.cases(my_titanic),]
library(tibble)
as.tibble(my_titanic)
my_titanic$Survived<- factor(my_titanic$Survived)
my_titanic$Pclass<- factor(my_titanic$Pclass)
str(my_titanic)

#Histograma

library(ggplot2)
x11()
ggplot(my_titanic, aes(x=Age))+
  geom_histogram()+
  labs(title = "Distribucion de la edad",
       x="Age",
       y="Frecuency")

#cambiando el color

ggplot(my_titanic, aes(x=Age))+
  geom_histogram(color="black", fill="white")

#cambiando el tama?o de las barras

ggplot(my_titanic, aes(x=Age))+
  geom_histogram(color="black", fill="white", binwidth = 5)

#Temas y bins

install.packages("ggthemes")
library(ggthemes)
#bins--> numero de barras

#comparando categorias

ggplot(my_titanic, aes(x=Age))+
  geom_histogram(aes(fill=Sex),
                 bins=10)+
  theme_excel()

#dodge
ggplot(my_titanic, aes(x=Age))+
  geom_histogram(aes(fill=Sex),
                 bins=10, position="dodge")+ #dodge:Comparacion lado a lado
  labs(title= "Distribucion de edad",
       x="Age",
       y="Frequency")+
  ggthemes:: theme_fivethirtyeight() #theme_excel()

#Agregando la linea de mediana
ggplot(my_titanic, aes(x=Age))+
  geom_histogram(fill="green")+
  geom_vline(aes(xintercept=mean(Age)),
             color="blue", linetype="dashed",size=1)

#Densidad(%)
ggplot(my_titanic, aes(x=Age))+
  geom_density()+
  labs(title= "Distribucion de edad",
       x="Age",
       y="Frequency")+
  theme_excel()
#por categoria
ggplot(my_titanic)+
  geom_density(aes(x=Age, fill= Sex))+
  labs(title= "Distribucion de edad",
       x="Age",
       y="Frequency")+
  theme_excel()

#graficos separados
ggplot(my_titanic)+
  geom_density(aes(x=Age, fill= Sex))+
  labs(title= "Distribucion de edad",
       x="Age",
       y="Frequency")+
  facet_grid(vars(Sex))+
  theme_classic()

#Histograma de densidad
ggplot(my_titanic, aes(x=Age))+
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 7)+
  geom_density(alpha=.2, fill="#FF6666")

#QQplot- Normalidad
ggplot(my_titanic, aes(sample= Age))+ stat_qq()

#Por categoria
ggplot(my_titanic, aes(sample= Age))+ 
  stat_qq(aes(color=Sex))+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  labs(y="Age")

##Graficos avanzados##
setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = T)
my_titanic = my_titanic[complete.cases(my_titanic),]
library(tibble)
as.tibble(my_titanic)
my_titanic$Survived<- factor(my_titanic$Survived)
my_titanic$Pclass<- factor(my_titanic$Pclass)
str(my_titanic)

#indicar la posicion de la media y mediana
library(ggplot2)
x11()
ggplot(data = my_titanic, mapping = aes(x= Survived, y= Age))+
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1)+
  stat_summary(fun= "mean", colour= "red", size= 5, geom= "point")+
  stat_summary(fun= "median", colour= "blue", size= 5, geom= "point")

#usando barras
ggplot(my_titanic, aes(x= Pclass))+
  stat_summary(aes(y=Age), fun= "mean", geom= "bar")

#usando lineas
ggplot(my_titanic, aes(x= Pclass, y=Fare, group= Sex, color= Sex))+
  stat_summary(fun= "mean", geom= "point")+
  stat_summary(fun= "mean", geom= "line")

ggplot(my_titanic, aes(x= Pclass, y=Age, group= Sex, color= Sex))+
  stat_summary(fun= "mean", geom= "point")+
  stat_summary(fun= "mean", geom= "line")

##Error Standar##
mean_se(my_titanic$Age, mult=1)
mean_se(my_titanic$Age[my_titanic$Survived==0], mult=1) #Por sobrevivientes, mult z=1 (desvstandar)

ggplot(my_titanic, aes(x= Pclass, y=Age))+
  stat_summary(fun.data = "mean_se", geom= "errorbar", color= "blue")

#Con barras
ggplot(my_titanic, aes(x= Pclass, y=Age))+
  stat_summary(aes(y= Age), fun= "mean", geom= "bar")+
  stat_summary(fun.data = "mean_se", geom= "errorbar", color= "blue")
  
##Intervalos de confianza##
library(dplyr)
titanic_age<- my_titanic %>% 
  group_by(Survived) %>% 
  summarise(mean_age = mean(Age), sd_age = sd(Age), num= n()) %>%
  mutate(len = 1.96*sd_age/sqrt(num)) #se 1.96 ----> confianza del 95%

titanic_age

#plot del intervalo de confianza
x11()
f<- ggplot(titanic_age, aes(x=Survived, y=mean_age,
                            ymin=mean_age - len, ymax= mean_age + len))
f
f + geom_point(size=3.5)+
  geom_errorbar(width= 0.2)+ #tambien se puede hacer con statsumary
  labs(title ="95% Confiance interval", y="Age" )
f + geom_pointrange()

#de manera directa
x11()
ggplot(my_titanic, aes(x=Survived, y=Age))+
  stat_summary(fun.data = "mean_se", geom = "errorbar", fun.args = list(mult = 1.96)) #mult 1.96 para el IC, mas 1.96 errores estandar de la media

#usando col
f + geom_col(width = 0.5, fill="skyblue")+
  geom_errorbar(width= 0.2)

##Spiderplot##
setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/Taller R/datasets-master")
my_titanic<- read.csv("titanic.csv", stringsAsFactors = F)
my_titanic$Pclass<- factor(my_titanic$Pclass)
my_titanic$Sex<- factor(my_titanic$Sex)
str(my_titanic)

library(tidyverse)
mydata <- my_titanic %>% 
  count(Pclass) %>% 
  mutate(percentage = n/sum(n)) 
mydata

temp <- mydata[, c(1,3)] %>% 
  pivot_wider(names_from = "Pclass", values_from = "percentage") #cambiar a una sola observacion
temp

#usando geomcol
ggplot()+ geom_col(mydata, mapping=aes(x=Pclass, y=percentage))

#usando ggradar##### falta funcion##



##Texto y anotaciones en graficos##
data(mpg, package = "ggplot2")
head(mpg)
str(mpg)

#Texto en los ejes
library(ggplot2)
x11()
ggplot(mpg)+ geom_bar(aes(x=manufacturer))

#option 1
ggplot(mpg)+ 
  geom_bar(aes(x=manufacturer))+
  coord_flip()

#option 2
ggplot(mpg)+ 
  geom_bar(aes(x=manufacturer))+
  theme(axis.text.x = element_text(angle = 45)) #Girar el texto

#Text in bars

#with geom_bar

ggplot(mpg, aes(x=manufacturer)) + #mapping must be in ggplot
geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", color = "red", vjust =-1)+
  theme(axis.text.x = element_text(angle = 45))

#with geom_col 

library(dplyr)
tempo <- mpg %>% 
  count(manufacturer)
tempo

ggplot(tempo, aes(x = manufacturer, y = n))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.2) +
  theme(axis.text.x = element_text(angle = 45))

#Annotate text#

sp2 <- ggplot(mpg) + geom_boxplot(aes(y = cty))

#solution 1

sp2 + geom_text(x = 0.1, y = 35, label = "Outlier")

#solution 2

sp2 + annotate(geom = "text", x = 0.1, y = 35, label = "Outlier", color = "red")

#All the points

sp2 + annotate(geom = "text", x = rep(0.1,4), y = c(27.5,29,33,35),
               label = c("Outlier","Outlier","Outlier","Outlier"))

##Adding title and labeling axes##

ggplot(dat = mpg)+
  geom_point(aes(x = cty, y = hwy))+
  ggtitle("Milles per gallon Comparison",
          subtitle = "City and Highway")+
  labs(x = "City(mpg)", y = "Highway(mpg)", caption = "Source: ggplot2")

#Labeling points#

ggplot(mpg, aes(x = cty, y = hwy))+
  geom_point()+
  geom_text( aes(label = model))

#modify

p <- ggplot(mpg, aes(x = cty, y = hwy, label = model))+
  geom_point()

p + geom_text(size = 2)

p + geom_text(size = 2) + geom_smooth(method = lm)

p + geom_label()

#Using less points

summary(mpg$displ)

p2 <- ggplot(mpg[mpg$displ>4.6,], aes(x = cty, y = hwy, label = model))+ #4.6 for 3 quartile
  geom_point()
p2

p2 + geom_label()

#Still overlapped
library(ggrepel)

p2 + ggrepel::geom_label_repel()

p2 + ggrepel::geom_text_repel()

#using colors for categories#

ggplot(mpg[mpg$displ>4.6,], aes(x = cty, y = hwy))+ 
  geom_point() +
geom_label_repel(aes(label = model, fill = factor(class)), size = 3)

#ading final touch
ggplot(mpg[mpg$displ>4.6,], aes(x = cty, y = hwy))+ 
  geom_point() +
  geom_label_repel(aes(label = model, fill = factor(class)), size = 3)+
ggtitle("Milles per galon",
        subtitle = "City and Higthway")+
  labs(x = "City(mpg)", y = "Higtway(mpg)", caption = "Source : ggplot2",
       fill = "Class")+
  theme(legend.position = "bottom")

##Exploratory analysis with ggplot##

#Time series#

library(gapminder)
data("gapminder")
head(gapminder)
str(gapminder)
library(ggplot2)

#Scatterplot with line

ggplot(gapminder[gapminder$country == "Colombia",] , aes(x = year, y = lifeExp))+
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Life expectancy", title = "Evolution of life expectancy in Colombia")

# Scatterplot with line correcting format of axis

ggplot(gapminder[gapminder$country == "Colombia",] , aes(x = year, y = pop))+
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia")

#fixing format of y axis

library(scales)

ggplot(gapminder[gapminder$country == "Colombia",] , aes(x = year, y = pop))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia")


#adding values un the plot

ggplot(gapminder[gapminder$country == "Colombia",] , aes(x = year, y = pop))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in Colombia") +
  geom_text(aes(label = pop), size = 3, position = position_stack(vjust = 1.05))

#mulitple lines (countries)

#Two conuntries

ggplot(gapminder[gapminder$country == "Colombia" | gapminder$country == "Ecuador",] , aes(x = year, y = pop, color = country))+  # | = "o"
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Comparation of the evolution of population in Colombia and Ecuador")

#many lines

ggplot(gapminder[gapminder$continent == "Americas",] , aes(x = year, y = pop, color = country))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in America")

#sub set

south_america <- c("Argentina", "Ecuador", "Colombia", "Peru")

ggplot(gapminder[gapminder$country %in% south_america,] , aes(x = year, y = pop, color = country))+ #in = dentro de
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Comparation of the evolution of population")

# Combinate last plots

#create subset with true o false

alp <- gapminder[gapminder$continent == "Americas",]$country %in% south_america

#let?s plot it

ggplot(gapminder[gapminder$continent == "Americas",] , aes(x = year, y = pop, color = country, alpha = alp))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in America") +
  guides(color = "none")+
  labs (alpha = "South")

#modify the axis limits

ggplot(gapminder[gapminder$continent == "Americas",] , aes(x = year, y = pop, color = country, alpha = alp))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Evolution of population in America") +
  guides(color = "none")+
  labs (alpha = "South") +
  scale_y_continuous(limits = c(0, 5e+7))

#Example 2 time series#

data("sunspot.month")
str(sunspot.month)
sunspot.month

#convert it to dataframe

library(zoo)

#v1

suns_p <- data.frame(Y = as.matrix(sunspot.month), data = as.Date(sunspot.month))
head(suns_p)
str(suns_p)

#plot

ggplot(suns_p, aes(x = data, y = Y))+
  geom_line(color = "red")+
  labs(title = "Sunspost 1749 - 2014",
       x = "date", y = "Number of sunspots")+
  scale_x_date(date_labels = "%m-%Y", limits = c(as.Date("2000-01-01"), NA)) #%d day, %a weekday ...

#Display for info

ggplot(suns_p[3150:3177, ], aes(x = data, y = Y))+
  geom_line(color = "red")+
  labs(title = "Sunspost 1749 - 2014",
       x = "date", y = "Number of sunspots")+
  scale_x_continuous(labels = as.character(suns_p$data), breaks = suns_p$data)+
  theme(axis.text.x = element_text(angle = 30))

## Two or more plot in one figure ##

library(gapminder)
data("gapminder")
head(gapminder)
str(gapminder)
library(ggplot2)
library(cowplot)
library(scales)

#save it in one object

p1 <- ggplot(gapminder[gapminder$country == "Colombia",])+
  geom_point(aes(x = pop, y = lifeExp))+
  labs(title = "Population vs Life expectancy", x = "Population")+
  scale_x_continuous(labels = comma)
p1

p2 <- ggplot(gapminder[gapminder$country == "Colombia",])+
  geom_point(aes(x = pop, y = gdpPercap))+
  labs(title = "Population vs Gross domestic\n product per capita", x = "Population")+
  scale_x_continuous(labels = comma)
p2

#Plotting both graph in the same figure

plot_grid(p1,p2)

#adding a name to cach plot

plot_grid(p1,p2, labels = c("A", "B"), label_size = 15)

#Number of columns 

plot_grid(p1,p2, labels = c("A", "B"), ncol = 1)

#adding a general title

p1 <- ggplot(gapminder[gapminder$country == "Colombia",])+
  geom_point(aes(x = pop, y = lifeExp), color = "red")+
  labs(title = "Life expectancy", x = "Population", y = "Life expectancy")+
  scale_x_continuous(labels = comma)
p1

p2 <- ggplot(gapminder[gapminder$country == "Colombia",])+
  geom_point(aes(x = pop, y = gdpPercap), color = "blue")+
  labs(title = "Gross domestic per capita", x = "Population", y = "Gross domestic per capita")+
  scale_x_continuous(labels = comma)
p2

p3 <- plot_grid(p1,p2, labels = c("A", "B"))

title <- ggdraw() + draw_label("Evolution of the population in Colombia versus other indicators", fontface = "bold")

# the relative widths and heights of rows and columns can be ajusted
# with the rel_widths and rel_heights arguments
# rel_heights values control vertcal title margins

plot_grid(title, p3, ncol = 1, rel_heights = c(0.1,1,1))

# what if i have legends in comoon, colors, caegories

two_data <- gapminder[gapminder$country == "Colombia" | gapminder$country == "Peru", ]

p1 <- ggplot(two_data) + geom_point(aes(y = lifeExp, x = pop, color = country))+
  labs(title = "Life expectancy", x = "Population", y = "Life expectancy") +
  scale_x_continuous(labels = comma)
  
p2 <- ggplot(two_data) + geom_point(aes(y = gdpPercap, x = pop, color = country))+
  labs(title = "Gross domestic per capita", x = "Population", y = "Gross domestic per capita") +
  scale_x_continuous(labels = comma)  
  
legend <- get_legend(p1 + guides(color = guide_legend(nrow = 1)))+
  theme(legend.position = "bottom")

plot_row <- plot_grid(p1 + theme(legend.position = "none")+ xlab(NULL),
                      p2 + theme(legend.position = "none")+ xlab(NULL),
                        labels = c("A", "B"))


title <- ggdraw() + draw_label("Evolution of the population in Colombia vrs indicators", fontface = "bold")

plot_grid(title, plot_row, legend, ncol = 1, rel_heights = c(0.1,1,1))

##Exploratory analysis with Hexagon##

library(gapminder)
data("gapminder")
head(gapminder)
str(gapminder)
library(ggplot2)

ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_point()

ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_point()

ggplot(gapminder, aes(x = lifeExp))+
  geom_histogram()

#Hexagons (combination between histogram and scatterplot)

install.packages("hexbin")
library(hexbin)
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_hex()

#What do we add using hexagon

#you can control the size of the bins by specifying the number or bins in each direction
#bins: numeric vector giving number of bins in both vertical and horizontal directions

ggplot(gapminder[gapminder$continent == "Americas",], 
       aes(x = lifeExp, y = gdpPercap))+
  geom_hex(bins = 10)

#customized colors

ggplot(gapminder[gapminder$continent == "Americas",], 
       aes(x = lifeExp, y = gdpPercap))+
  geom_hex(bins = 10) +
  scale_fill_gradient(low = "lightblue1", high = "darkblue", trans = "log10" )

#Example 2

data(mpg)
head(mpg)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_hex(bins = 10)+
  scale_fill_gradient(low = "lightblue1", high = "darkblue", trans = "log10" )

#adding options

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_hex(bins = 10, aes(color = drv))+
  scale_fill_viridis_c()

#Example 3

data("faithful")
str(faithful)

ggplot(faithful, aes(x = eruptions, y = waiting))+
  geom_point()

#Other scale

ggplot(faithful, aes(x = eruptions, y = waiting))+
  geom_hex()+
  scale_fill_viridis_c()

ggplot(faithful, aes(x = eruptions, y = waiting))+
  geom_hex(bins = 10)

#Labeling

ggplot(faithful, aes(x = eruptions, y = waiting))+
  geom_hex(bins = 10)+
  stat_bin_hex(aes(label = ..count..), geom = "text", bins = 10, colour = "white")+
  theme_bw()+
  ggtitle("Eruptions vs waiting time")

### Plottlyyy ###
install.packages("plotly")
library(plotly)

data(mpg)
head(mpg)

p <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

#converting plotly

ggplotly(p)

#boxplot

p2 <- ggplot(mpg, aes(y = hwy)) + geom_boxplot()

ggplotly(p2)

p3 <- ggplot(mpg, aes(x = drv, y = hwy)) + geom_boxplot() + labs(title = "f = front- wheel drive, r = rear wheel drive, 4 = 4wd")

ggplotly(p3)


#second method

plot_ly(data = mpg, x = ~class, y = ~hwy, type = "box")

#Scatterplot

plot_ly(data = mpg, x = ~displ, y = ~hwy, type = "scatter", mode = "markers") #markers is for points

#lineal regression

lml <- lm(data = mpg, hwy ~ displ)
y_fit <- predict(lml, dataframe = displ) #fitted values

xy <- data.frame(displ = mpg$displ, hwy = y_fit)

fig <- plot_ly(data = mpg, x = ~displ, y = ~hwy, type = "scatter", mode = "markers", alpha = 0.65, name = "disp-hwy")
fig <- fig %>% add_trace(data = xy, x = ~displ, y = ~hwy, name = "Regression Fit", mode = "lines", alpha = 0.65 )

fig

#Histogram

plot_ly(data = mpg, x = ~cyl, color = ~class, colors = "Accent" ,type = "histogram")


### Web scraping###

library(rvest)

#reading web page

covid_mex <- read_html("https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Mexico")

library(tidyverse)

#Retrieve all information

covid_mex %>% 
  html_text()

#nodes: find one

covid_mex %>% 
  html_nodes(".mw-headline")

#different tables

covid_mex %>% 
  html_nodes(".mw-headline") %>% 
  html_text()

#paragraphs

covid_mex %>% 
  html_nodes("p") %>% 
  html_text()

#find table SALUD_reported

table1 <- covid_mex %>% 
  html_table()

class(table1)

#Find number of table than i want

table_mx <- table1[[4]]

#saving source

fuente <- table_mx[34,1]

#removing not desired values and columns

table_mx <- table_mx[-c(1,34),-6]
str(table_mx)

#reformating variables

#sub is for replace

table_mx$`Cumulative cases` <- sub(",","",table_mx$`Cumulative cases`)
table_mx$`Active cases` <- sub(",","",table_mx$`Active cases`)
table_mx$Recoveries <- sub(",","",table_mx$Recoveries)
table_mx$Deaths <- sub(",","",table_mx$Deaths)

str(table_mx)

#string to numeric

table_mx$`Cumulative cases` <- as.integer(table_mx$`Cumulative cases`)
table_mx$`Active cases` <- as.integer(table_mx$`Active cases`)
table_mx$Recoveries <- as.integer(table_mx$Recoveries)
table_mx$Deaths <- as.integer(table_mx$Deaths)

str(table_mx)

#visualizations

ggplot(table_mx, aes(x = State, y = Deaths)) +
  geom_col() + coord_flip()

ggplot(table_mx, aes(x = State, y = `Cumulative cases`)) +
  geom_col() + coord_flip()


































