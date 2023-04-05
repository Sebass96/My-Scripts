setwd("C:/Users/Usuario/Desktop/ANALISIS ESTADISTICO EN R (INIA)/ANALISIS MULTIVARIADO")
data<-read.csv("Espectros_HSI.csv", header = T, sep = ";", stringsAsFactors = F)
data$Variedad<-factor(data$Variedad, levels = c(1,2,3), labels = c("Catimor", "Caturra", "Tipica"))
data$Lugar<-factor(data$Lugar, levels = c(1,2), labels = c("Mendoza", "Utcubamba"))                    
data$Periodo.de.Recogida<-factor(data$Periodo.de.Recogida, levels= c(1,2,3), labels = c("<24h", "de 24h a 48h", ">48h"))
data<-data[ , -c(6:81)]
summary(data)
str(data)
data<-data[, -c(4:5)]

#split/unsplit# =segmentar un dataframe
lugar1<- split(data,data$Lugar)
lugar1[[1]]
str(lugar1[[2]])
summary(lugar1[[1]])

#particion del dataframe#

install.packages("caret")
library(caret)

training.ids<- createDataPartition(data$Variedad, p=0.6, list = F)
data.modelling<- data[training.ids,]
temp<-  data[-training.ids,]
validation.ids<- createDataPartition(temp$Variedad, p=0.5, list=F)
data.validation<-temp[validation.ids,]
data.testing<- temp[-validation.ids,]

##Graficos###

setwd("C:/Users/Usuario/Desktop/ANALISIS ESTADISTICO EN R (INIA)/ANALISIS MULTIVARIADO")
cafe <- read.csv("Datos de café.csv", header = T, sep = ";")
attach(cafe)

#histograma
hist(FDA)
hist(FDA, col = rainbow(5), xlab = "Fibras",
     ylab= "Frecuencias", main= "Histograma FDA")
#boxplot
boxplot(Proteina ~ Periodo.de.recogida)

#scater plot
plot(Proteina ~ FDA)
linearmodel<- lm(Proteina ~ FDA)
abline(linearmodel)

#colores##

with(subset(cafe, Periodo.de.recogida == "< 24 h"), points(FDA, Proteina, col = "red"))
with(subset(cafe, Periodo.de.recogida == "de 24 a 48 h"), points(FDA, Proteina, col = "blue")) 
with(subset(cafe, Periodo.de.recogida == "> 48h"), points(FDA, Proteina, col = "green"))

pairs(~ Proteina+FDN+FDA+ELN)

#LETICE

install.packages("lattice")
library(lattice)
bwplot(~cafe$Proteina | cafe$Periodo.de.recogida, main="%Protein", xlab= "Periodo de cosecha ~ beneficio")
bwplot(~cafe$Proteina | cafe$Variedad, main="Proteina vs variedad", xlab= "variedad")

#Grafico de judias

install.packages("beanplot")
library(beanplot)
beanplot(cafe$Proteina ~ cafe$Variedad, col = c("blue", "red", "yellow"))

