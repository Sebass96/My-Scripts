
#Representaciones gráficas

cafe <- read.csv("Datos de café.csv", header=TRUE, sep = ";")
attach(cafe)
head(cafe)

#histograma de frecuencias
hist(FDA)
hist(Proteina, col = rainbow(10),
     xlab = "Fibra Detergente Neutro",
     ylab = "Frecuencias",
     main = "Histograma para FDN")
#boxplot
boxplot(Proteina ~ Periodo.de.recogida)
boxplot(cafe) # no es recomendable si no todas las variables son numericas y estan en la misma 
                 #unidad

#Scatter plot
plot(Proteina ~ FDA)
linearmodel <- lm(Proteina ~ FDA)
abline(linearmodel)

#agregar colores según periodo poscosecha
with(subset(cafe, Periodo.de.recogida == "< 24 h"),
     points(FDA, Proteina, col = "red"))
with(subset(cafe, Periodo.de.recogida == "de 24 a 48 h"),
     points(FDA, Proteina, col = "blue"))
with(subset(cafe, Periodo.de.recogida == "> 48h"),
     points(FDA, Proteina, col = "green"))

pairs(~Proteina+FDN+FDA+ELN)

#utilizar el paquete lattice
install.packages("lattice")
library(lattice)
bwplot(~cafe$Proteina | cafe$Periodo.de.recogida,
       main = "% Protein",
       xlab = "Periodo poscosecha - beneficio")
bwplot(~cafe$Proteina | cafe$Variedad,
       main = "Proteina vs variedad",
       xlab = "variedad")
# grafico de judias
install.packages("beanplot")
library(beanplot)
beanplot(cafe$Proteina ~ cafe$Lugar, col = c("blue", "red", "yellow"))

