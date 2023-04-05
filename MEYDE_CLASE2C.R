##########################################################
############# Intervalos de confianza ####################
##########################################################
setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/CLASE2")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/5) Estadística inferencial")
library(readxl)
contraccion <- read_excel("Contraccion.xlsx")
str(contraccion)

head(contraccion)
##########################################################
#IC para datos que provienen de una distribución normal
##########################################################
contraccion <-read.delim("clipboard")
Talara<-subset(contraccion,DISTRITO=="Talara")
Chinchero<-subset(contraccion,DISTRITO=="Chinchero")
Olmos<-subset(contraccion,DISTRITO=="Olmos")
#####################################
#Intervalo de confianza para la media
#####################################
hist(Talara$CV2)
qqnorm(Talara$CV2)
x11()
t.test(Talara$CV2,conf.level = 0.96)$conf
t.test(Talara$CV2,conf.level = 0.97)$conf

#El intervalo que va de 12.98 a 13.70 brinda un 96% 
# de confianza de contener al ingreso media en el 
#coeficiencte de variabilidad 2 de las especies de la Talara

########################################
#Intervalo de confianza para la varianza
########################################
hist(Chinchero$CV2)
qqnorm(Chinchero$CV2)
library(EnvStats)
varTest(Chinchero$CV2,conf.level = 0.96)$conf
sqrt(varTest(Chinchero$CV2,conf.level = 0.96)$conf)

######################################################################
#Intervalo de confianza para la diferencia de medias de datos pareados
######################################################################
#IC(mu antes - mu despues)= [a,b]
#IC(mu despues - mu antes)= [-b,-a]
#mu antes = mu despues entonces mu antes - mu despues = 0
#mu1-mu2
#[-,+] mu1=m2
#[-,-] mu1<mu2
#[+,+] mu1>mu2
hist(Chinchero$CV1-Chinchero$CV2)
qqnorm(Chinchero$CV1-Chinchero$CV2)
t.test(Chinchero$CV1,Chinchero$CV2,conf.level = 0.96,
       paired=T)$conf

########################################################
#Intervalo de confianza para el coeficiente de variación
########################################################
par(mfrow=c(1,2))
hist(Talara$CV1)
qqnorm(Talara$CV1)
library(DescTools)
CoefVar(Talara$CV1,conf.level = 0.96)*100

sd(Talara$CV1)/abs(mean(Talara$CV1))

#################################################
#Intervalo de confianza para la razón de varianza
#################################################
boxplot(contraccion$CV1~contraccion$DISTRITO)
par(mfrow=c(2,2))
hist(Talara$CV1)
qqnorm(Talara$CV1)
hist(Olmos$CV1)
qqnorm(Olmos$CV1)

#IC(sigma1/sigma2)=[a,b]
#IC(sigma2/sigma1)=[1/b,1/a]
#sigma1 = sigma2 entonces sigma1/sigma2 = 1
#si esta el 1, las varianzas son iguales
var.test(Talara$CV1,Olmos$CV1,conf.level = 0.96)$conf
var.test(Olmos$CV1,Talara$CV1,conf.level = 0.96)$conf

####################################################
#Intervalo de confianza para la diferencia de medias
####################################################
#IC(mu 1 - mu 2)= [a,b]
#IC(mu 2 - mu 1)= [-b,-a]
#mu 1 = mu 2 entonces mu 1 - mu 2 = 0
#[-,+] 
#[-,-]
#[+,+]

t.test(Talara$CV1,Olmos$CV1,conf.level = 0.96,
       paired=F, var.equal=T)$conf


##########################################################
#IC para datos que no provienen de una distribución normal
##########################################################

#Datos provenientes de una distribución exponencial (una muestra)

ic.de<-function(datos,conf){
  alfa<-1-conf
  n<-length(datos)
  LI<-sum(datos)/qgamma(1-alfa/2,n,1)
  LS<-sum(datos)/qgamma(alfa/2,n,1)
  lim<-c(LI,LS)
  return(lim)
}

RNGkind(sample.kind="Rounding")
set.seed(15)
tiempo<-rexp(25,1/6)
hist(tiempo)
mean(tiempo)
ic.de(tiempo,0.96)



#Datos provenientes de una distribución Bernoulli (una muestra)
table(contraccion$DISTRITO,contraccion$SEXO)
table(Olmos$SEXO)
#mejor metodo= menor ancho
#a) Método de Wald
library(DescTools)
mw<-BinomCI(33,60,conf.level = 0.96,method="wald")
mw[3]-mw[2]

#b)Método Score (o Wilson)
library(PropCIs)
ms<-scoreci(33,60,conf.level = 0.96)$conf
ms[2]-ms[1]

#c)Método Exacto
me<-binom.test(33,60,conf.level=0.96)$conf
me[2]-me[1]

#d)Método de Agresti-Caffo
library(PropCIs)
ma<-add4ci(33,60,conf.level=0.96)$conf
ma[2]-ma[1]

mimw<-function(a,n,conf){
  alfa<-1-conf
  p<-a/n
  eep<-sqrt(p*(1-p)/n)
  LI<-p-qnorm(1-alfa/2)*eep
  LS<-p+qnorm(1-alfa/2)*eep
  lim<-c(LI,LS)
  return(lim)
}

mimw(499,500,0.96)
BinomCI(499,500,conf.level = 0.96,method="wald")


#Datos provenientes de una Poisson (una muestra)
n<-nrow(Olmos)
poisson.test(sum(Olmos$ANILLOS),conf.level = 0.96)$conf/n



#Datos provenientes de una Bernoulli (dos muestras independientes)
table(contraccion$DISTRITO,contraccion$ESPECIE)
table(contraccion$DISTRITO)
#a)Método de Wald
library(DescTools)
mw<-BinomDiffCI(13,50,18,70,conf.level = 0.96,method="wald")
mw[3]-mw[2]
library(PropCIs)
wald2ci(13,50,18,70,conf.level = 0.96,adjust="Wald")$conf

#b)Método de Score
library(PropCIs)
ms<-diffscoreci(13,50,18,70,conf.level = 0.96)$conf
ms[2]-ms[1]

#c)Método de Agresti-Cafo
library(PropCIs)
ma<-wald2ci(13,50,18,70,conf.level = 0.96,adjust="AC")$conf
ma[2]-ma[1]

#Datos provenientes de una Poisson (dos muestras independientes)

dif.dp<-function(datos1,datos2,conf){
  alfa<-1-conf
  n1<-length(datos1)
  n2<-length(datos2)
  xbar1<-mean(datos1)
  xbar2<-mean(datos2)
  LI<-(xbar1-xbar2)-qnorm(1-alfa/2)*sqrt(xbar1/n1+xbar2/n2)
  LS<-(xbar1-xbar2)+qnorm(1-alfa/2)*sqrt(xbar1/n1+xbar2/n2)
  lim<-c(LI,LS)
  return(lim)
}

dif.dp(Talara$ANILLOS,Olmos$ANILLOS,0.96)

