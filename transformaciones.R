##################################################
###Transformaciones para estabilizar Variancia ###
##################################################

setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/Transformaciones")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/8)Transformaciones")

#Ejemplo: Un ingeniero civil está interesado en determinar si cuatro métodos 
#diferentes para estimar la frecuencia de las inundaciones producen estimaciones 
#equivalentes de la descarga pico cuando se aplican a la misma cuenca. Cada 
#procedimiento se usa seis veces en la cuenca, y los datos de las descargas 
#resultantes (en pies cúbicos por segundo) se muestran en la siguiente tabla:

descarga<-read.table("descarga.txt",header=T)
str(descarga)
head(descarga)
y<-descarga[,1]
metodo<-as.factor(descarga[,2])
mod1<-lm(y~metodo)
anova(mod1)
par(mfrow=c(2,2))
plot(mod1)
ri<-rstandard(mod1)
shapiro.test(ri)
library(car)
ncvTest(mod1)

bartlett.test(y~metodo)

## Transformacion Raiz Cuadrada
yt<-y^0.5
mod2<-lm(yt~metodo)
anova(mod2)
bartlett.test(yt~metodo)

ncvTest(mod2)
ri<-rstandard(mod2)
shapiro.test(ri)
## Transformacion seno inverso
ysen<-asin((y/100)^0.5)
modsen <- lm(ysen~metodo)
ncvTest(modsen)
bartlett.test(ysen~metodo)

ri<-rstandard(modsen)
shapiro.test(ri)

##Transformacion log
yl <- log10(y) # en base 10
yl2 <- log(y) # logaritmo natural
modlog <- lm(yl2~metodo)
ncvTest(modlog)
bartlett.test(yl2~metodo)

ri<-rstandard(modlog)
shapiro.test(ri)

## Transformacion de Box y Cox
par(mfrow=c(1,1))
library(MASS)
transformacion <- boxcox(y~metodo)
abline(transformacion,v=0.55, col="blue")
#library(fpp)
#BoxCox.lambda(y, method=c("loglik"), lower=-5, upper=5)
#BoxCox.lambda(y, method=c("guerrero"), lower=-5, upper=5)

ytb<-(y^0.55-1)/0.55 #transforamción Box-cox
mod3<-lm(ytb~metodo)
anova(mod3)

par(mfrow=c(2,2))
plot(mod3)
ncvTest(mod3)
bartlett.test(ytb~metodo)
ri<-rstandard(mod3)
shapiro.test(ri)

