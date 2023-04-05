######################
#Pruebas de normalidad
######################
setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/CLASE2")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/5) Estadística inferencial")

getwd()
library(readxl)
desembosque <- read_excel("Desembosque.xlsx")
str(desembosque)
desembosque$Base <- factor(desembosque$Base)
desembosque<-read.delim("clipboard")
sur<-subset(desembosque,Base=="Sur")
str(sur)
par(mfrow=c(1,2))
hist(sur$Rendimiento)
qqnorm(sur$Rendimiento)
shapiro.test(sur$Rendimiento) #tiene como límite 5000 datos
ks.test(sur$Rendimiento,"pnorm",mean(sur$Rendimiento),sd(sur$Rendimiento)) #anuncio de cuidado porque existen empates
library(nortest)
ad.test(sur$Rendimiento)
pearson.test(sur$Rendimiento)
lillie.test(sur$Rendimiento)
sf.test(sur$Rendimiento)

library(moments)
jarque.test(sur$Rendimiento)
1-pchisq(1.4663,2) # como se distribuye como una chicuadrado con 2 grados de libertad

library(normtest) ##funciones reutilizan los datos por eso sale diferente (remuestreo) 
jb.norm.test(sur$Rendimiento)

#####################
#Prueba para la media
#####################
#varianza desconocida
#mu>15
t.test(sur$Rendimiento,mu = 15,alternative="g") #usar g para pruebas que buscan un mayor que
#H0: mu < = 15
#H1: mu > 15
#alfa=0.05
#tcal=1.9985                     pvalor=0.02496 < alfa se rechaza H0
qt(0.05,64,lower.tail = F)
#tcrit = 1.669
#Como tcal > tcrit se rechaza H0
#Conclusión
#A un nivel de significación de 0.05, existe suficiente evidencia estadística
#para rechazar H0.
#Por lo tanto, podemos afirmar que el rendimiento es superior a 15

#pvalor
1-pt(1.9985,64)
pt(1.9985,64,lower.tail = F)

#################
#Para la varianza
#################
library(EnvStats)
varTest(sur$Rendimiento,sigma.squared =1.2,alternative="l")# l para menor que (cola izquierda)
#Sigma2<1.2
#H0:sigma2=>1.2
#H1:sigma2<1.2
#alfa=0.05
#Chical= 78.519                   pvalor=0.8953>alfa no se rechaza H0
qchisq(0.05,64)
#Chicrit=46.59491
#Como Chical>Chicrit entonces no se rechaza H0
#Conclusión
#A un nivel de significación de 0.05 no existe suficiente evidencia
#estadística para rechaza H0
#Por lo tanto, no podemos afirmar que la varianza del rendimiento es
#inferior a 1.2

#pvalor
pchisq(78.519,64)

########################################################
#Para la diferencia de medias de una muestra relacionada
########################################################
hist(sur$`Ingreso 1`-sur$`Ingreso 2`)
qqnorm(sur$`Ingreso 1`-sur$`Ingreso 2`)
shapiro.test(sur$`Ingreso 1`-sur$`Ingreso 2`)
t.test(sur$`Ingreso 1`,sur$`Ingreso 2`,alternative="t",mu=0,paired=T)
t.test(sur$`Ingreso 2`,sur$`Ingreso 1`,alternative="t",mu=0,paired=T)
#H0:mu1 = mu2     entonces H0: mu1 - mu2 = 0
#H1:mu1 dif mu2   entonces H1: mu1 - mu2 dif 0
#alfa=0.05
#tcal = 0.0529            pvalor=0.9579 > alfa no se rechaza H0

qt(0.025,64)
qt(0.025,64,lower.tail=F)
#tcrit1 = -1.9977         tcrit2=1.9977 
#Como tcrit1<tcal<tcrit2 no se rechaza H0
#Conclusión
#A un nivel de significación de 0.05, no existe suficiente envidencia
#estadística para rechazar H0
#Por lo tanto, no podemos afirmar que existe diferencia en el ingreso 
#medio de los dos días en evaluación.

#pvalor
2*pt(0.0529,64,lower.tail=F)
