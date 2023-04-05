##########################################################
################# ESTADÍSTICA DESCRIPTIVA ################
##########################################################
#Configuración de directorio de trabajo
setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/CLASE1")
setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/ESTADISTICA FORESTAL/Clase 01")
#Lectura de datos
library(readxl)
anatomia <- read_excel("anatomia.xlsx")
head(anatomia,n=10)
# anatomia<-read.delim("clipboard") leer solamente si se copia la base de datos
str(anatomia)
#Información pertinente: 
# La variable cualitativa (nominal) Especie, tiene la siguiente especificación 1=Eucalipto y 2= Pino
# En la varaible cualitativa (nominal) Distrito: 1=Paucartambo, 2=Urubamba, 3=Chinchero
# En la variable cualitativa (ordinal) Opinión: 1=Mala, 2= Regular y 3 =Buena
#Codificar variables: modo 1
library(car)
anatomia$Especie <- recode(anatomia$Especie, "2 = 'Pino'; 1 = 'Eucalipto'")
anatomia$Distrito <- recode(anatomia$Distrito, "1 = 'Paucartambo'; 2 = 'Urubamba';3 = 'Chinchero'")
anatomia$Opinión <- recode(anatomia$Opinión, "1 = 'Mala'; 2 = 'Regular';3 = 'Buena'")
head(anatomia)

#Crear subgrupos
pino <- subset(anatomia, Especie=="Pino")
head(pino)
eucalipto <- subset(anatomia, Especie=="Eucalipto")
head(eucalipto)

#Definir un orden en las categorías
table(eucalipto$Opinión)
eucalipto$Opinión <- factor(eucalipto$Opinión,levels = c("Mala","Regular","Buena"),ordered=TRUE)
table(eucalipto$Opinión)

#Codificar variable: modo 2
library(readxl)
anatomia <- read_excel("anatomia.xlsx")
anatomia$Distrito<- factor(anatomia$Distrito, levels = c("1", "2","3"),
                           labels = c("Paucartambo", "Urubamba","Chinchero"))
anatomia$Especie<- factor(anatomia$Especie, levels = c("1", "2"),
                          labels = c("Eucalipto", "Pino"))
anatomia$Opinión<- factor(anatomia$Opinión, levels = c("1", "2","3"),
                          labels = c("Mala", "Regular", "Buena"))
head(anatomia)

#Crear subgrupos
pino <- subset(anatomia, Especie=="Pino")
head(pino)
eucalipto <- subset(anatomia, Especie=="Eucalipto")
head(eucalipto)

#Generación de tablas de frecuencia
#La función table crea las frecuencias absolutas
f1 <- table(eucalipto$Opinión)
#La función prop.table crea las frecuencias relativas
h1 <- round(prop.table(f1),4) #en proporción
p1 <- round(prop.table(f1)*100,2) #en porcentaje

## Gráfico de pie ##
## Pie 2D ##
x11()
pie(p1)
pie(p1,main="Distribución de la calificación de la plantación de Eucalipto")
porcentaje1 <- round(f1*100/sum(f1),2 ) #se obtiene lo mismo que en p1
porcentaje <- as.numeric(p1) #se obtiene una etiqueta para valores de porcentaje
opinion <- c("Malo", "Regular", "Bueno") #se obtiene una etiqueta para la opinion
etiquetas <- paste(opinion,"",porcentaje,"%") #se crea una etiqueta con 4 componentes
#Gráfico de PIE: 
# p1 es la base de datos en forma de tabla con el que se genera el grafico
# radius permite elegir el tamaño del círculo
# labels permite añadir una etiqueta a cada sector del pie
# col permite elegir el color de los sectores
# main permite escribir un título
# cex.main permite elegir el tamaño de letra del título
# \n permite dar un salto a la siguiente línea
# cex permite elegir el tamalo de letra de la etiqueta de los sectores
pie(p1 ,radius=1,labels=etiquetas,col = c("lightsalmon1","royalblue", "tomato3"),
    main="Distribución de la calificación de la plantación de Eucalpito"
    , cex.main= 1.5, cex= 1.0)
pie(p1 ,radius=1,labels=etiquetas,col = c("red","green", "blue"),
    main="Distribución de la calificación \n  de la plantación de Eucalpito"
    , cex.main= 2.1, cex= 1.5)
pie(p1 ,radius=0.8,labels=etiquetas
    ,main="Distribución de la calificación \n  de la plantación de Eucalpito"
    , cex.main= 1.5, cex= 1.5)

## Pie 3D ##
x11()
library(plotrix)
#Para el pie:
#p1: base de datos para formar los sectores
#explode:separación de sectores
#radius: tamaño de los sectores
#height: alto de sectores
#theta: ángulo de presentación del pie
#col: seleccionar colores
#labelcol: color de la etiqueta de los sectores
#labelcex: tamaño de letra de las etiquetas de los sectores
#main: para agregar título
#cex.main: tamaño de letra del título

#Para la leyenda:
#labels: agregar etiquetas (en este caso previamente creamos un vector pielabels)
#posición: puede ser "bottomright", "bottom", "bottomleft", "left", "topleft", "top",
# "topright", "right", "center"
# agregar un vector para la legenda c("Mala", "Regular", "Buena")
# fill: para elegir el color de los recuadros de la leyenda
# cex: tamaño de letra de la leyenda
# posición de la leyenda
pielabels <- c("24.05 %","41.77 %","34.18 %") #creo etiqueta de los sectores
pie3D(p1, explode=0.1, radius=0.75, height=0.1, theta=0.7,  
      col= c("lightsalmon1", "royalblue", "tomato3"),labelcol="black", labelcex=1.2
      , main = "Distribución de la calificación de las plantaciones forestales", 
      cex.main= 1.4, labels=pielabels) 
legend("topleft", c("Mala", "Regular", "Buena"), 
       fill = c("lightsalmon1", "royalblue", "tomato3"), cex = 0.9, inset = 0.1)

pie3D(p1, explode=0.1, radius=0.8, height=0.1, theta=0.6,
labelcol="black",labelcex=0.9, 
main = "Distribución de la calificación \n de las plantaciones forestales",
cex.main= 1.4, labels=pielabels) 
  legend("topleft", c("Mala", "Regular", "Buena"), fill = c("green", "blue", "red")
         , cex = 1, inset = 0.15)

#La función cbind sirve para unir dos objetos en forma de columnas
tabla1 <- cbind(f1,p1)
tabla1
#La función rbind sirve para unir dos objetos en forma de filas
tabla1.1 <- rbind(f1,p1)
tabla1.1
#Generación de una tabla de contingencia
tabla2 <- table(eucalipto$Opinión,eucalipto$Distrito)
tabla2
tabla2.1 <- table(eucalipto$Distrito,eucalipto$Opinión)
tabla2.1
## Gráfico de barras ##
# ylim: límites en el eje y
# xlab: nombre del eje x
# ylab: nombre del eje y
# sub: para agregar subtítulo en la parte inferior
# border: color del borde de las barras
x11()
barplot(f1,main="Calificación del estado de la plantación de Eucalipto",ylim=c(0,40), 
        col= c("lightsalmon1","royalblue", "tomato3"),xlab="Opinión",ylab="Observaciones",
        sub="Gráfico de barras",border="gray")
## Gráfico de barras apiladas o comparativas
# es necesario crear una tabla de contingencia (tabla2)
#font.lab: permite elegir la fuente de letras de los ejes
#font.main: permite elegir la fuente de letras del título
#beside: si es False se crean barras apiladas si es True se crean barras comparativas
#horiz: si es True las barras estarán en sentido horizontal, si es False serán verticales
tabla2
#Barras apiladas
barplot(tabla2,ylim=c(0,40),font.lab=2,font.main=4,beside=F, 
        col= c("lightsalmon1","royalblue", "tomato3"),xlab="Distritos",ylab="Observaciones",
        main="Calificación del estado de la plantación de Eucalipto por distrito",sub="",
        border="black", horiz = F)
legend("top", c("Mala", "Regular", "Buena"), 
       fill = c("lightsalmon1","royalblue", "tomato3"), cex = 1.2, inset = 0.1)
#Barras comparativas
barplot(tabla2.1,xlim=c(0,20),font.lab=2,font.main=2,beside=T, 
        col= c("lightsalmon1","royalblue", "tomato3"),xlab="Observaciones",
        ylab="Calificación",
        main="Calificación del estado de la plantación de Eucalipto por distrito",
        sub="",border="black", horiz = T)
legend("bottomright", c("Chinchero", "Paucartambo", "Urubamba"), 
       fill = c("lightsalmon1","royalblue", "tomato3"), cex = 1.2, inset = 0.1)

#Tabla de frecuencia para datos discretos
f2 <- table(eucalipto$Anillos)
f2
## Gráfico de Varas para varaibles discretas##
plot(f2,main="Gráfico de varas")
plot(f2,ylim=c(0,30),font.lab=2,font.main=2,col= "blue3", ylab = "Observaciones",
     xlab = "N° de anillos por cm", main="Gráfico de varas")

#Tabla de frecuencia para datos continuos
library(agricolae)
h2 <- with(eucalipto,graph.freq(eucalipto$Longitud,plot=F))
print(table.freq(h2),row.names=FALSE)
## Histograma de frecuencias ##
#hist: función para crear histogramas de intervalos de clase redondeados
# breaks: se selecciona un método para la determinación de los intervalos de clase
#polygon.freq: crea un polígono de frecuencias que une las marcas de clase
histo <- hist(eucalipto$Longitud,breaks= "Sturges", col= "lightgrey", font.lab=2,
              font.main=2,xlim=c(0,10),ylim=c(0,20),main="Histograma de frecuencias", 
              xlab = "Clases de longitud de fibra", ylab = "Frecuencia")
polygon.freq(histo, col= "blue3")
#los intervalos pueden manipularse a criterio del investigador con seq(inicio,fin,by=frecuencia)
histo2 <- hist(eucalipto$Longitud,breaks= seq(0,9,by=1), col= "lightgrey", font.lab=2,
               font.main=2,xlim=c(0,10),ylim=c(0,20),main="Histograma de frecuencias", 
               xlab = "Clases de longitud de fibra", ylab = "Frecuencia")
histo2 <- hist(eucalipto$Longitud,breaks= seq(0,10,by=2), col= "lightgrey", font.lab=2,
               font.main=2,xlim=c(0,10),ylim=c(0,30),main="Histograma de frecuencias", 
               xlab = "Clases de longitud de fibra", ylab = "Frecuencia")
#Creación de una tabla de frecuencia a partir de un histograma con la función hist()
# histo es una lista compuesta por los intervalos (breaks), frecuencias absolutas (counts)
# proporciones(density), marcas de clase (mids), entre otros
# creamos un data.frame con algunos componentes de histo
tab <- data.frame(histo$breaks[-1],histo$counts,histo$density,histo$mids)
sum(tab$histo.density) #se comprueba que la suma de las proporciones es igual a 1
LI <- tab$histo.breaks..1.-1 #se crea el límite inferior de la clase de 0 a 8
LS <- tab$histo.breaks..1. #se crea el límite inferior de la clase de 1 a 9
X <- tab$histo.mids #se crea las marcas de clase 
Fabs <- tab$histo.counts# se crea las frecuencias absolutas
Faacum <- cumsum(Fabs) # se crea las frecuencias absolutas acumuladas
Frel <- tab$histo.density*100 # se crea las frecuencias relativas (%)
Fracum <- cumsum(Frel) # se crea las frecuencias relativas (%) acumuladas
data.frame(LI,LS,X,Fabs,Frel,Faacum,Fracum) #Obtención de tabla de frecuencias de variables continuas

#Creación de histogramas y tablas de frecuencias con el método de Stuges sin redondeo
classes <- with(eucalipto,sturges.freq(eucalipto$Longitud))# lista de intervalos de clases
breaks <- classes$breaks #intervalos de clases
#gráfico de histograma con intervalos de clase sin redondear
#axes: si se coloca False borra el contenido de los ejes
h<-with(eucalipto,graph.freq(eucalipto$Longitud,breaks, col="lightgrey",axes=F,
      xlim=c(0,10),main="Histograma de frecuencias",
      xlab="Clases de longitud de fibra o traqueida",ylab="Frecuencia"))
#axis: se manipulan los ejes, 1 para el eje x en donde coloco los valores de los intervalos
# con el objeto breaks y giro los valores con las=2, 2 para el eje y en donde decido las
# frecuencias de los valores y giro con las=3
axis(1,breaks,las=2)
axis(2,seq(0,25,2.5),las=2)
polygon.freq(h, col= "blue3")
print(table.freq(h),row.names=FALSE) # es la misma tabla de frecuencia

## Gráfica acumulada
h3<-graph.freq(eucalipto$Longitud,plot=F)#se crea una lista con contenido de una tabla de frecuencia
#gráfica acumulada tipo 1
points<-ogive.freq(h3,col="red",frame=T,xlim=c(0,10),
                   xlab="Longitud", ylab="Frecuencia relativa acumulada", main="")

#gráfica acumulada tipo 2
#pch: se selecciona el tipo de punto 
#bty: si es igual a "l" se elimina el marco del gráfico
#frame: si es False se elimina las líneas de los ejes
plot(points,type="b",pch=16,bty="l",frame=T,xlim=c(0,10),axes=F,
     xlab="Longitud", ylab="Frecuencia relativa acumulada")
axis(1,breaks,las=2)
axis(2,seq(0,1,0.2),las=1)

###### Medidas de Posición ######

#Media
mean(eucalipto$Longitud) #media general de longitud de fibras de Eucalipto
#La función tapply permite calcular una medida por grupos
tapply(eucalipto$Longitud, eucalipto$Distrito, mean) #media de longitud por distrito
by(eucalipto$Longitud, eucalipto$Distrito, mean)#media de longitud por distrito
colMeans(eucalipto[,c(3,4)]) #media de longitud de fibra y espesor de pared de Eucalipto
head(eucalipto)
#Media podada
mean(eucalipto$Longitud, 0.05) #media podada global de longitud de fibra del 90% central
mean(trim=0.05,x=eucalipto$Longitud) #sin respetar el orden de los componentes de la función
tapply(eucalipto$Longitud, eucalipto$Distrito, mean,trim=0.05)#media podadade la longitud por distrito
by(eucalipto$Longitud, eucalipto$Distrito, mean,trim=0.05) #media podada por distrito
#Mediana
median(eucalipto$Longitud) #mediana global
tapply(anatomia$Pared,anatomia$Especie,median)  #mediana por especie

#Moda
table(eucalipto$Opinión) #se determina la moda indirectamente viendo las frecuencias
table(eucalipto$Anillos)

library(modeest) #permite identificar la moda directamente
mfv(eucalipto$Opinión)
mfv(eucalipto$Anillos)
#Cuantil
min(eucalipto$Longitud)
quantile(eucalipto$Longitud) # se ven 5 cuantiles de 25% de los datos
quantile(eucalipto$Longitud,0.2)#cuantil 20
quantile(eucalipto$Longitud,0.2,type=6) #cuantil 20 con el método 6
quantile(eucalipto$Longitud,0.97)
tapply(anatomia$Pared,anatomia$Especie,quantile,0.6,type=6) #cuantil 20 de pared por especie

###### Medidas de Dispersión #######

#Rango
r <- range(eucalipto$Longitud) #brinda el valor mínimo y el máximo
r[2]-r[1] #se obtiene el rango por deferencia del máximo y mínimo

#También se puede crear una función para obtener el rango
mirango <- function(datos){
  r<-max(datos)-min(datos)
  print(r)
}

mirango(eucalipto$Longitud) #rango global de longitud
mirango(eucalipto$Pared) #rango global de espesor de pared
tapply(anatomia$Pared,anatomia$Especie,mirango)
tapply(anatomia$Pared,anatomia$Especie,range)
#Rango Intercuartil o Rango Intercuartílico
IQR(eucalipto$Longitud) # rango intercuartil global de longitud
IQR(eucalipto$Longitud,type=6) # rango intercuartil global de longitud método 6
tapply(eucalipto$Longitud,eucalipto$Distrito,IQR) #IQR de longitud por distrito 

#Varianza
var(eucalipto$Longitud) #varianza global de longitud
tapply(eucalipto$Longitud,eucalipto$Distrito,var) #varianza de longitud por distrito

#Desviación Estándar
sd(eucalipto$Longitud) #desviación estándar global
tapply(anatomia$Pared,anatomia$Especie,sd) #desviación por especie

#Coeficiente de Variabilidad
library(raster)
cv(eucalipto$Longitud) #coeficiente de variación global
tapply(anatomia$Longitud,anatomia$Especie,cv) #cv por especie

###### Medidas de Asimetría y Curtosis #######

library(moments)
#Coeficiente de asimetría
skewness(eucalipto$Longitud)
hist(eucalipto$Longitud)
tapply(eucalipto$Longitud,eucalipto$Distrito,skewness)

#simulación de 100 datos con distribución normal
datos<-rnorm(100)
hist(datos)
skewness(datos)

#Coeficiente de Curtosis
kurtosis(eucalipto$Longitud)
tapply(eucalipto$Longitud,eucalipto$Distrito,kurtosis)

## Resumen de estadísticos descriptivos
summary(anatomia$Longitud) #se observa mínimo, máximo y los tres cuartiles
library(mosaic)
# promedio de longitud por especie
estadisticos <-  favstats(anatomia$Longitud~anatomia$Especie)[,c(1,7)]
estadisticos
#promedio de longitud por especie y distrito
estadisticos2 <-  favstats(anatomia$Longitud~anatomia$Especie+anatomia$Distrito)[,c(1,7)]
estadisticos2
#promedio de longitud por especie, distrito y opinión
estadisticos3 <-  favstats(anatomia$Longitud~anatomia$Especie+anatomia$Distrito+anatomia$Opinión)[,c(1,7)]
estadisticos3
#Tabla completa de estadísticos descriptivos
estadisticos4 <- favstats(anatomia$Longitud~anatomia$Especie+anatomia$Distrito+anatomia$Opinión) 
estadisticos4
# Creo una columna más sobre coeficiente de variabilidad
estadisticos4$cv <- (estadisticos4$sd*100)/estadisticos4$mean
estadisticos4
# Excluyo columnas sin importancia
estadisticos5 <- estadisticos4[,-10]
estadisticos5
# Guardo el resultado en mi carpeta de trabajo
write.csv(estadisticos5,file = "estadisticos_descriptivos.csv")

#Funciones adicionales para gráficos
#Visualizar sólo la línea de unión de puntos
x<-seq(1:10)
y<-x^2
plot(x,y,type="l")
lines(x,x^3)

#Graficar segmentos finitos y rectas
segments(8,0,6,80,col="blue")
segments(6,0,8,80,col="red")
abline(v=6)
abline(h=20)

#Agregar texto
title("Gráfica")
text(2,100,"Nuevo texto")

##### Panel de gráficos #####
x <- seq(1:10)
y <- seq(1:10)^2
plot(x,y)
w <- c(rep(1,3),c(1,4,5,6,6,7,3))
plot(w,y)

#Crear un panel de 2 filas y 2 columnas
layout(matrix(1:4,2,2,byrow=T))
plot(x,y)
plot(x,w)
plot(w,y)
plot(x,x)

par(mfrow=c(2,2))
plot(x,y)
plot(x,w)
plot(w,y)
plot(x,x)

# Crear panel de sólo una fila
par(mfrow=c(1,4))
plot(x,y)
plot(x,w)
plot(w,y)
plot(x,x)

# Volver a condiciones normales
par(mfrow=c(1,1))
#Colores
plot(x,y,main="Grafica x vs y",sub="grafica",xlab="datos de x",ylab="datos de y",type="b",col="blue")

#GRAFICOS IMPORTANTES
#Con la data tortuga
datos <- read.table("tortuga.txt",T,col.names=c("Longitud","Ancho","Altura","Sexo"))
head(datos)
tabla <- table(datos$Sexo)
tabla
#grafico de barras
x11()
barplot(tabla, col="red")
barplot(tabla, col=c("red","blue"))
#space: espacio entre barras
#
barplot(tabla, col=c("red","blue"),space=0.5,xlab="Sexo",ylab="fi",
        main="Gráfico de barras para sexo de tortuga",sub="data tortuga")
barplot(tabla, col=c("red","blue"),space=0.8,xlab="Sexo",ylab="fi",
        main="Gráfico de barras para sexo de tortuga",sub="data tortuga",border="green",horiz=TRUE)
barplot(tabla, col=c("red","blue"),space=1.2,xlab="fi",ylab="Sexo",
        main="Gráfico de barras para sexo de tortuga",sub="data tortuga",
        border="green",horiz=TRUE,names.arg=c("Hembra","Macho"),xlim = c(0,25))

#grafico de pie
tabla
pie(tabla)
porc <- tabla*100/sum(tabla)
as.numeric(porc)
etiquetas <- paste(c("H","M")," ",as.numeric(porc),"%")
etiquetas
grafico_pie <- pie(tabla,labels=etiquetas,col=rainbow(length(etiquetas)),main="Gráfico de pie para sexo de tortuga")

#grafico de varas o bastones
marcas <- c(rep(1,10),rep(2,15),rep(3,23)) #creación de 48 datos discretos
marcas
plot(table(marcas),xlab="N° de marcas en caparazón",ylab="fi",
     main="Gráfico de varas para el número de marcas en caparazón")

#histograma de frecuencias
hist(datos$Ancho)
hist(datos$Ancho, main="Histograma para ancho de caparazón")
hist(datos$Ancho, main="Histograma para ancho de caparazón",
     col="gray",xlab = "Ancho",ylab = "fi",ylim = c(0,20))

#histograma y tabla de frecuencias 
library(agricolae)
classes1 <- with(datos,sturges.freq(datos$Ancho))
breaks1 <- classes1$breaks
hist <- with(datos,graph.freq(datos$Ancho,frequency=1, col="lightgrey",axes=F,
       main="Histograma de frecuencias",xlab="Clases de ancho de caparazón",
       ylab="Frecuencia"))
axis(1,breaks1,las=2)
axis(2,seq(0,20,2.5),las=1)
polygon.freq(hist, col= "blue3")
print(table.freq(hist),row.names=FALSE)

