##########################################################
################# AN�LISIS EXPLORATORIO ##################
##########################################################

## Configurar carpeta de trabajo ##

setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTAD�SICO/MEYDE_P1/CLASE2")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/Clase 02")
getwd()


############################################################
############## An�lisis exploratorio #######################
############################################################
datos <- read.csv("TARDIAVSTEMPRANACSV.csv")
str(datos)
head(datos)
datos <- read.csv("TARDIAVSTEMPRANACSV.csv", header=T, sep=",", dec=",")
str(datos)
datos <- read.delim("clipboard")
head(datos)

## Creaci�n de subconjuntos
araucaria <- subset(datos,Especie=="Araucaria")
araucaria_temprana <- subset(araucaria, madera =="temprana")
araucaria_tardia <- subset(araucaria, madera=="tardia")
pino <- subset(datos,Especie=="Pino")
pino_temprana <- subset(pino, madera =="temprana")
pino_tardia <- subset(pino, madera=="tardia")
######################################################
############# Gr�ficos univariados ####################
######################################################

#######################
#Diagrama de Intervalos
#######################

## Creaci�n de tabla de espesor de pared por especie y tipo de madera
tabla1 <- data.frame(pino_temprana$epar,pino_tardia$epar,araucaria_temprana$epar,
                     araucaria_tardia$epar)
str(tabla1)
head(tabla1)
names(tabla1)=c("P.temprana","P.tardia","A.temprana","A.tardia")
library(psych)
x11()
# su base de datos es un data.frame
# alpha: 1 - alpha es el nivel de confianza, mientras menor sea alpha mayor ser� el intervalo
# eyes: Si es TRUE, vemos un �rea, si es FALSE solo se ve los intervalos
error.bars(tabla1,col = c("lightsalmon1","royalblue", "tomato3","gray") ,font.lab=2,font.main=2, 
           ylab= "Espesor de pared celular (�m)", xlab= "Tipo de madera por especie",
           main="Diagrama de Intervalos al 95% de confianza",alpha=0.05, eyes = T)
error.bars(tabla1,col = c("lightsalmon1","royalblue", "tomato3","gray"),font.lab=2,font.main=2 , 
           ylab= "Espesor de pared celular (�m)", xlab= "Tipo de madera por especie",
           main="Diagrama de Intervalos al 95% de confianza",alpha=0.05, eyes = F)

##################
#Diagrama de Cajas
##################
#border: color del borde de las cajas
#notch: hace una muesca al centro de la caja
#names: permite cambiar en el gr�fico los nombres de las categor�as
x11()

#Diagrama de cajas cl�sico con datos obtenidos de subgrupos
boxplot(tabla1,
        main="Diagrama de Cajas",xlab="Espesor de pared celular (�m)",
        ylab="Tipo de madera por especie",
        horizontal=F,notch=F, ylim=c(2,7))
#Diagrama de cajas personalizado con datos obtenidos de subgrupos
boxplot(tabla1,border = c("lightsalmon1","royalblue", "tomato3","gray"),
        main="Diagrama de Cajas",xlab="Espesor de pared celular (�m)",
        ylab="",
        horizontal=T,notch=T, ylim=c(2,7), axes=F)
legend("topleft", c("P.temprana","P.tardia","A.temprana","A.tardia"), 
       fill = c("lightsalmon1","royalblue", "tomato3","gray"), cex = 1.2, inset = 0.1)
axis(1,seq(2,7,0.5),las=2)

#Diagrama de cajas personalizado con datos obtenidos de la data original
boxplot(datos$epar~datos$Especie+datos$madera+datos$etapa,border = c("lightsalmon1","royalblue", "tomato3","gray"),
        main="Diagrama de Cajas",xlab="Espesor de pared celular (�m)", ylab="",horizontal=F,notch=F, 
        ylim=c(2,7)) # nombres muy extensos

boxplot(datos$epar~datos$Especie+datos$madera+datos$etapa,col = c("lightsalmon1","royalblue", "tomato3","gray"),
        main="Diagrama de Cajas",xlab="Espesor de pared celular (�m)", ylab="",horizontal=F,notch=F, 
        ylim=c(2,7),names= c("A.adulto.tard�a","P.adulto.tard�a","A.adulto.temprana","P.adulto.temprana",
                             "A.juvenil.tard�a","P.juvenil.tard�a","A.juvenil.temprana","P.juvenil.temprana")) # nombres muy extensos
#Boxplot separado por especie
par(mfrow=c(1,2))
boxplot(araucaria$epar~araucaria$madera+araucaria$etapa,col = c("lightsalmon1","royalblue", "tomato3","gray"),
    font.main=6 ,main="Araucaria angustifolia",xlab="", ylab="Espesor de pared celular (�m)",horizontal=F,notch=F, 
        ylim=c(2,7),names= c("Adulto.tard�a","Adulto.temprana","Juvenil.tard�a","Juvenil.temprana"))

boxplot(pino$epar~pino$madera+pino$etapa,col = c("lightsalmon1","royalblue", "tomato3","gray"),
   font.main=6,main="Pinus patula",xlab="", ylab="Espesor de pared celular (�m)",horizontal=F,notch=F, 
        ylim=c(2,7),names= c("Adulto.tard�a","Adulto.temprana","Juvenil.tard�a","Juvenil.temprana"))

par(mfrow=c(1,2))
x11()
boxplot(araucaria$epar~araucaria$madera+araucaria$etapa,col = c("lightsalmon1","royalblue", "tomato3","gray"),
        font.main=6 ,main="Araucaria angustifolia",xlab="Espesor de pared celular (�m)", 
        ylab="",horizontal=F,notch=F, ylim=c(2,7),
        names= c("Adulto.tard�a","Adulto.temprana","Juvenil.tard�a","Juvenil.temprana"),axes=F) 
axis(2,seq(2,7,0.5),las=2)  
legend("topright", c("Adulto tard�a","Adulto temprano","Juvenil tard�a","Juvenil temprana"), 
       fill = c("lightsalmon1","royalblue", "tomato3","gray"), cex = 0.6, inset = 0.1)

boxplot(pino$epar~pino$madera+pino$etapa,col = c("lightsalmon1","royalblue", "tomato3","gray"),
        font.main=6,main="Pinus patula",xlab="Espesor de pared celular (�m)", ylab="",horizontal=F,notch=F, 
        ylim=c(2,7),names= c("Adulto.tard�a","Adulto.temprana","Juvenil.tard�a","Juvenil.temprana"),axes=F)
legend("topright", c("Adulto tard�a","Adulto temprano","Juvenil tard�a","Juvenil temprana"), 
       fill = c("lightsalmon1","royalblue", "tomato3","gray"), cex = 0.6, inset = 0.1)
axis(2,seq(2,7,0.5),las=2)
################
#Diagrama Violin
################
library(vioplot)
par(mfrow=c(1,1))
x11()
vioplot(tabla1)
par(mfrow=c(1,2))
vioplot(tabla1, horizontal=F,col = c("lightsalmon1","royalblue", "tomato3","gray"),font.lab=2,
        main="Diagrama de Viol�n",ylab="Espesor de pared celular (�m)",
        xlab="Tipo de madera por especie")
boxplot(tabla1, horizontal=F,col = c("lightsalmon1","royalblue", "tomato3","gray"),font.lab=2,
        main="Diagrama de Cajas",ylab="Espesor de pared celular (�m)",
        xlab="Tipo de madera por especie")
par(mfrow=c(1,1))

######################################################
############# Gr�ficos multivariados #################
######################################################

#Manejo del panel gr�fico
m1 <- matrix(c(1,2,3,4,5,5), nrow=2, ncol=3)
layout(m1, heights=c(1, 1), widths=c(1, 1,2))
layout.show(3)

######################
#Gr�fico de Dispersi�n
######################
head(datos)
x11()
head(datos)
#Gr�fico de dispersi�n general
plot(datos$epar,datos$longitud,main="Gr�fico de Dispersi�n",xlab="Espesor de pared celular",
     ylab="Longitud de traqueidas (�m)")

#Gr�ficos de dispersi�n por grupos
plot(pino_tardia$epar,pino_tardia$longitud,col="blue",main="Pino madera tard�a",
     xlab="Espesor de pared celular",
     ylab="Longitud de traqueidas (�m)")
plot(pino_temprana$epar,pino_temprana$longitud,col="blue",main="Pino madera temprana",
     xlab="Espesor de pared celular",ylab="Longitud de traqueidas (�m)")
plot(araucaria_tardia$epar,araucaria_tardia$longitud,col="blue",main="Araucaria madera tard�a",
     xlab="Espesor de pared celular",ylab="Longitud de traqueidas (�m)")
plot(araucaria_temprana$epar,araucaria_temprana$longitud,col="blue",main="Araucaria madera temprana",
     xlab="Espesor de pared celular",ylab="Longitud de traqueidas (�m)")

#Identificaci�n de categor�as dentro del gr�fico solo para la especie pino
x11()
m1 <- matrix(c(1,2,3,4,5,5), nrow=2, ncol=3)
layout(m1, heights=c(1, 1), widths=c(1, 1,2))
layout.show(3)
#en una base de datos ordenada se puede extraer tambi�n de la siguiente forma
head(pino)
pino[1:25,] #adulto tard�a
pino[26:50,] #adulto temprana
pino[51:75,] #juvenil tard�a
pino[76:100,] #juvenil temprana
col <- c(rep("red",25),rep("green",25),rep("blue",25),rep("gray",25))

plot(pino$epar,pino$longitud,main="Gr�fico de Dispersi�n espesor de pared vs longitud",xlab="Espesor de pared celular",
     ylab="Longitud de traqueidas (�m)", col= col, font.main=3)

plot(pino[1:25,6],pino[1:25,7],col="red",main="Madera adulta tard�a",
     xlab="Espesor de pared celular",
     ylab="Longitud de traqueidas (�m)",font.main=1)
plot(pino[26:50,6],pino[26:50,7],col="green",main="Madera adulta temprana",
     xlab="Espesor de pared celular",ylab="Longitud de traqueidas (�m)",
     font.main=4)
plot(pino[51:75,6],pino[51:75,7],col="blue",main="Madera juvenil tard�a",
     xlab="Espesor de pared celular",ylab="Longitud de traqueidas (�m)",
     font.main=9)
plot(pino[76:100,6],pino[76:100,7],col="gray",main="Madera juvenil temprana",
     xlab="Espesor de pared celular",ylab="Longitud de traqueidas (�m)",
     font.main=7)

########################
#Matriz de Dispersi�n 2D
########################
par(mfrow=c(1,1))
pairs(pino_tardia[,4:7],main="Matriz de Dispersi�n",col=2)
# upper.panel = NULL le quita la repetici�n de informaci�n
pairs(pino_tardia[,4:7],main="Matriz de Dispersi�n",col=5,upper.panel = NULL)


library(car)
head(pino_tardia)
scatterplotMatrix(pino_tardia[,4:7],col=("blue4"),upper.panel = NULL)

########################
#Matriz de Dispersi�n 3D
########################
head(pino_tardia)
library(scatterplot3d)
# type="h" le agrega una l�nea vertical para facilitar ubicaci�n
scatterplot3d(pino_tardia[1:25,5:7],main="Gr�fico de Dispersi�n 3D", type="h", color = "blue3")

library(rgl)
open3d()   
col <- c(rep("red",25),rep("green",25),rep("blue",25),rep("gray",25))
plot3d(pino$dlum,pino$epar,pino$longitud,xlab="",ylab="",zlab="",col=col,size=1.5,type="s") + 
decorate3d(xlab="Ancho",ylab="Espesor",zlab="Longitud", axes = TRUE, main = "", expand = 0.7) 

legend3d("topright", legend = paste('Pino', c('A.tard�a', 'A.temprana', 'J.tardia', 'J.temprana')),
         pch = 16, col = c("red","green","blue","gray"), cex=1.2,inset=c(0.15))

library(plot3D)
x11()
scatter3D(pino$dlum,pino$epar,pino$longitud,xlab="Ancho",ylab="Espesor",zlab="Longitud",
          clab = c("Espesor de", "pared (�m)"), colvar = pino$epar, col = NULL, add = F,
          main ="Diagrama 3D")
scatter3D(pino$dlum,pino$epar,pino$longitud, xlab="Ancho",ylab="Espesor",zlab="Longitud",
          bty = "b2",clab = c("Espesor de", "pared (�m)"), colvar = pino$epar,colkey = T, 
          main ="Diagrama 3D" )
scatter3D(pino$dlum,pino$epar,pino$longitud, xlab="Ancho",ylab="Espesor",zlab="Longitud",
          bty = "g",clab = c("Espesor de", "pared (�m)"), colvar = pino$epar,colkey = T, 
          main ="Diagrama 3D" )

##################
#Diagrama Marginal
##################
library(ggplot2)
# se crea un dataframe con las variables espesor y longitud
df <- data.frame(espesor = pino_tardia$epar, longitud = pino_tardia$longitud)
# se crea el gr�fico de dispersi�n: 
# ggplot(df, aes(x, y)): crea formato del panel gr�fico con l�mites seg�n los valores 
# geom_point(): grafica la dispersi�n
# theme_classic(): cambia de estilo del recuadro
p <- ggplot(df, aes(espesor, longitud)) + geom_point() + theme_classic()
#se a�ade un histograma por cada variable
ggExtra::ggMarginal(p, type = "histogram")

#########################
#Gr�fico de Correlaci�n #
#########################
library(PerformanceAnalytics)
x11()
chart.Correlation(pino_tardia[,-c(1,2,3)], histogram=TRUE, pch=20)

library(corrplot)
M<-cor(pino_tardia[,4:7],method="pearson")
corrplot(M, method="circle")
corrplot(M, method="number")
corrplot(M, method="color") #MAPA DE CALOR#
corrplot(M, type="upper")
corrplot.mixed(M)
corrplot.mixed(M, lower="ellipse", upper="circle")

library(psych)
cor.plot(cor(pino_tardia[,-c(1,2,3)]),  main="Mapa de Calor",   diag=TRUE, show.legend = TRUE) 

library(corrgram)
corrgram(pino_tardia[,4:7],lower.panel=panel.cor,upper.panel=panel.conf,cor.method="pearson")

#Recomiendo usar la funci�n pairs.panels para correlaci�n por par de variables
library(psych)
head(datos)
par(mfrow=c(2,2))
hist(datos$dtot)
hist(datos$dlum)
hist(datos$epar)
hist(datos$longitud)
par(mfrow=c(1,1))
pairs.panels(datos[-c(1,2,3)], breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)
pairs.panels(pino_tardia[-c(1,2,3)], breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)
pairs.panels(pino_temprana[-c(1,2,3)], breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)
pairs.panels(araucaria_tardia[-c(1,2,3)], breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)
pairs.panels(araucaria_temprana[-c(1,2,3)], breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)

##################
#Caras de Chernoff
##################
##TAMBI�N LLAMADO GR�FICO DE PERFILES##
head(datos)
library(aplpack)
faces(datos[1:25,4:7])
faces(datos[1:25,4:7], face.type=2)
library(TeachingDemos)
faces2(datos[1:25,4:7])


#####################
#Piramide Poblacional
#####################
x11()
library(plotrix)
#gap: espacio entre 2 histogramas
#show.values: permite visualizar las frecuencias
ma<-c(10,25,15)
fe<-c(15,20,10)
edad<-c("0-25","26-50","51-75")
par(mar=pyramid.plot(ma,fe,labels=edad,top.labels=c("Masculino","Edades","Femenino"),
                     main="Piramide Poblacional",gap=7,show.values=TRUE))

# se crean 12 valores de frecuencia por cada categor�a 
m.pop<-c(7.1,7.8,6.9,6.5,6.2,5.8,3.3,2.3,1.5,1.3,0.7,0.4)
f.pop<-c(7.2,7.0,7.4,6.8,6.7,5.5,3,1.7,1.7,1.3,1,0.9)
# se crean los intervalos de clase
edades<-c("30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
# se crean las gradientes de colores, se recomienda solo modificar el �ltimo valor
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),12)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),12)
# Pir�mide poblacional con colores por defecto
par(mar=pyramid.plot(m.pop,f.pop,labels=edades,top.labels=c("Lupuna","DAP","Tornillo"),
                     main="Piramide Poblacional de Australia 2002",gap=0.5,show.values=TRUE))
# Pir�mide poblacional con gradientes de colores creados
par(mar=pyramid.plot(m.pop,f.pop,labels=edades,top.labels=c("Lupuna","DAP","Tornillo"),
                     main="Pir�mide Poblacional de dos especies forestales",lxcol=mcol,
                     rxcol=fcol,gap=1,show.values=TRUE))

# Pir�mide poblacional desde una base de datos
setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTAD�SICO/MEYDE_P1/PC1_MEYDE")
data <- read.csv("CASO1_MEYDE.csv")
head(data)
pb <- subset(data,Especie=="Parinari blanco") #creo subgrupo de especie de inter�s
mb <- subset(data,Especie=="Machimango blanco")
hist(pb$DAP) # visualizo histogramas independientes
hist(mb$DAP)
# creo histogramas con ciertas caracter�sticas y lo guardo en un objeto
hpb <- hist(pb$DAP,breaks= seq(0,60,by=5),xlim=c(0,70),ylim=c(0,25))
hmb <- hist(mb$DAP, breaks= seq(0,60,by=5),xlim=c(0,50),ylim=c(0,25))
m.pop<-hpb$counts[2:12]
f.pop<-hmb$counts[2:12]
edades<-c("5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),11)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),11)
par(mar=pyramid.plot(m.pop,f.pop,labels=edades,top.labels=c("Parinari blanco","DAP","Machimango blanco"),
main="Pir�mide Poblacional de dos especies forestales",lxcol=mcol,rxcol=fcol,gap=1.5,show.values=TRUE))

###################
#Gr�fico de Mosaico
###################
setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTAD�SICO/MEYDE_P1/CLASE1")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/Clase 01")
library(readxl)
dat<- read_excel("anatomia.xlsx")
library(car)
head(dat)
# creamos subgrupos
dat$Especie <- recode(dat$Especie, "2 = 'Pino'; 1 = 'Eucalipto'")
dat$Distrito <- recode(dat$Distrito, "1 = 'Paucartambo'; 2 = 'Urubamba';3 = 'Chinchero'")

anatomia$Distrito<- factor(anatomia$Distrito, levels = c("1", "2","3"),
                           labels = c("Paucartambo", "Urubamba","Chinchero"))
anatomia$Especie<- factor(anatomia$Especie, levels = c("1", "2"),
                          labels = c("Eucalipto", "Pino"))
# se puede crear un objeto a partir de una columna de la base de datos
Especie <- dat$Especie
Distrito <- dat$Distrito
table(dat$Distrito,dat$Especie)
table(Distrito,Especie)
library(vcd)
x11()
mosaicplot(~dat$Especie+dat$Distrito, data = dat, col = c("lightsalmon1","royalblue", "gray"), 
           main="Ubicaci�n -Tipo")
mosaicplot(~Especie+Distrito, data = dat, col = c("lightsalmon1","royalblue", "gray"), 
           main="Ubicaci�n -Tipo")



### Gr�fico extra con aplicaciones emergentes de redes sociales ##
#Nube de palabras (miner�a de texto)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Lectura de datos
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

#Carga los datos como un corpus
docs <- Corpus(VectorSource(text))

#Permite inspeccionar el documento
inspect(docs)

#Cambia caracteres especiales por espacio en blanco
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convierte el texto a min�sculas
docs <- tm_map(docs, content_transformer(tolower))
# Elimina n�meros
docs <- tm_map(docs, removeNumbers)
# Elimina comunes palabras "stopwords" en english  
docs <- tm_map(docs, removeWords, stopwords("english"))
# Elimina propios stopword
# Se debe especificar stopwords como un vector de caracteres
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Elimina puntuaciones
docs <- tm_map(docs, removePunctuation)
# Elimina espacios en blanco extras
docs <- tm_map(docs, stripWhitespace)

#Construye el texto como una matriz
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Genera el Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
