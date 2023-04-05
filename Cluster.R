#######################################################
################## Análisis Cluster ###################
#######################################################

#Configuración de directorio de trabajo

setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/Conglomerado")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/EXTRA_ ANÁLISIS CLUSTER")
getwd()

#Lectura de datos
datos <- read.csv("datos_clustercsv.csv", dec = ",")
str(datos)

library(readxl)
datos <- read_excel("datos_cluster.xlsx")
str(datos)

datos<-read.delim("clipboard", dec=",")
str(datos)

#para visualizar los datos en una ventana aparte 
View(datos)


#matriz.dis.manhattan<-dist(datos[-1],method="manhattan",diag=TRUE)
matriz.dis.euclid<-dist(datos[-1],method="euclidean",diag=TRUE)
round(print(matriz.dis.euclid),2)

#ESTIMACIONES DE LOS CONGLOMERADOS

#estimacion con promedio

hclust.average<-hclust(matriz.dis.euclid,method="average")
data.frame(hclust.average[2:1])
plot(hclust.average,labels=datos$id,xlab = NULL, ylab = "Height", sub=NULL, 
     hang=-1,cex = 0.6)
rect.hclust(hclust.average, k = 2, border = "red")


#estimacion con Ward

hclust.ward<-hclust(matriz.dis.euclid,method="ward.D2")
data.frame(hclust.ward[2:1])
plot(hclust.ward,labels=datos$id,xlab = NULL, ylab = "Height", sub=NULL, 
     hang=-1,cex = 0.6)
rect.hclust(hclust.ward, k = 4, border = "red")


#estimacion con complete
hclust.complete<-hclust(matriz.dis.euclid,method="complete")
data.frame(hclust.complete[2:1])
plot(hclust.complete,labels=datos$id,xlab = NULL, ylab = "Height", sub=NULL,
     hang=-1,cex = 0.6)
rect.hclust(hclust.complete, k = 4, border = "red")

#Determinación del número de conglomerados( para las tres que no nos generan 
#dudas respecto a la claridad de los dendogramas)

install.packages("NbClust")
library(NbClust)

Datos.NbClust<-datos[-1]

res.average<-NbClust(Datos.NbClust, distance = "euclidean", min.nc=2, 
                     max.nc=15, method = "average", index = "alllong")
#res.average<-NbClust(Datos.NbClust, distance = "manhattan", min.nc=2, max.nc=15, method = "average", index = "alllong")

res.wardD2<-NbClust(Datos.NbClust, distance = "euclidean", min.nc=2, 
                    max.nc=15, method = "ward.D2", index = "alllong")
#res.wardD2<-NbClust(Datos.NbClust, distance = "manhattan", min.nc=2,max.nc=15, method = "ward.D2", index = "alllong")

res.complete<-NbClust(Datos.NbClust, distance = "euclidean", min.nc=2, 
                      max.nc=15, method = "complete", index = "alllong")
#res.complete<-NbClust(Datos.NbClust, distance = "manhattan", min.nc=2, max.nc=15, method = "complete", index = "alllong")

#Nota: El mayor índice permite permite determinar el número óptimo 
#de conglomerados.
