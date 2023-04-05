##########################################################
############## INTRODUCCIÓN AL PROGRAMA R ################
##########################################################

############ Uso matemático #############

#### Nombramiento ##
x <- 16
x = 16
y <- -25
y 
z <- sqrt(x)                #raíz cuadrada
w <- abs(y)                 #valor absoluto
exp(x)                      #halla exponente:   e^16
operacion <- ((x + 2^z)*w^(1/2))/sqrt(w) #ecuaciones generadas en R
sqrt(x/w)                   #uso de funciones integrando ecuaciones en R

#Clasificación de objetos

#Lógicos
V <- TRUE
V <- T
V <- t #tener cuidado con las mayúsculas, minúsculas y signos de puntuación

canción <- c("la cura", "la rueda", "mi libertad")
canción
cancion

N <- FALSE
N <- F

#Numéricos
Q <- 20
#Complejos
I <- -20+16i

#Categoría
C <- "JOAN"


### Algunas funciones básicas ##
#log(x, base)
x <- 64
log(x, 4)

z <- -8
z
abs(z)

#Potencia  ^ 
P <- 5^2  
P
A <- 25
C <- A^1/2  
C
D <- A^(1/2)  
D

#Redondeo de Números
x <- 19.499999999

#Redondeo al entero por exceso
ceiling(x)                      ### para número de intervalos

#Redondeo al entero por defecto
floor(x)

#Redondeo aritmético simple
round(19.499999,0)

round(13.505091,2)
round(13.489999,2)

#Para comprobación del tipo de objeto
# Preguntas particulares
N <- 20 
is.numeric(N)

D <- "JOAN"
is.character(D)

c <- 3+4i
is.complex(c)

#Pregunta general
mode(D)
mode(N)

#Eliminación de objetos
rm(list=ls())

#Para usar la ayuda colocar ?función
?rm

###### Vectores ########

#Crear una serie de números enteros
V3 <- 14:20
V3

#seq 
#Para una secuencia determinada (14 al 20) y suba de 0.5
v4 <- seq(14,20,0.5)
v4

rep(3,5)    #repite 3, cinco veces
rep(c(1,2,3),4) #vector (1,2,3),  se repite cuatro veces
rep(1:3,4) #secuencia 1 al 3, esta se  repite cuatro veces

rep(1:5,c(1,4,5,6,8))

#sort 
#Ordenación 
v6 <- c(9,6,37,12,11,13,1,5)
sort(v6)

#Invertir el orden
sort(v6,decreasing=TRUE)  

#Para saber cuantos elementos posee el vector
length(v6)

#Evaluación de una expresión
calificacion <- c(18,14,12,13,15)
nombres <- c("Pep","Mou","Chemo","Mosquera","Titin")
aprobados <- calificacion>=14
data.frame(nombres,calificacion,aprobados)  ## ver marco de datos

#Vectores con caracteres 

nivel.portugues = c("intermedio", "intermedio", "basico", "intermedio", "basico", "intermedio", "avanzado", "avanzado","basico","intermedio")           
nivel.portugues
###### Factores #######

#factor
d<-factor(c(1,1,1,2,2,2,3,3,3))
d
is.factor(d)
d<-factor(c(1,1,1,1,1,1,1,1,1,q,1,1,2,2,2,2,2,3,2,2,2,2,2,2),levels=c(1,2))
d

##### Matrices #####

#FUNCION MATRIX 
#matrix(VECTOR, N° DE FILAS, N° COLUMNAS)

A <- matrix(0,3,7)
A
B <- matrix(1:9,3,3)
B

C <- matrix(1:9,3,3,byrow=T)  #Para rellenar por filas: byrow
C

#Para editar una matriz: fix
fix(A)
length(A)               ## número de datos
dim(A)                  ## número de filas y columnas 


####### Manipulación de vectores y matrices #######

N <- matrix(1:6,nr=3,nc=2)
N
A <- matrix(1:6,nr=3,nc=2,byrow=TRUE)
A
t(A)      ##transposición
t(t(A))

## operaciones con vectores y matrices ##
a<-seq(1:5)
a
b<-rep(2,5)
b
a+b
a-b
a*b
a/b

############# Selección de datos en vectores #######
d<-c(7,10,15,36,9)
d[3]
d[1:3]

###imprimir todos los elementos del vector d menos el último
d[1:4]
d[-5]


##### Series de tiempo #####

#Serie de tiempo
f<-c(1:15)
f
serie<-ts(f,start=2000)
serie
serie2<-ts(f,start=2015,frequency = 12)
serie2
serie3<-ts(f,start=2015,frequency=4)
serie3


##### Lista #####
#lista
a<-c(28,33,29,27,20)
d<-factor(c(1,1,1,2,3),levels=c(1,2))
L1<-list(a,d)
L1
b<-c("amarillo","rojo","azul","azul","blanco")
e<-c(5,5,6)
L2<-list(colores=b,numeros=e)
L2

#llamado de objetos en una lista
#lista
L2$colores
L2$numeros

##### Configurar el directorio de trabajo ####

setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/CLASE1")
# Comprobación #
getwd()

##### LECTURA DE DATOS #####

# Abrir un archivo #
# TXT #
tortuga <- read.table("tortuga.txt", header=T) 
tortuga
head(tortuga)
# CSV separación decimal con coma #
seriemaestra <- read.csv("negexp.csv", header=T, sep=";", dec=",")
seriemaestra
# CSV separación decimal con punto #
seriemaestra1 <- read.csv("negexp.csv", header=T, sep=",", dec=".",col.names=c("Año","Estandarización","Amplitud_muestra"))
seriemaestra1

#read.table
data1 <- read.table("tortuga.txt",T,col.names=c("Longitud","Ancho","Altura","Sexo"))
head(data1)

#read.delim
data2 <- read.delim("tortuga.txt",T,col.names=c("v1","v2","v3","v4"))
head(data2)
data2 <- read.delim("clipboard",T) ##copiar todo el contenido
head(data2)

#read.csv
data3 <- read.csv("negexp.txt",T)
head(data3)

#read_excel
library(readxl)
data5 <- read_excel("NEGEXP2.xlsx")
head(data5)
data6 <- read_excel("NEGEXP2.xlsx",sheet=2)   # aviso nuevos nombres de columnas #
head(data6)
str(data6)
#read_rwl
library(dplR)
data7 <- read.rwl("SALIDAFINAL.rwl")

######### SELECCIÓN DE DATOS ############

setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/CLASE1")
getwd()

datos <- read.table("tortuga.txt",T,col.names=c("Longitud","Ancho","Altura","Sexo"))
head(datos)
datos
datos[1:6,2]    #primeras 6 filas de la columna 2
datos$Ancho[1:6]
datos[c(8,11,15),3:4] ## fila número 8, 11 y 15 de las columnas 3 y 4
datos$Sexo<-replace(datos$Sexo,datos$Sexo==0,"H") #solo para reemplazar números
datos$Sexo
datos$Sexo<-replace(datos$Sexo,datos$Sexo==1,"M")
datos$Sexo
head(datos)

## Creación de subgrupos #
macho <- subset(datos,datos$Sexo=="M")
hembra <- subset(datos,datos$Sexo=="H")
head(macho)
head(hembra)
subset(macho,macho$Longitud<100)


########## EXPORTAR DATOS ###########
write.csv(macho,file = "macho_tortuga.csv") ##guardar datos creados en formato .csv
# exportamos macho en formato tabular separado por punto y coma. 
library("readr")
write_delim(macho, "macho_tortuga_2.txt", delim = ";")
# exportamos macho en formato tabular separado por tabuladores 
write_delim(macho, "macho_tortuga_3.txt", delim = "\t")
# exportamos macho en formato tabular separado por un espacio en blanco 
write_delim(macho, "macho_tortuga_4.txt", delim = " ")