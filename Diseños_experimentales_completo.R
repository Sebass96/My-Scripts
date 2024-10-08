####################################################
############# Dise�os experimentales ###############
####################################################


setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTAD�SICO/MEYDE_P1/Dise�os experimentales")
setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/ESTADISTICA FORESTAL/7) Dise�os experimentales")
######################################
#crear dise�o experimental: DCA o DCR#
######################################

tratamientos <- c("T1","T2","T3","T4","T5","T6","T7")

library(agricolae)
DCA <- design.crd(tratamientos, r=7, serie = 2, seed = 0, kinds = "Super-Duper",
                  randomization=TRUE)
dise�o_dca <- DCA$book
library(readr)
write_delim(dise�o_dca,"dise�o_dca.txt", delim=",")

########################################
#crear dise�o experimental: DBCA o DBCR#
########################################
library(agricolae)
DBCA <- design.rcbd(tratamientos, r=7, serie = 2, seed = 0, kinds = "Super-Duper", 
                    randomization=TRUE )
dise�o_DBCA <- DBCA$book
write.table(dise�o_DBCA,"dise�o_dbca.txt",row.names=FALSE, sep="\t")

################################
#crear dise�o experimental: DCL#
################################
library(agricolae)
DCL <- design.lsd(tratamientos, serie = 2, seed = 0, kinds = "Super-Duper",randomization=TRUE)
dise�o_dcl <- DCL$book
write.table(dise�o_dcl,"dise�o_dcl.txt",row.names=FALSE, sep="\t")

###########################################
############An�lisis en DCA################
###########################################
library(readxl)
#Porcentaje de caobas vivas
data_dca <- read_excel("data_dise�os.xlsx",sheet=1)
head(data_dca)
str(data_dca)

data_dca$suelo<- factor(data_dca$suelo)
str(data_dca)
summary(data_dca)
# Creaci�n de objetos
suelo <- data_dca$suelo
porcentaje <- data_dca$porcentaje 

########################
## Analisis de Variancia
########################
mod <- lm(porcentaje~suelo, data_dca) #variable numerecica(%) en funcion del tratamiento
summary(aov(mod))
# otra forma
anva <- aov(mod)
summary(anva)


############################
## Cumplimiento de supuestos
############################
# Parte la pantalla en 4 partes
par(mfrow=c(2,2))
# grafica los 4 modelos
plot(mod)


# Residuales Estandarizados
ri<-rstandard(mod)
sort(ri)

# Test de Shapiro-Wilk
shapiro.test(ri)

# Test de Anderson Darling
library(nortest)
ad.test(ri)

# Test de Barlett (homogeneidad de varianzas)
library(car)
bartlett.test(porcentaje~suelo,data_dca)

# Test de Levene
library(car)
leveneTest(porcentaje~suelo,data_dca)

# Test de Score para variancia del error no constante
library(car)
ncvTest(mod)
#recomendada# se puede usar con todos los dise�os

##########################
## Comparaciones m�ltiples
##########################

# === Prueba de DMS === (dif de minima significaci�n)
#Todos contra todos
pairwise.t.test(porcentaje,suelo,p.adjust.method="none",alternative = "t")
#comparaci�n planeada
head(data_dca,n=20)
B <- data_dca[5:10,] #B tiene 6 datos
C <- data_dca[11:16,] #C tiene 6 datos
data_BVSC <- rbind(B,C) #uno por filas
data_BVSC
porcentajebc <- data_BVSC$porcentaje
suelobc <- data_BVSC$suelo
pairwise.t.test(porcentajebc,suelobc,p.adjust.method="none",alternative = "t")

# === Prueba de Tukey-Cramer ===

## directamente
mod<-aov(porcentaje~suelo,data=data_dca)
library(multcomp)
cht <- glht(mod, linfct = mcp(suelo = "Tukey"))
summary(cht)

## Otra Forma
### directamente
mod<-aov(porcentaje~suelo,data=data_dca)
tukey <- TukeyHSD(mod,"suelo")
tukey
par(mfrow=c(1,1))
plot(tukey)


# === Prueba de Dunnett === dunet considera como testigo al primer tratamiento de los datos
library(multcomp)

mod<-aov(porcentaje~suelo,data=data_dca)
cdu <- glht(mod, linfct = mcp(suelo = "Dunnett"))
summary(cdu)

# === Prueba de Duncan ===
library(agricolae)
mod<-aov(porcentaje~suelo,data=data_dca)

df<-df.residual(mod) #grados de libertad
MSerror<-deviance(mod)/df #cuadrado del error
out <- with(data_dca,duncan.test(porcentaje,suelo,df,MSerror, group=TRUE))

plot(out,variation="IQR",horiz=TRUE,las=1)
print(out$groups)
duncan.test(mod, "suelo",alpha=0.05,console=TRUE, group=F)
#############################
#Prueba no param�trica en DCA
#############################
# Se pide la opini�n de especialistas con criterios similares
#de calificaci�n de est�ndares de calidad del servicio 
#ecotur�stico 
# === Prueba de Kruskal-Wallis ===
library(readxl)
data_KW <- read_excel("data_dise�os.xlsx",sheet=2)
str(data_KW)
head(data_KW,n=12)
calificacion<-data_KW[,1] #primera columna
metodo<-data_KW[,2] #cuidado al no convertir en factor
str(metodo)
library(agricolae)
kruskal(calificacion,metodo, console=T)
kruskal(calificacion,metodo,group=FALSE, console=T)


############################################
############An�lisis en DBCA################
############################################
# Se evalu� el rendimiento de cinco tipos de aditivos de 
# carga (Km/gal�n) en cinco veh�culos
#
library(readxl)
data_dbca <- read_excel("data_dise�os.xlsx",sheet=3)
# Estructura del archivo
head(data_dbca, n=10)
str(data_dbca)
rendimiento<-data_dbca$rendimiento
vehiculo <- as.factor(data_dbca$bloques)
tipos<-as.factor(data_dbca$aditivo)

#ANVA
mod <-lm(rendimiento~tipos+vehiculo)
anva<-anova(mod)
anva

# Parte la pantalla en 4 partes
par(mfrow=c(2,2))

plot(mod)

#Cumplimiento de supuestos #considerar un ns de 10%
library(car)
ncvTest(mod)

ri<-rstandard(mod)
shapiro.test(ri)

# Eficiencia Relativa

library(readxl)
data_dbca <- read_excel("data_dise�os.xlsx",sheet=3)
data_dbca$bloques <- as.factor(data_dbca$bloques)
modeg<-lm(rendimiento~bloques+aditivo,data_dbca)
anva<-anova(modeg)
anva
cm<-anva$`Mean Sq` # cuadrado medio de los errores (3)
sc<-anva$`Sum Sq` # suma de cuadrado de los errores (3)
nt<-tapply(data_dbca$rendimiento,data_dbca$bloques,length)
t<-nt[1] #n�mero de tratamientos
nb<-tapply(data_dbca$rendimiento,data_dbca$bloques,length)
b<-nb[1] #n�mero de bloques
# Eficiencia Relativa
ER<-((sc[1]+b*(t-1)*cm[3])/(t*b-1))/cm[3]
ER #Si ER > ???1 entonces el Dise�o de Bloques Completos al
#Azar es m�s eficiente que un Dise�o Completamente al azar.

##  === Comparaciones Multiples ===
### Prueba DMS

#comparaci�n entre aditivo D y E
modeg<-lm(rendimiento~vehiculo+tipos)
modeg

mediat<-tapply(rendimiento,tipos,mean)
mediat
mediaD<-mediat[4]
mediaE<-mediat[5]
esdmedia<-sqrt(2*cm[3]/b) #estad�stico de DMS (Sd)
esdmedia
tc<-(mediaD-mediaE-0)/esdmedia # 0 es porque se plantea que son iguales D-E=0
tc

pvalue<-2*pt(tc,df.residual(modeg))
pvalue

### Prueba de Tukey
library(multcomp)
anva<-aov(rendimiento~vehiculo+tipos)
comptipos<-glht(anva,linfct=mcp(tipos="Tukey"))
confint(comptipos)
summary(comptipos)

### Prueba de Duncan
mod2<-lm(rendimiento~tipos+vehiculo)
ANOVA <- aov(mod2)
library(agricolae)
duncan.test(y = ANOVA, trt = "tipos", group = T, console = T)
duncan.test(y = ANOVA, trt = "tipos", group = F, console = T)


### Prueba de Dunnett
library(multcomp)
anva<-aov(rendimiento~vehiculo+tipos)
comptipos<-glht(anva,linfct=mcp(tipos="Dunnett"))
confint(comptipos)
summary(comptipos)

### Prueba de Friedmann
# Se pide la opini�n de especialistas de los que se sospecha
# que no tienen criterios similares en la calificaci�n de 
# est�ndares de calidad del servicio ecotur�stico

data_F <- read_excel("data_dise�os.xlsx",sheet=4)
str(data_F)
juez <- factor(data_F$juez)
marca <- factor(data_F$marca)
calificacion <-  data_F$calificaci�n
library(agricolae)
comparison<-friedman(juez,marca,calificacion,alpha=0.05,
                     group=FALSE) #(bloque,en funcion, variable respuesta)
comparison
## Otra forma
friedman(juez,marca,calificacion,alpha=0.05,
         group=T, console=T)

############################################
############An�lisis en DCL ################
############################################

data_dcl <- read_excel("data_dise�os.xlsx",sheet=5)
str(data_dcl)
head(data_dcl)
data_dcl$fila<-factor(data_dcl$fila)
data_dcl$col<-factor(data_dcl$col)
data_dcl$trat<-factor(data_dcl$trat)
str(data_dcl)
#ANVA
mod<-lm(rend~.,data=data_dcl)
anva<-anova(mod)
anva
par(mfrow=c(2,2))
plot(mod)
#Comprobaci�n de supuestos
ri<-rstandard(mod)
shapiro.test(ri)

library(car)
ncvTest(mod)
#Comparaciones m�ltiples
library(multcomp)
amod<-aov(rend~.,data=data_dcl)
comptrat<-glht(amod,linfct=mcp(trat="Tukey"))
summary(comptrat)

amod1<-aov(rend~fila+col+trat,data=data_dcl)
comptrat1<-glht(amod1,linfct=mcp(trat="Tukey"))
summary(comptrat)

library(agricolae)
anva<-aov(mod)
compd<-duncan.test(anva,"trat")
compd

compd1<-duncan.test(anva,"trat",alpha=0.05,console=TRUE, group=F)
