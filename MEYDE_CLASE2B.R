#############################################################
###################### Probabilidades #######################
#############################################################

######################
#Distribución Uniforme
######################
#Crear dados
dado<-1:6
sample(dado,1,replace=T)

#Principales Distribuciones Discretas

#d  density  P(X=a)   a es un entero
#p  probability P(X<=a)
#r: random 
######################
#Distribución Binomial (con reemplazo) porque cada evento tiene probabilidad constante
######################
#Examen de 10 preguntas cada una con 5 alternativas (2ptos cada pregunta)
#¿Cuál es la probabilidad que un estudiante apruebe?

#X:Número de preguntas correctamente respondidas 
#X~Bin(n=10,pi=1/5)
#P(X=0) probabilidad de que todas las respuestas sean incorrectas
dbinom(0,10,1/5)
#P(X<=4) probabilidad de que a lo más 4 respuestas sean correctas
dbinom(0,10,1/5)+dbinom(1,10,1/5)+dbinom(2,10,1/5)+dbinom(3,10,1/5)+dbinom(4,10,1/5)
pbinom(4,10,1/5)
#P(X<3)=p(X<=2)  probabilidad de que a haya menos de 3 respuestas correctas
dbinom(0,10,1/5)+dbinom(1,10,1/5)+dbinom(2,10,1/5)
pbinom(2,10,1/5)
#P(X>3)=1-P(X<=3) casi siempre los software acumulan de izquierda a derecha por 
#defecto, se usa lower.tail=F para analizar cola superior (se empieza desde la derecha)
dbinom(10,10,1/5)+dbinom(9,10,1/5)+dbinom(8,10,1/5)+dbinom(7,10,1/5)+dbinom(6,10,1/5)+dbinom(5,10,1/5)+dbinom(4,10,1/5)
1-pbinom(3,10,1/5)
pbinom(3,10,1/5,lower.tail = F)
#P(X>5)=1-P(X<=5) probabilidad que apruebe
1-pbinom(5,10,1/5)
pbinom(5,10,1/5,lower.tail = F)

#### Gráfico de probabilidades #
#E(X)=n(pi) =10(1/5)=2
px<-dbinom(0:10,10,1/5)
x<-0:10
#V(X)=npi(1-pi)=10(1/5)(4/5)
plot(x,px,type="h")

#############################
#Distribución Hipergeométrica (sin reemplazo)
#############################
# El profe Canchucaja realiza una inspección, elige 15 durmientes (N),de ellas 8 
# son defectuosos (A), el profe selecciona al azar 5 durmientes (n)
# Para que el lote sea rechazado no debe haber ningún durmiente defectuoso
# X: número de durmientes defectuosos
#N=15 , A=8   n=5
#¿Cuál es la probabilidad de que el lote sea rechazado?
#P(X=>1)=1-P(X<1)=1-P(X=0)  en R hyper(x,A,N-A,n)
1-dhyper(0,8,7,5)
1-phyper(0,8,7,5)
#E(X)=n(A/N)=5(8/15)=2.66

########################
#Distribución de Poisson
########################
# En una campaña de aprovechamiento forestal se quiere evaluar la probabilidad de 
# ocurrencia de accidentes
#X: Número de accidentes/semana (14)  X~Poi(14)
#P(X>10)=1-p(X<=10)
1-ppois(10,14)
ppois(10,14,lower.tail = F)
#P(X>16)
ppois(16,14,lower.tail = F)

#Y: Número de accidentes/día (2) Y~Poi(2)
#P(Y<=3) Probabilidad de que ocurra a lo más 3 accidentes al día
ppois(3,2)

########################
#Distribución Geométrica
########################
# La probabilidad de éxito al lanzar un cohete es 0.8, suponiendo que el ensayo 
# del lanzamiento ha ocurrido.
#¿Cuál es la probabilidad que exactamente sean necesarios 6 ensayos?
#X: Numero de ensayos realizados hasta que el cohete sea lanzado.
#P(X=6)
dgeom(6-1,0.8)
# E(X)= 1/pi

###############################
#Distribución Binomial Negativa
###############################
# En control de calidad el 60% de los productos presentan altos estándares de calidad. 
# Se necesitan 4 de estos productos con altos estándares. 
# Determinar la probabilidad de que al seleccionar 7 productos se encuentren 4 de 
# ellos con la característica de interés.

#P(X=7)  X~BN(r=4,pi=0.6)
dnbinom(7-4,4,0.6)


#########################
#Distribuciones Continuas

#p : probability P(X<=a)
#q : quantile    P(X<a)=p

######################
#Distribución Uniforme
######################
# En un aserradero, un equipo corta una troza cada de 8 minutos  
# El jefe de operaciones llega de improviso a la planta. Si X es la 
# variable aleatoria que representa el tiempo de corta de una troza. Halle la 
# probabilidad de que el jefe vea una troza completamente cortada en más de 2 minutos.
#P(X>2)=P(X<2)=P(X<=2) es igual porque la integral en un punto es 0, que contenga 
# o no a 2 es lo mismo
1-punif(2,0,8)
punif(2,0,8,lower.tail = F)

#########################
#Distribución Exponencial
#########################
# La vida de cierto tipo de tubos electrónicos tiene una distribución exponencial 
# con vida media de 500 horas. Si se selecciona un tubo al azar, hallar la 
# probabilidad que se queme antes de las 300 horas
#X~Exp(beta=500)
pexp(300,1/500)
#p(x>600)
1-pexp(600,1/500)
pexp(600,1/500,lower.tail = F)
#¿Cuál es el tiempo mínimo de vida que debe tener un tubo para estar comprendido 
#dentro del 70% de mayor duración?
#P(X<a)=0.3
qexp(0.3,1/500)
qexp(0.7,1/500,lower.tail = F)

#¿Cuál es el tiempo máximo de vida que debe tener un tubo para estar comprendido 
# dentro del 20% de menor duración?
qexp(0.2,1/500)

####################
#Distribución Normal
####################
# La duración de un proyecto de construcción de una casa de madera  está normalmente 
# distribuida con media de 85 días y desviación estándar de 5 días. Si se selecciona 
# un proyecto ¿cuál es la probabilidad que tenga una duración menor a los 75 días?
#X~N(mu=85,sigma=5)
#P(X<75)
pnorm(75,85,5)
# Probabilidad de que la construcción dure entre 73 y 84 días
#P(73<X<84)=P(X<84)-P(X<73)
pnorm(84,85,5)-pnorm(73,85,5)

#¿Cuánto debe durar un proyecto como mínimo para estar dentro del 10% de 
#proyecto de mayor duración?
qnorm(0.9,85,5)
qnorm(0.1,85,5,lower.tail = F)

###########################################
#Distribuciones Chi Cuadrado, t y de Fisher
###########################################
#Chi Cuadrado
#X~Chi(15)
#P(X>5.6)
pchisq(5.6,15,lower.tail = F)
1-pchisq(5.6,15)
# si cae entre 6.26 y 27.48 no se rechaza la hipótesis
#Chi(0.975,15)
qchisq(0.975,15)
#Chi(0.025,15)
qchisq(0.025,15)

#t de Student
#X~t(18)
#P(X<1.86)
pt(1.86,18)

#Simetría en la distribución t Student
#t(0.95,18) 
qt(0.95,18)
qt(0.05,18)

#F de Fisher
#X~F(12,14)
#P(2.1<X<3.8)=P(X<3.8)-P(X<2.1)
pf(3.8,12,14)-pf(2.1,12,14)

# F(0.3,12,14)
qf(0.3,12,14)