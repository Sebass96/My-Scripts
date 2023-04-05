##############################################
######### Experimento Factorial###############
##############################################

setwd("C:/JORDAN/TRABAJO/FORESTAL_ESTADÍSICO/MEYDE_P1/Experimento factorial")
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/9)Experimento factorial")
abono1<-read.table("abono1.txt",T)
abono1

# Creación del modelo
rendimiento<-abono1[,1]
A<-abono1[,2]
B<-abono1[,3]
campo<-factor(abono1[,4])
mod<-lm(rendimiento~campo+A+B+A*B)
#mod1 <- lm(rendimiento~campo + A*B)
#anva<-anova(mod1)
#anva
#Interacción
rendimiento<-abono1$Y
tipo<-abono1$A
dosis<-abono1$B
interaction.plot(tipo,dosis,rendimiento)
interaction.plot(dosis,tipo,rendimiento)

#Análisis de varianza
anva1<-anova(mod)
anva1


# Efecto simple
#Ab1 (se mantiene constante el b1)
abono<-data.frame(rendimiento,campo,A,B)
abonoab1<-abono[abono$B=="b1",] #subgrupo
abonoab1

modab1<-lm(rendimiento~campo+A,abonoab1)
anvaab1<-anova(modab1)
anvaab1

#Ab2
abonoab2<-abono[abono$B=="b2",]
abonoab2<-abono[abono$B=="b2",]
abonoab2

modab2<-lm(rendimiento~campo+A,abonoab2)
anvaab2<-anova(modab2)
anvaab2

#Ab3
abonoab3<-abono[abono$B=="b3",]
abonoab3

modab3<-lm(rendimiento~campo+A,abonoab3)
anvaab3<-anova(modab3)
anvaab3

#Ba1
abonoba1<-abono[abono$A=="a1",]
abonoba1

modba1<-lm(rendimiento~campo+B,abonoba1)
anvaba1<-anova(modba1)
anvaba1

#Ba2
abonoba2<-abono[abono$A=="a2",]
abonoba2

modba2<-lm(rendimiento~campo+B,abonoba2)
anvaba2<-anova(modba2)
anvaba2


### Prueba de Tukey
anva3<-aov(mod)
tHSD <- TukeyHSD(anva3)
print(tHSD)


