## TRABAJO FINAL ##

#1. Cargar datos

setwd("C:/Users/pc/Desktop/Diseños Experimentales/TRABAJO FINAL")

library(readxl)
library(agricolae)

data <- read_excel("BASE DE DATOS.xlsx")
data <- data.frame(data)
class(data)
attach(data)

#2. Declaración de variables

FACTORA <- factor(Fungicidad)
FACTORB <- factor(Rhizobium)
RPTA <- as.numeric(Nod)

#3. Creacion del modelo

mod <- lm(RPTA ~ FACTORA + FACTORB + FACTORA:FACTORB)

#4. ANOVA

anova(mod)

#5. Evaluacion de supuestos

#5.1 Normalidad

ri <- rstandard(mod)
shapiro.test(ri)

#5.2 Homogeneidad de varianzas

library(car)
ncvTest(mod)

# 5.3 Grafica de supuestos

par(mfrow=c(1,2))
plot(mod)

#6. Comparacion por niveles de Fisher

gl <- df.residual(mod)
cm <- deviance(mod)/gl

fisher <- LSD.test(RPTA, FACTORA:FACTORB, gl, cm)
fisher

#7. Grafica de interaccion

par(mfrow=c(1,1))
interaction.plot(FACTORA,FACTORB,RPTA, xlab = "Fungicida", 
                 ylab="Cantidad de Nodulos", 
                 main="Efecto de interaccion entre Fungicida e Inoculante\n en la cantidad de Nodulos", 
                 trace.label = "Inoculante")

#8. PostHoc Tukey

library(multcompView)
library(lsmeans)
library(multcomp)

marginal <- lsmeans(mod,~ FACTORA:FACTORB)
marginal
CLD <- cld(marginal, alpha   = 0.05, Letters = letters,  adjust  = "tukey")  
CLD

#9. Graficos 

library(tidyverse)
install.packages("ggthemes")
library(ggthemes)

#1. Barras de color

ggplot(CLD, aes(x = FACTORA, y = lsmean, color = FACTORB, label = .group)) + 
  xlab("Fungicida") +  ylab("Cantidad de Nodulos") +
  scale_color_hue(name="Inoculante")+  
  geom_bar(position = position_dodge(), stat = "identity", fill = "white") +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), width = .2, position = position_dodge(.9))+
  theme_minimal() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))+
  geom_text(nudge_x = c(-0.22, 0.22, 0.22, -0.25),    
            nudge_y = rep(5, 5),
            color   = "black") +
  coord_cartesian(ylim = c(20, 90)) +  
  ggtitle("Medias de Nodulos con Error Estandar")

#2. Barras escala de grises

ggplot(CLD, aes(x = FACTORA, y = lsmean, fill = FACTORB, label = .group)) + 
  xlab("Fungicida") +  ylab("Cantidad de Nodulos") +
  scale_fill_grey() +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), width = .2, position = position_dodge(.9))+
  labs(fill = "Inoculante")+
  theme_minimal() +
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))+
  geom_text(nudge_x = c(-0.22, 0.22, 0.22, -0.25),    
            nudge_y = rep(5, 5),
            color   = "black") +
  coord_cartesian(ylim = c(20, 90)) +  
  ggtitle("Medias de Nodulos con Error Estandar")


##### PARTE 2 #####

#1. Cargar datos

setwd("C:/Users/pc/Desktop/Diseños Experimentales/Diseños")

data1 <- read_excel("data_diseños.xlsx")
data1 <- data.frame(data1)
class(data1)
attach(data1)

#2. Declaración de variables

tratamiento <- factor(suelo)
rpta <- as.numeric(porcentaje)

#3. Creacion del modelo

mod1 <- lm(rpta ~ tratamiento)

#4. ANOVA

anova(mod1)

#5. Evaluacion de supuestos

#5.1 Normalidad

ri1 <- rstandard(mod1)
shapiro.test(ri1)

#5.2 Homogeneidad de varianzas

library(car)
ncvTest(mod1)

# 5.3 Grafica de supuestos

par(mfrow=c(1,2))
plot(mod1)

#6. Comparacion por niveles de Fisher

gl1 <- df.residual(mod1)
cm1 <- deviance(mod1)/gl1

fisher1 <- LSD.test(rpta, tratamiento, gl1, cm1)
fisher1

#7. Grafica de efectos

par(mfrow=c(1,1))
library(effects)
plot(allEffects(mod1), xlab = "Tratamientos", ylab = "Porcentaje de sobrevivencia",
     main = "Efecto del tipo de suelo sobre el porcentaje de sobrevivencia")

#8. PostHoc Tukey

marginal1 <- lsmeans(mod1,~ tratamiento)
marginal1
CLD1 <- cld(marginal1, alpha   = 0.05, Letters = letters,  adjust  = "tukey")  
CLD1

#9. Graficos 

#1. Intervalos de confianza

ggplot(CLD1, aes(x = tratamiento, y = lsmean, label = .group)) +
  geom_point(shape  = 15, size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax  =  upper.CL), width = 0.2, size  =  0.7)  +
  ylab("Porcentaje de sobrevivencia") +
  theme_minimal()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(5, 5, 5),  
            color   = "black") +  
  ggtitle("Medias de porcentaje de sobrevivencia con Intervalos de Confianza al 95%") 

#2. Escala de grises

ggplot(CLD1, aes(x = tratamiento, y = lsmean, fill = tratamiento, label = .group)) + 
  xlab("Tratamientos") +  ylab("Porcentaje de sobrevivencia") +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lsmean-SE, ymax = lsmean+SE), width = .2, position=position_dodge(.9))+
  scale_y_continuous(limits = c(0,80)) +
  scale_fill_grey()+
  theme_minimal()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(5, 5, 5),  
            color   = "black") +  
  ggtitle("Medias de porcentaje de sobrevivencia con Error Estandar") +
  guides(fill = F) 
  

#3. Barras de colores

ggplot(CLD1, aes(x = tratamiento, y = lsmean, color = tratamiento, label = .group)) + 
  xlab("Tratamientos") +  ylab("Porcentaje de sobrevivencia") +
  geom_bar(stat = "identity", fill = "white") +
  geom_errorbar(aes(ymin = lsmean-SE, ymax = lsmean+SE), width = .2, position=position_dodge(.9))+
  theme_minimal()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(5, 5, 5),  
            color   = "black") + 
  coord_cartesian(ylim = c(0, 80)) +
  ggtitle("Medias de porcentaje de sobrevivencia con Error Estandar") +
  guides(color = F)

#4. Puntos con error estandar

ggplot(CLD1, aes(x = tratamiento, y = lsmean, label = .group)) + 
  xlab("Tratamientos") +  ylab("Porcentaje de sobrevivencia") +
  geom_point(shape = 15, size = 3) +
  geom_errorbar(aes(ymin = lsmean-SE, ymax = lsmean+SE), width = .2, position=position_dodge(.9))+
  theme_minimal()       +  
  theme(axis.title   = element_text(face = "bold"),
        axis.text    = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0)) +
  geom_text(nudge_x = c(0, 0, 0),
            nudge_y = c(3, 3, 3),  
            color   = "black") + 
  ggtitle("Medias de porcentaje de sobrevivencia con Error Estandar")


