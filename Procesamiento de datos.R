#lectura de datos#
setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/TEST/CARTAS Y DOCUMENTOS")
###Condicion de estaquillas###
library(readxl)
data_condicion <-read_excel("Datos_Tesis.xlsx")
head(data_condicion)
str(data_condicion)
summary(data_condicion)
data_condicion$tratamiento <-factor(data_condicion$tratamiento)
str(data_condicion)
summary(data_condicion)
#Tabla de contingencia (Cantidad de estaquillas por condición)
table <-table(data_condicion$enraizamiento,data_condicion$tratamiento)
table
#mosaico
library(vcd)
x11()
mosaicplot(~data_condicion$tratamiento+data_condicion$enraizamiento, 
           data_condicion,  col = c("goldenrod", "forestgreen", "black"),
           main = "Características de estaquillas de Eucalipto Urograndis por dosis de AIB", xlab="Tratamientos", ylab="Estaquillas")

##Enraizamiento##
enraizadas <- subset(data_condicion, enraizamiento == "Enraizada")
head(enraizadas)
str(enraizadas)
ne <- table(enraizadas)
ne
fe <- ne/90
fe
fpe <- round(fe*100,2)
fpe
#Grafico de barras
install.packages("tidyverse")
library(tidyverse)
library(scales)
library(ggplot2)
df <- as.data.frame(table(enraizadas$tratamiento))  
df
names(df) <- c("tratamiento","enraizamiento")
df
df$porcentaje <- round(df$enraizamiento/90,4)
df$percent <- df$porcentaje*100
df
#Cantidad
x11()
ggplot(df, aes(x=tratamiento, y= enraizamiento, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=enraizamiento),vjust= -0.3)+
  theme_classic()+
  labs(title = "Cantidad de estaquillas de Eucalipto Urograndis enraizadas por dosis de AIB")+
  labs(x = "Tratamientos", y= "Cantidad de estaquillas (unidades)")+
  theme(legend.position = "none")
#Porcentaje
x11()
ggplot(df, aes(x=tratamiento, y= percent, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent((porcentaje))),vjust= -0.3)+
  theme_classic()+
  labs(title = "Porcentaje de enraizamiento de estaquillas de Eucalipto Urograndis por dosis de AIB")+
  labs(x = "Tratamientos", y= "Porcentaje de enraizamiento (%)")+
  theme(legend.position = "none")

##Mortandad##
muertas <- subset(data_condicion, enraizamiento == "Muerta")
muertas
str(muertas)
nm <- table(muertas)
nm
fm <- nm/90
fm
fpm <- round(fm*100,2)
fpm
#Grafico de barras
dfm <- as.data.frame(table(muertas$tratamiento))  
dfm
names(dfm) <- c("tratamiento","mortandad")
dfm
dfm$porcentaje <- round(dfm$mortandad/90,4)
dfm$percent <- dfm$porcentaje*100
dfm
#Cantidad
x11()
ggplot(dfm, aes(x=tratamiento, y= mortandad, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=mortandad),vjust= -0.3)+
  theme_classic()+
  labs(title = "Cantidad de estaquillas de Eucalipto Urograndis muertas por dosis de AIB")+
  labs(x = "Tratamientos", y= "Cantidad de estaquillas (unidades)")+
  theme(legend.position = "none")
#Porcentaje
x11()
ggplot(dfm, aes(x=tratamiento, y= percent, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent((porcentaje))),vjust= -0.3)+
  theme_classic()+
  labs(title = "Porcentaje de mortandad de estaquillas de Eucalipto Urograndis por dosis de AIB")+
  labs(x = "Tratamientos", y= "Porcentaje de mortandad (%)")+
  theme(legend.position = "none")

#Presencia de callo
callosidad <- subset(data_condicion, enraizamiento == "Callo")
nc <- table(callosidad)
nc
str(callosidad)
fc <- nc/90
fc
fpc <- round(fc*100,2)
fpc
#Grafico de barras
dfc <- as.data.frame(table(callosidad$tratamiento))  
dfc
names(dfc) <- c("tratamiento","callosidad")
dfc
dfc$porcentaje <- round(dfc$callosidad/90,4)
dfc$percent <- dfc$porcentaje*100
dfc
#Cantidad
x11()
ggplot(dfc, aes(x=tratamiento, y= callosidad, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=callosidad),vjust= -0.3)+
  theme_classic()+
  labs(title = "Cantidad de estaquillas de Eucalipto Urograndis con presencia de callo por dosis de AIB")+
  labs(x = "Tratamientos", y= "Cantidad de estaquillas (unidades)")+
  theme(legend.position = "none")
#Porcentaje
x11()
ggplot(dfc, aes(x=tratamiento, y= percent, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent((porcentaje))),vjust= -0.3)+
  theme_classic()+
  labs(title = "Porcentaje de estaquillas de Eucalipto Urograndis con presencia de callo por dosis de AIB")+
  labs(x = "Tratamientos", y= "Porcentaje de callosidad (%)")+
  theme(legend.position = "none")
#Resumen de condicion
library(ggpubr)
library(ggplot2)
#Cantidad
x11()
c1 <-ggplot(df, aes(x=tratamiento, y= enraizamiento, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=enraizamiento),vjust= 1)+
  theme_classic()+
  labs(title = "Enraizamiento")+
  labs(x = "Tratamientos", y= "Cantidad de estaquillas (unidades)")+
  theme(legend.position = "none")
c2 <-ggplot(dfm, aes(x=tratamiento, y= mortandad, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=mortandad),vjust= 1)+
  theme_classic()+
  labs(title = "Mortandad")+
  labs(x = "Tratamientos", y= "Cantidad de estaquillas (unidades)")+
  theme(legend.position = "none")
c3 <-ggplot(dfc, aes(x=tratamiento, y= callosidad, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=callosidad),vjust= 1)+
  theme_classic()+
  labs(title = "Presencia de callo")+
  labs(x = "Tratamientos", y= "Cantidad de estaquillas (unidades)")+
  theme(legend.position = "none")
ggpubr :: ggarrange(c1,c2,c3, ncol = 1)
#Porcentaje
x11()
p1 <-ggplot(df, aes(x=tratamiento, y= percent, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent((porcentaje))),vjust= 1.1)+
  theme_classic()+
  labs(title = "Porcentaje de enraizamiento")+
  labs(x = "Tratamientos", y= "Porcentaje de enraizamiento (%)")+
  theme(legend.position = "none")
p2 <-ggplot(dfm, aes(x=tratamiento, y= percent, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent((porcentaje))),vjust= 1.1)+
  theme_classic()+
  labs(title = "Porcentaje de mortandad")+
  labs(x = "Tratamientos", y= "Porcentaje de mortandad (%)")+
  theme(legend.position = "none")
p3 <-ggplot(dfc, aes(x=tratamiento, y= percent, fill=tratamiento))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent((porcentaje))),vjust= 1.1)+
  theme_classic()+
  labs(title = "Porcentaje de callosidad")+
  labs(x = "Tratamientos", y= "Porcentaje de callosidad (%)")+
  theme(legend.position = "none")
ggpubr :: ggarrange(p1,p2,p3, ncol = 1)

####Numero de raices####
data_numero <-read_excel("Datos_Tesis.xlsx", sheet = 2)
head(data_numero)
str(data_numero)
summary(data_numero)
data_numero$Tratamiento <-factor(data_numero$Tratamiento)
str(data_numero)
summary(data_numero)
nraices <-data_numero$`Número de raices (u)`
nraices
tratamiento_n <-data_numero$Tratamiento
tratamiento_n

#Estadistica descriptiva#
library(mosaic)
estadisticos <- favstats(nraices~tratamiento_n, data=data_numero)
estadisticos
estadisticos$cv <- estadisticos$sd*100/estadisticos$mean
estadisticos
estadisticos_n <-estadisticos[,-10]
estadisticos_n

#Grafico de bastones
#Frecuencia numero de raíces
x11()
f2<- table(nraices)
f2
plot(f2, ylim=c(0,60),font.lab=2,font.main=2,col= "blue3", ylab = "Frecuencia Absoluta",
     xlab = "Número de raíces", main="Gráfico de varas")
#Frecuencia numero de raíces por tratamiento
fp <- as.data.frame(table(data_numero$`Número de raices (u)`, data_numero$Tratamiento)) 
fp
names(fp) <- c("numero","tratamiento","frecuencia")
fp
x11()
ggplot(fp, aes(x=tratamiento,y =frecuencia,fill=numero))+
  geom_bar(stat="identity", position="dodge",width = 0.7)+
  theme_classic()+
labs(title = "Número de raíces por tratamiento")+
  labs(x = "Tratamientos", y= "Frecuencia Absoluta")+
  labs(fill="Número de raíces")

#Histograma
library(ggplot2)
#General
x11()
ggplot(data_numero, aes(x=nraices))+
  geom_histogram(aes(y=..density..), binwidth =1, 
                 fill="white",colour="black")+
  geom_density(alpha=.2, fill= "#FF6666")+
  geom_vline(aes(xintercept=mean(nraices)), 
             colour="red", linetype="dashed",size=1)+
  labs(title = "Histograma de número de raíces desarrolladas por las estaquillas de Eucalipto Urograndis")+
  labs(x = "Número de raíces (unidades)", y= "Frecuencia Relativa")+
  theme_classic()
#Por tratamiento
x11()
ggplot(data_numero)+ geom_histogram(aes(x=nraices, fill=tratamiento_n, colour="black")
                                    ,bins=6, position = "identity",alpha = 0.5)+
  facet_grid(tratamiento_n)+
  labs(title = "Histograma de número de raíces desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  labs(x = "Número de raíces (unidades)", y= "Frecuencia Absoluta")+
  theme_classic()+
  theme(legend.position = "none")

##Analisis##
#ANOVA#
#Supuestos#
mod_n <- lm(nraices~tratamiento_n,data_numero)
anva_n <- aov(mod_n)
summary(anva_n)
#Grafica
x11()
par(mfrow=c(2,2))
plot(mod_n)
# Residuales Estandarizados
ri <- rstandard(mod_n)
sort(ri)
# Test de Shapiro-Wilk (Normalidad)
shapiro.test(ri)
# Test de Barlett (homogeneidad de varianzas)
library(car)
bartlett.test(nraices~tratamiento_n,data_numero)
ncvTest(mod_n)
leveneTest(nraices~tratamiento_n,data_numero)
#Prueba no paramétrica en DCA
# === Prueba de Kruskal-Wallis ===
#Supuestos
install.packages("kSamples")
library(kSamples)
ad.test(nraices~tratamiento_n)
#Test de Kruskal-Wallis
library(readxl)
dataKW_n <-read_excel("Datos_Tesis.xlsx", sheet = 2)
str(dataKW_n)
nraices <-dataKW_n[,1]
tratamiento <-dataKW_n[,2]
str(tratamiento)
library(agricolae)
kruskal(nraices,tratamiento, console=T)
kruskal(nraices,tratamiento,group=FALSE, console=T)

####Longitud de raices####
data_longitud <-read_excel("Datos_Tesis.xlsx", sheet = 3)
head(data_longitud)
str(data_longitud)
summary(data_longitud)
data_longitud$Tratamiento <-factor(data_longitud$Tratamiento)
str(data_longitud)
summary(data_longitud)
lraices <-data_longitud$`Longitud de raíz (cm)`
lraices
tratamiento_l <-data_longitud$Tratamiento
#Estadistica descriptiva#
library(mosaic)
estadisticos1 <- favstats(lraices~tratamiento_l, data=data_longitud)
estadisticos1
estadisticos1$cv <- estadisticos1$sd*100/estadisticos1$mean
estadisticos1
estadisticos_l <-estadisticos1[,-10]
estadisticos_l
#Boxplot
library(ggplot2)
x11()
ggplot(data = data_longitud, mapping = aes(x = tratamiento_l, y = lraices, fill = tratamiento_l)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Longitud de raíces desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  stat_summary(fun = mean, geom = "point", colour = "darkred", size= 1.5)+
  labs(x = "Tratamientos", y= "Longitud de raíces (cm)")+
  theme(legend.position = "none")
#Histograma
library(ggplot2)
#General
x11()
ggplot(data_longitud, aes(x=lraices))+
  geom_histogram(aes(y=..density..), binwidth =1, 
                 fill="white",colour="black")+
  geom_density(alpha=.2, fill= "#FF6666")+
  geom_vline(aes(xintercept=mean(lraices)), 
             colour="red", linetype="dashed",size=1)+
  labs(title = "Histograma de longitud de raíces desarrolladas por las estaquillas de Eucalipto Urograndis")+
  labs(x = "Longitud de raíces (cm)", y= "Frecuencia Relativa")+
  theme_classic()
#Por tratamiento
x11()
ggplot(data_longitud)+ geom_histogram(aes(x=lraices, fill=tratamiento_l, colour="black")
                                    ,bins=6, position = "identity",alpha = 0.5)+
  facet_grid(tratamiento_l)+
  labs(title = "Histograma de longitud de raíces desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  labs(x = "Longitud de raíces (cm)", y= "Frecuencia Absoluta")+
  theme_classic()+
  theme(legend.position = "none")


##Analisis##
#ANOVA#
#Supuestos#
mod_l <-lm(lraices~tratamiento_l,data_longitud)
anva_l <-aov(mod_l)
summary(anva_l)
#Grafica
x11()
par(mfrow=c(2,2))
plot(mod_l)
# Residuales Estandarizados
ri_l <- rstandard(mod_l)
sort(ri_l)
# Test de Shapiro-Wilk (Normalidad)
shapiro.test(ri_l)
# Test de Barlett (homogeneidad de varianzas)
library(car)
bartlett.test(lraices~tratamiento_l,data_longitud)
ncvTest(mod_l)
leveneTest(lraices~tratamiento_l,data_longitud)
#Prueba no paramétrica en DCA
# === Prueba de Kruskal-Wallis ===
#Supuestos
library(kSamples)
ad.test(lraices~tratamiento_l)
#Test de Kruskal-Wallis
library(readxl)
dataKW_l <-read_excel("Datos_Tesis.xlsx", sheet = 3)
str(dataKW_l)
nraices <-dataKW_l[,1]
tratamiento <-dataKW_l[,2]
str(tratamiento)
library(agricolae)
kruskal(lraices,tratamiento_l, console=T)
kruskal(lraices,tratamiento_l,group=FALSE, console=T)

####Peso humedo de raices####
data_ph <-read_excel("Datos_Tesis.xlsx", sheet = 4)
head(data_ph)
str(data_ph)
summary(data_ph)
data_ph$Tratamiento <-factor(data_ph$Tratamiento)
str(data_ph)
summary(data_ph)
ph_raices <-data_ph$`Peso Húmedo de raices (g)`
ph_raices
tratamiento_ph <-data_ph$Tratamiento
#Estadistica descriptiva#
library(mosaic)
estadisticos2 <- favstats(ph_raices~tratamiento_ph, data=data_ph)
estadisticos2
estadisticos2$cv <- estadisticos2$sd*100/estadisticos2$mean
estadisticos2
estadisticos_ph <-estadisticos2[,-10]
estadisticos_ph
#Boxplot
library(ggplot2)
x11()
ggplot(data = data_ph, mapping = aes(x = tratamiento_ph, y = ph_raices, fill = tratamiento_ph)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Peso húmedo de raíces desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  stat_summary(fun = mean, geom = "point", colour = "darkred", size= 1.5)+
  labs(x = "Tratamientos", y= "Peso húmedo de raíces (g)")+
  theme(legend.position = "none")
#Histograma
library(ggplot2)
#General
x11()
ggplot(data_ph, aes(x=ph_raices))+
  geom_histogram(aes(y=..density..), binwidth =0.05, 
                 fill="white",colour="black")+
  geom_density(alpha=.2, fill= "#FF6666")+
  geom_vline(aes(xintercept=mean(ph_raices)), 
             colour="red", linetype="dashed",size=1)+
  labs(title = "Histograma de peso húmedo desarrolladas por las estaquillas de Eucalipto Urograndis")+
  labs(x = "Peso húmedo de raíces (g)", y= "Frecuencia relativa")+
  theme_classic()
#Por tratamiento
x11()
ggplot(data_ph)+ geom_histogram(aes(x=ph_raices, fill=tratamiento_ph, colour="black")
                                      ,bins=6, position = "identity",alpha = 0.5)+
  facet_grid(tratamiento_ph)+
  labs(title = "Histograma de peso húmedo desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  labs(x = "Peso húmedo de raíces (g)", y= "Frecuencia absoluta")+
  theme_classic()+
  theme(legend.position = "none")

##Analisis##
#ANOVA#
#Supuestos#
mod_ph <-lm(ph_raices~tratamiento_ph,data_ph)
anva_ph <-aov(mod_ph)
summary(anva_ph)
#Grafica
x11()
par(mfrow=c(2,2))
plot(mod_ph)
#Cumplimiento de supuestos#
# Residuales Estandarizados
ri_ph <- rstandard(mod_ph)
sort(ri_ph)
# Test de Shapiro-Wilk (Normalidad)
shapiro.test(ri_ph)
# Test de Anderson Darling (Normalidad)
library(nortest)
ad.test(ri_ph)
#Test de pearson (Normalidad)
pearson.test(ri_ph)
# Test de Barlett (homogeneidad de varianzas)
library(car)
bartlett.test(ph_raices~tratamiento_ph,data_ph)
ncvTest(mod_ph)
leveneTest(ph_raices~tratamiento_ph,data_ph)
#Prueba no paramétrica en DCA
# === Prueba de Kruskal-Wallis ===
#Supuestos
library(kSamples)
ad.test(ph_raices~tratamiento_ph)
#Test de Kruskal-Wallis
library(readxl)
dataKW_ph <-read_excel("Datos_Tesis.xlsx", sheet = 4)
str(dataKW_ph)
nraices <-dataKW_ph[,1]
tratamiento <-dataKW_ph[,2]
str(tratamiento)
library(agricolae)
kruskal(ph_raices,tratamiento_ph, console=T)
kruskal(ph_raices,tratamiento_ph,group=FALSE, console=T)

####Peso seco de raices####
data_ps <-read_excel("Datos_Tesis.xlsx", sheet = 5)
head(data_ps)
str(data_ps)
summary(data_ps)
data_ps$Tratamiento <-factor(data_ps$Tratamiento)
str(data_ps)
summary(data_ps)
ps_raices <-data_ps$`Peso Seco de raices (g)`
ps_raices
tratamiento_ps <-data_ps$Tratamiento
#Estadistica descriptiva#
library(mosaic)
estadisticos3 <- favstats(ps_raices~tratamiento_ps, data=data_ps)
estadisticos3
estadisticos3$cv <- estadisticos3$sd*100/estadisticos2$mean
estadisticos3
estadisticos_ps <-estadisticos3[,-10]
estadisticos_ps
#Boxplot
library(ggplot2)
x11()
ggplot(data = data_ps, mapping = aes(x = tratamiento_ps, y = ps_raices, fill = tratamiento_ps)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Peso seco de raíces desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  stat_summary(fun = mean, geom = "point", colour = "darkred", size= 1.5)+
  labs(x = "Tratamientos", y= "Peso seco de raíces (g)")+
  theme(legend.position = "none")
#Histograma
library(ggplot2)
#General
x11()
ggplot(data_ps, aes(x=ps_raices))+
  geom_histogram(aes(y=..density..), binwidth =0.005, 
                 fill="white",colour="black")+
  geom_density(alpha=.2, fill= "#FF6666")+
  geom_vline(aes(xintercept=mean(ps_raices)), 
             colour="red", linetype="dashed",size=1)+
  labs(title = "Histograma de peso seco desarrolladas por las estaquillas de Eucalipto Urograndis")+
  labs(x = "Peso seco de raíces (g)", y= "Frecuencia relativa")+
  theme_classic()
#Por tratamiento
x11()
ggplot(data_ps)+ geom_histogram(aes(x=ps_raices, fill=tratamiento_ps, colour="black")
                                ,bins=6, position = "identity",alpha = 0.5)+
  facet_grid(tratamiento_ps)+
  labs(title = "Histograma de peso seco desarrolladas por las estaquillas de Eucalipto Urograndis por dosis de AIB")+
  labs(x = "Peso húmedo de raíces (g)", y= "Frecuencia absoluta")+
  theme_classic()+
  theme(legend.position = "none")


##Analisis##
#ANOVA#
#Supuestos#
mod_ps <-lm(ps_raices~tratamiento_ps,data_ps)
anva_ps <-aov(mod_ps)
summary(anva_ps)
#Grafica
x11()
par(mfrow=c(2,2))
plot(mod_ps)
# Residuales Estandarizados
ri_ps <- rstandard(mod_ps)
sort(ri_ps)
# Test de Shapiro-Wilk (Normalidad)
shapiro.test(ri_ps)
# Test de Anderson Darling (Normalidad)
library(nortest)
ad.test(ri_ps)
#Test de pearson (Normalidad)
pearson.test(ri_ps)
# Test de Barlett (homogeneidad de varianzas)
library(car)
bartlett.test(ps_raices~tratamiento_ps,data_ps)
ncvTest(mod_ps)
leveneTest(ps_raices~tratamiento_ps,data_ps)
#Prueba no paramétrica en DCA
# === Prueba de Kruskal-Wallis ===
#Supuestos
library(kSamples)
ad.test(ps_raices~tratamiento_ps)
##Prueba de la mediana##
Median.test(ps_raices,tratamiento_ps)
Median.test(ps_raices,tratamiento_ps, group=F, console=T)

###Analisis de correlación###
#lectura de datos#
library(readxl)
data_cor <-read_excel("Analisis de correlacion.xlsx")
head(data_cor)
str(data_cor)
library(psych)
#TO#
#nombres detallados
names(data_cor)= c("Longitud de raíces", "Peso húmedo de raíces", "Peso seco de raíces")
head(data_cor)
#Analisis
x11()
pairs.panels(data_cor[c(1,2,3)], cex.labels=2,cex.cor = 2,breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)

#T1#
data_cor1 <-read_excel("Analisis de correlacion.xlsx", sheet = 2)
head(data_cor1)
str(data_cor1)
#nombres detallados
names(data_cor1)= c("Longitud de raíces", "Peso húmedo de raíces", "Peso seco de raíces")
head(data_cor1)
#Analisis
x11()
pairs.panels(data_cor1[c(1,2,3)], cex.labels=2,cex.cor = 2,breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)

#T2#
data_cor2 <-read_excel("Analisis de correlacion.xlsx", sheet = 3)
head(data_cor2)
str(data_cor2)
#nombres detallados
names(data_cor2)= c("Longitud de raíces", "Peso húmedo de raíces", "Peso seco de raíces")
head(data_cor2)
#Analisis
x11()
pairs.panels(data_cor2[c(1,2,3)], cex.labels=2,cex.cor = 2,breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)

#T3#
data_cor3 <-read_excel("Analisis de correlacion.xlsx", sheet = 4)
head(data_cor3)
str(data_cor3)
#nombres detallados
names(data_cor3)= c("Longitud de raíces", "Peso húmedo de raíces", "Peso seco de raíces")
head(data_cor3)
#Analisis
x11()
pairs.panels(data_cor3[c(1,2,3)], cex.labels=2,cex.cor = 2,breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)

#T4#
data_cor4 <-read_excel("Analisis de correlacion.xlsx", sheet = 5)
head(data_cor4)
str(data_cor4)
#nombres detallados
names(data_cor4)= c("Longitud de raíces", "Peso húmedo de raíces", "Peso seco de raíces")
head(data_cor4)
#Analisis
x11()
pairs.panels(data_cor4[c(1,2,3)], cex.labels=2,cex.cor = 2,breaks = "Sturges",bg= c( "blue"),pch=21,lm=TRUE, 
             method = "pearson", hist.col = "cyan", stars = T, cor = T, digits = 2,scale = T, 
             density = T, ellipses = T, show.points = T,  alpha = .05)
