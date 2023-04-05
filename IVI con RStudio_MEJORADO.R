#################################
####### Construcción IVI ########
#################################
setwd("C:/Users/Usuario/Desktop/ESTADISTICA FORESTAL/Clase 01")
datos <- read.csv("CASO1_MEYDE.csv",T)
head(datos)
str(datos)
ivi <- datos[,c(3,2,6)] #selección de datos relevantes
head(ivi)
abundancia <- data.frame(table(ivi$Especie)) ##número de individuos por especie
abundancia
library(mosaic)
estadisticos <- favstats(ivi$AB~ivi$Especie)#estadísticos de área basal por especie
head(estadisticos)
dominancia <- estadisticos$mean*estadisticos$n #suma de AB por especie
head(dominancia)
##IVIsimplificado#
ivis1 <- data.frame(abundancia,dominancia) #tabla de IVIs
head(ivis1)
abun_relativa <- round(prop.table(abundancia$Freq)*100,4) #abundancia relativa
cumsum(abun_relativa)# suma igual a 100%
domin_relativa <- round(prop.table(dominancia)*100,4)# dominancia relativa
cumsum(domin_relativa) ## suma igual a 100%
#Componentes del IVI completado
ivis2 <- data.frame(abundancia,dominancia,abun_relativa,domin_relativa)
head(ivis2)

ivis2$ivi <- ivis2$abun_relativa+ivis2$domin_relativa #añado columna IVI
head(ivis2)
#IVI ordenado de mayor a menor
ivis3 <- ivis2[with(ivis2, order(-ivis2$ivi)), ]
head(ivis3)
ivis3$iviacum <- round(cumsum(ivis3$ivi),2) #añado columna IVI acumulado
head(ivis3,n=17)
ivis_sp <- ivis3$Var1[1:17]
#cambio de nombres a las columnas
names(ivis3)= c("Especie","Abundancia absoluta","Dominancia absoluta","Abundancia relativa","Dominancia relativa","IVIs","IVIs acumulado")
head(ivis3)
#guardar resultados
write.csv(ivis3,file = "ivisjh.csv")

### IVI ##
table(ivi$Especie,ivi$Parcela) #número de individuos por parcela
freq <- data.frame(table(ivi$Especie,ivi$Parcela))#tabla de frecuencias por parcelas
head(freq)
#solo importa presencia o no, 0 significa no presencia
freq$Freq <- factor(freq$Freq)#reemplazo la columna Freq por si mismo pero como factor
str(freq)
frecuencia <- data.frame(table(freq$Freq,freq$Var1))#número de veces que aparece en las n de 5 parcelas
head(frecuencia,n=15)#Aachuri caspi, en 4 ocasiones aparece en 0 parcelas y en una
#ocasión aparece en una parcela
no_freq <- subset(frecuencia, Var1=="0")#número de parcelas en las que no aparece
str(no_freq)
head(no_freq)

no_freq$frec_abs <- 5-no_freq$Freq #número de parcelas en las que sí aparece
head(no_freq)
no_freq$frec_rel <- round(prop.table(no_freq$frec_abs)*100,4) #frecuencia relativa%
cumsum(no_freq$frec_rel)#suma 100%
head(no_freq)
IVI <- data.frame(ivis2,no_freq[,4:5]) #tabla de componentes del IVI
head(IVI)
#creacion de tabla de componentes de IVI
IVI <- data.frame(IVI$Var1,IVI$Freq,IVI$dominancia,IVI$frec_abs,IVI$abun_relativa,IVI$domin_relativa,IVI$frec_rel)
head(IVI)
str(IVI)
#cambio de nombres en las columnas
names(IVI)= c("Especie","Abundancia absoluta","Dominancia absoluta","Frecuencia absoluta","Abundancia relativa","Dominancia relativa","Frecuencia relativa")

freq_rel_acum <- round(cumsum(IVI$`Frecuencia relativa`),2)#suma 100
abun_rel_acum <-  round(cumsum(IVI$`Abundancia relativa`),2)#suma 100
dom_rel_acum <- round(cumsum(IVI$`Dominancia relativa`),2)#suma 100

head(IVI)
#anado una columna de IVI con la suma de sus componentes
IVI$IVI <- IVI$`Abundancia relativa`+IVI$`Dominancia relativa` + IVI$`Frecuencia relativa`
head(IVI)
IVI_rel_acum <- round(cumsum(IVI$`IVI`),4) #suma 300
mode(IVI)
indvalimp <- data.frame(IVI) #tabla completa del IVI
head(indvalimp)
dataivi <- indvalimp[with(indvalimp, order(-indvalimp$IVI)), ] #ordenamiento de mayor a menor
head(dataivi)

ivi_acum <- round(cumsum(dataivi$`IVI`),2)# redondeo de valor de IVI
dataivi$IVIacumulado <- ivi_acum #creación de columna IVI acumulado
head(dataivi)
head(dataivi, n=23) #se llega al 150% con la especie 23
IVI_SP <- dataivi[1:23,1] #identificación de especies más importantes
IVI_SP
#cambio de nombre a las columnas
names(dataivi)= c("Especie","Abundancia absoluta","Dominancia absoluta","Frecuencia absoluta","Abundancia relativa","Dominancia relativa","Frecuencia relativa","IVI","IVI acumulado")
head(dataivi,n=23)
write.csv(dataivi,file = "IVIJH.csv") #exportar resultado
getwd()
