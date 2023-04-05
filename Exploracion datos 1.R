
#Importar los datos
data <-read.csv("Espectros_HSI.csv", header = TRUE, sep = ";",
                stringsAsFactors = FALSE)
View(data)

data$Variedad <- factor(data$Variedad,
                        levels = c(1, 2, 3),
                        labels = c("Catimor", "Caturra", "Tipica"))
data$Lugar <- factor(data$Lugar,
                     levels = c(1, 2),
                     labels = c("R. mendoza", "Utcubamba"))
data$Periodo.de.Recogida <- factor(data$Periodo.de.Recogida,
                                   levels = c(1, 2, 3),
                                   labels = c("<24h", "de 24h a 48h", ">48h"))
data <-data[, -c(6:81)]
summary(data)
str(data)
data <- data[, -c(4:5)]

#split/ansplit == segmentar un dataframe
lugar1 <- split(data, data$Lugar)
lugar1[[1]]
str(lugar1[[1]])

lugar1[[2]]
str(lugar1[[1]])
summary(lugar1[[2]])


# partition of dataframe variable categorical
install.packages("caret")
library(caret)
training.ids <- createDataPartition(data$Variedad, p=0.6, list=F)
data.modelling <- data[training.ids,]
temp <- data[-training.ids,] # reprenta el 40%
validation.ids <- createDataPartition(temp$Variedad, p=0.5, list = F)
data.validation <- temp[validation.ids,]
data.testing <- temp[-validation.ids,]
