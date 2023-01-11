## Statistics Avanced ##

## Imputacion de datos perdidos ##

# Charge packages

library(palmerpenguins)
install.packages("missForest")
library(missForest)
library(tidyverse)

# Charge dataset

data <- data.frame(penguins)
class(data)
glimpse(data)
sapply(data, class)

# Imputaciones

imp <- missForest(data, verbose = T, variablewise = F)
imp$OOBerror

imp <- missForest(data, verbose = T, variablewise = T)
imp$OOBerror

# Casos completados

data_compl <- data.frame(imp$ximp)

## Analisis de componentes principales (PCA) ##

# Charge packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tibble","tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)


# Charge dataset

library(readxl)

data <- read_excel("Data/data_marvel.xlsx")

# Acomodar el dataset

respca <- prcomp(data, scale = T)

install.packages("textshape")
library(textshape)

data <- data[!duplicated(data$Name),] # quitar duplicados
data <- textshape::column_to_rownames(data, loc = 1) # Mover las columnas

data <- data.frame(data)
data <- subset(data, select = -c(Alignment, Total))

# PCA in data matrix

respca <- prcomp(data, scale = T)
names(respca)

head(respca$rotation)[, 1:5] #las coordenadas de los datos en el nuevo sistema rotado de coordenadas. 
                            #Estas coordenadas se corresponden con los scores de los componentes principales.

dim(respca$rotation) #Número de distintos componentes

head(respca$x)[,1:5] #los vectores de los scores.

respca$sdev #las desviaciones estándares de cada CP.

respca$sdev^2  ## Varianza explicada por cada componente

summary(respca)

# Importance of component 1

xx <- respca$x
xx <- data.frame(xx)

data$PC1 <- xx$PC1
data$PC2 <- xx$PC2
head(data)
cor(data)

# PCA with more details

library(FactoMineR)

data <- data[, -c(7,8)]

respca2 <- PCA(X = data, scale.unit = F, ncp = 6, graph = T)

print(respca2)

head(respca2$eig)

library(factoextra)

get_pca(respca2) #Extrae la información sobre las variables.

get_pca_var(respca2) #Extrae la información sobre las variables.

get_pca_ind(respca2) #Extrae la información sobre las observaciones.


#visualización

fviz_eig(respca2) #visualizar eigenvalores (scree plot)

fviz_screeplot(respca2) #visualizar eigenvalores (scree plot)

#Representación de observaciones sobre componentes principales

fviz_pca_ind(respca2) 

fviz_pca_ind(respca2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE)    # Avoid text overlapping
             
             
#Representación de variables sobre componentes principales.             

fviz_pca_var(respca2) 

fviz_pca_var(respca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping


fviz_pca_var(respca2, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)


fviz_contrib(respca2,choice = "var") #Representa la contribución de filas/columnas de los resultados de un pca.
fviz_contrib(respca2,choice = "ind")

biplot(x = respca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

fviz_pca_biplot(respca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(respca, repel = TRUE,
                #geom= "point", #o text o geom=c("point", "text")
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.ind = list(contrib = 30)
)


## Radar spider ###

# Charge packages

install.packages("fmsb")
library(fmsb)
library(scales)
library(plotly)
library(tidyverse)

# Create dataset

max_min <- data.frame(
  R = c(100,0), Python = c(100, 0), Julia = c(100,0))

rownames(max_min) <- c("Max", "Min")

data <- as.data.frame(cbind(60,10,5))
colnames(data) <- c("R", "Python", "Julia")

# Union by rows

df <- rbind(max_min, data)

# Radar graphic

color<-rgb(0.2,0.5,0.5,0.9)

op <- par(mar = c(1, 2, 2, 1))
radarchart(df, axistype=1 , #axistype me dice el tipo de iformación en el eje
           
           #personalizamos el polígono (color lo acabo de crear)
           pcol= color , pfcol= scales::alpha(color, 0.5) , plwd=2 , 
           
           #Personalizamos la rejilla (grid)
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=c("Bajo", "", "Medio", "", "Alto"), cglwd=0.8,
           
           title= "Habilidades en programación estadística",
           
           #otros
           vlcex=1.5 
)


# Example 2

# Create data

exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20))

max_min <- data.frame(
  Biology = c(20, 0), Physics = c(20, 0), Maths = c(20, 0),
  Sport = c(20, 0), English = c(20, 0), Geography = c(20, 0),
  Art = c(20, 0), Programming = c(20, 0), Music = c(20, 0))

rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, exam_scores)
df

# Create personality function 

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


# Radar graphic

create_beautiful_radarchart(
  data = df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07"))
legend(
  x = "top", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5)
par(op)


# Example 3

colors <- c("#00AFBB", "#E7B800", "#FC4E07")
titles <- c("Student.1", "Student.2", "Student.3")

# Reduce plot margin using par()
# Split the screen in 3 parts

op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))

# Create the radar chart

for(i in 1:3){
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ], caxislabels = c(0, 5, 10, 15, 20),
    color = colors[i], title = titles[i]
  )
}
par(op)


## Graphics 3D ##

# Charge packages

install.packages("scatterplot3d")

# Example 1

ventas <- c(10,6,5,12,10,15,5,12,17,20)
precio <- c(1.3,2,1.7,1.5,1.6,1.2,1.6,1.4,1,1.1)
publicidad <- c(9,7,5,14,15,12,6,10,15,21)

x <- precio
y <- ventas
z <- publicidad 

# Create model

mod <- lm(ventas ~ precio + publicidad)
summary(mod)

# Graphic

library(scatterplot3d)

scatterplot3d(x, y, z, pch = 19, color = "blue" )
scatterplot3d(x, z, y, pch = 19, color = "blue" )

# Example 2

# Charge packages

install.packages("rgl")
library(rgl)
install.packages("shiny")
library(shiny)

# plot

plot3d(x,y,z,
       type = "s",
       radius = 0.1,
       col = "lightblue",
       xlab = "X axis lab",
       ylab = "Y axis lab",
       zlab = "Z axis lab")


# Example 3

exam <- data.frame(precio, ventas, publicidad)

colnames(exam) <- c("precio", "ventas", "publicidad")
head(exam)

library(scatterplot3d)
attach(exam)

plt1 <- scatterplot3d(x = precio, y = ventas, z = publicidad,
                      pch = 16, cex.lab = 1, highlight.3d = T, type = "h",
                      xlab = "Precio", ylab = "Ventas", zlab = "Publicidad")

# Ajustar el plano de regresion multiple

plt1$plane3d(mod, lty = "solid", col = "mediumblue")

# Interactive graphic

library(tidyverse)
library(plotly)
library(graphics)

plot_ly(x = precio, y = ventas, z = publicidad, type = "scatter3d", color = precio) %>%
  layout(scene = list(xaxis = list(title = "Precio"),
                      yaxis = list(title = "Ventas"),
                      zaxis = list(title = "Publicidad")))


# Regresion Multiple

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("forestmodel","see","jtools","olsrr","parameters","stats","ggplot2", "palmerpenguins", "plot3D" , "plot3Drgl","apaTables","gvlma","broomExtra","performance")
ipak(packages)

remotes::install_github("IndrajeetPatil/broomExtra")

#cambiar a vuestro directorio de trabajo.

data(package = 'palmerpenguins')
head(penguins)
df<-as.data.frame(penguins)
df<-df[complete.cases(df), ]


#Hacemos la regresión

linearMod <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm, data=df)  # build linear regression model on full data
print(linearMod)

#comprobamos los supuetos:

gvlma(linearMod)
summary(gvlma(linearMod))
plot(gvlma(linearMod))

#mejores presentaciones de resultados.

model_parameters(linearMod, bootstrap = TRUE, iterations = 500)

ols_step_both_p(linearMod)

summary(linearMod)

broomExtra::glance(linearMod)

#intervalos de confianza

confint(linearMod)

#ciertos indicadore

AIC(linearMod)  # AIC => 
BIC(linearMod)  # BIC =>

#te muestra el mejor r2 para utilizar en tu modelo.

performance::r2(linearMod)

#modelo de predicción

library(readxl)
testData <- read_excel("testData.xlsx")
View(testData)

pesoPred <- predict(linearMod, testData)
actuals_preds <- data.frame(cbind(actuals=testData$Peso, predicteds=pesoPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)


#Tablas apañadas

forest_model(linearMod)

apa.reg.table(linearMod, filename = "tablaregresion.doc", table.number = NA,
              prop.var.conf.level = 0.95)

#visualizaciones

ggstatsplot::ggcoefstats(
  x = stats::lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm, data=df),
  sort = "ascending", # sorting the terms of the model based on estimate values
  ggtheme = ggplot2::theme_gray(), # changing the default theme
  stats.label.color = c("#CC79A7"),
  title = "Chupito R",
  subtitle = "Suscríbete"
) +
  # further modification with the ggplot2 commands
  # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c(
    "pico1", "pico 2", "aleta")) +
  ggplot2::labs(y = "etiqueta y)") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 14, face = "bold"))


plot_summs(linearMod, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)


#representación 3D de la regesión con dos predictoras

z<-df$body_mass_g
y<-df$bill_length_mm
x<-df$flipper_length_mm

scatter3D(x, y, z, theta = 15, phi = 20)
scatter3D(x, y, z, phi = 0, bty ="g")
scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "pingu", xlab = "Aleta",
          ylab ="Pico", zlab = "Peso")

scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "Aleta",
          ylab ="Pico", zlab = "Peso")

#es importante que los valores z sean de la dependiente. 
#creamos otro obj de regresión

objr<-lm(z ~ x+y)
objr

#preparamos el modelado 3d

grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# Marcamos las líneas de iteracción para que busquen la recta de regresión
fitpoints <- predict(objr)
#ploteamos la gráfica en 3d con recta de regresión
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "aleta", ylab = "pico", zlab = "peso",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")
#la hacemos interactiva (coge el último gráfico que hayamos creado)
plotrgl()
#lo guardamos como gif en nuestro working directory
movie3d(spin3d(axis = c(0, 0, 1)), duration = 15,
        dir = getwd())
