install.packages("FactorMineR")
install.packages("factoextra")
library(FactoMineR)
library(ggplot2)
library(factoextra)
setwd("C:/Users/Usuario/Desktop/ANALISIS ESTADISTICO EN R (INIA)/ANALISIS MULTIVARIADO")
library(readxl)
datos <-read_excel("Datos.xlsx")

#Transformacion a Z
data <-as.data.frame(scale(datos[,-1]))
rownames(data) <- datos$Code

#Analisis de componentes
PCA(data, scale.unit = T, ncp = 2, graph = T)
res.pca <- PCA(data, graph = F)
print(res.pca)

eig.val <- get_eigenvalue(res.pca) #varianza de las variables
eig.val

fviz_eig(res.pca, addlabels = T, ylim = c(0,70))
var <- get_pca_var(res.pca)
var

#coordenadas#
head(var$coord, 7)
head(var$cos2)

library("corrplot")
corrplot(var$cos2, is.corr = F)

set.seed(123)
res.km <- kmeans(var$coord, center = 3, nstart = 25)
grp <- as.factor (res.km$cluster)
fviz_pca_var(res.pca,col.var = grp,palette = c("red", "blue", "green"), legend.title = "Clusters")
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1

#Graficas individuales#
ind <- get_pca_ind(res.pca)
ind
x11()
fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_pca_biplot(res.pca, repel = T, col.var = "green", col.ind = "blue")  

#analisis de tres dimensiones#
res.pca <- PCA(data, ncp = 3, graph = F)
res.hcpc <- HCPC(res.pca, graph = F)

fviz_dend(res.hcpc, cex = 0.8, palette = "jco", rect = T, rect_fill = T, rect_border = "jco", labels_track_height = 1.5)

#CP + tree
plot(res.hcpc, choice = "3D.map")
