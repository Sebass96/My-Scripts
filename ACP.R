
install.packages("FactoMineR")
install.packages("ggplot2")
install.packages("factoextra")

library("FactoMineR")
library(ggplot2)
library("factoextra")

library(readxl)
datos <- read_excel("Datos.xlsx",
                    sheet = "Hoja1")

data <- as.data.frame(scale(datos[,-1]))
# data$Code <- datos$Code
rownames(data) <- datos$Code
data$Code <- NULL

#Análisis de componentes
PCA(data, scale.unit = TRUE, ncp = 2, graph = TRUE)

res.pca <- PCA(data, graph =FALSE)
print(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 70))
var <- get_pca_var(res.pca)
var
#coordenadas
head(var$coord, 8)
head(var$cos2)
head(var$contrib)
#fviz_pca_var((res.pca, col.var = "#0073C2FF"))

library("corrplot")
corrplot(var$cos2, is.corr = FALSE)

set.seed(123)
res.km <- kmeans(var$coord, center = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
fviz_pca_var(res.pca, col.var = grp,
             palette = c("red", "blue", "green"),
             legend.title = "Clusters")

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
res.desc$Dim.2

#GRAFICAS INDIVIDUALES
ind <- get_pca_ind(res.pca)
ind
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_cos2(res.pca, choice = "ind", axes = 1:2)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "green",
                col.ind = "blue")

#analisis de tres dimensiones
res.pca <- PCA(data, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)

fviz_dend(res.hcpc,
          cex = 0.8,
          palette = "jco",
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",
          labels_track_height = 1.5)

#cp + tree
plot(res.hcpc, choice = "3D.map")
