
#Analisis de conglomerados
library(readxl)
datos <- read_excel("Datos.xlsx", 
                    sheet = "Hoja1")
View(datos)
data <- as.data.frame(scale(datos[,-1]))
data$Code <- datos$Code
rownames(data) <- data$Code
data$Code <- NULL

#clúster jerarquico
hc <- hclust(dist(data, method = "euclidean"), method = "complet")
hc

plot(hc, hang = -0.01, cex = 0.7)

rect.hclust(hc, k=2, border = "blue")


####################
#K means para agrupación

km <- kmeans(data,2)
km

aggregate(data, by = list(cluster = km$cluster), mean)

library(ggplot2)
library(factoextra)
fviz_cluster(km, data = data)


fviz_nbclust(data, kmeans, method = "gap_stat")

## OTRO TIPO DE PRESENTACIÓN
km_clusters <- kmeans(x = data, centers = 3, nstart = 50)
fviz_cluster(object = km_clusters, data = data, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

### k-medoides (PAM)
library(cluster)
library(factoextra)
fviz_nbclust(x = data, FUNcluster = pam, method = "wss", k.max = 10,
             diss = dist(data, method = "manhattan"))

set.seed(123)
pam_clusters <- pam(x = data, k = 3, metric = "manhattan")
pam_clusters

fviz_cluster(object = pam_clusters, data = data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

## CLARA (Clustering Large Applications)
library(cluster)
library(factoextra)
clara_clusters <- clara(x = data, k = 2, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)
clara_clusters

fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")


#################################################
## Hierarchical clustering
matriz_distancias <- dist(x = data, method = "euclidean")
set.seed(567)
hc_euclidea_completo <- hclust(d = matriz_distancias, method = "complete")
hc_euclidea_single   <- hclust(d = matriz_distancias, method = "single")
hc_euclidea_average  <- hclust(d = matriz_distancias, method = "average")
par(mfrow = c(1,1))
plot(x = hc_euclidea_completo, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage complete")
plot(x = hc_euclidea_single, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage single")
plot(x = hc_euclidea_average, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage average")
# método divisivo
matriz_distancias <- dist(x = data, method = "euclidean")
hc_diana <- diana(x = matriz_distancias, diss = TRUE, stand = FALSE)

fviz_dend(x = hc_diana, cex = 0.5) +
  labs(title = "Hierarchical clustering divisivo",
       subtitle = "Distancia euclídea")

#numero optimo de clusters
#library(factoextra)
#Elbow method
fviz_nbclust(x = data, FUNcluster = kmeans, method = "wss", k.max = 10) +
  labs(title = "Número óptimo de clusters")

#Average silhouette method
fviz_nbclust(x = data, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")

#Gap statistic method
fviz_nbclust(x = data, FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             k.max = 10, verbose = FALSE, nstart = 50) +
  labs(title = "Número óptimo de clusters")

#Customizacion de dendogramas
library(factoextra)
library(dendextend)

mat_distancia <- dist(data, method = "euclidean")
hc_average <- hclust(d = mat_distancia, method = "average")

set.seed(5665)
fviz_dend(x = hc_average,
          cex = 0.5,
          main = "Dendrograma - ward",
          xlab = "observaciones",
          ylab = "distancia",
          sub = "")
#Representación horizontal
fviz_dend(x = hc_average,
          cex = 0.5,
          main = "Dendrograma - ward", 
          xlab = "observaciones",
          ylab = "distancia",
          sub = "",
          horiz = TRUE)
#Colores - k
fviz_dend(x = hc_average,
          k = 3,
          k_colors = c("red", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          rect_border = c("red", "#00AFBB", "#E7B800"),
          rect_fill = TRUE,
          cex = 0.5,
          main = "Dendrograma - ward",
          xlab = "observaciones",
          ylab = "distancia",
          sub = "",
          horiz = TRUE)

#Dendograma circular
set.seed(5665)
fviz_dend(x = hc_average,
          k = 4,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          cex = 0.5,
          type = "circular")

#Dendrograma en forma de árbol filogenético.
install.packages("igraph")
library("igraph")
set.seed(5665)
fviz_dend(x = hc_average,
          k = 4,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          cex = 0.8,
          type = "phylogenic",
          repel = TRUE)

# Zoom en un área determinada.
set.seed(5665)
fviz_dend(x = hc_average,
          k = 4,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          cex = 0.5,
          main = "Zoom del area x = 1, 20, y = -10, 100",
          xlim = c(1,10),
          ylim = c(-10,100))



