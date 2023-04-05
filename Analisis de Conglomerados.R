#Analisis de conglomerados#
setwd("C:/Users/Usuario/Desktop/ANALISIS ESTADISTICO EN R (INIA)/ANALISIS MULTIVARIADO")
library(readxl)
datos <-read_excel("Datos.xlsx")

#Transformacion a Z
data <-as.data.frame(scale(datos[,-1]))
rownames(data) <- datos$Code

#Cluster jerarquico#
hc <- hclust(dist(data, method = "euclidean"), method = "ward.D")
hc
plot(hc, hang = -0.01, cex = 0.7)
x11()
rect.hclust(hc, k=2, border = "blue")

#K means para agrupación
km <- kmeans(data,2)
km

aggregate(data, by = list(cluster= km$cluster), mean)

library(ggplot2)
library(factoextra)
fviz_cluster(km, data = data)
fviz_nbclust(data, kmeans, method = "gap_stat")

#OTRO TIPO DE PRESENTACION#
km_clusters <- kmeans(x=data, centers = 3, nstart = 50)
x11()
fviz_cluster(object = km_clusters, data = data, show.clust.cent = T, ellipse.type = "euclid", star.plot = T, repel = T)+
  labs(title = "Resultados clustering K-means") +
  theme_bw()+
  theme(legend.position = "none")

#K medoides (PAM)
library(cluster)
#N optimo de clusters#
fviz_nbclust(x=data, FUNcluster = pam, method = "wss", k.max = 10, diss = dist(data , method = "manhattan"))

set.seed(123)
pam_clusters <- pam(x = data, k = 3, metric = "manhattan")
pam_clusters
x11()
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "t", repel = T)+
  theme_bw()+
  labs(title = "Resultados clustering PAM")+
  theme(legend.position = "none")

# CLARA (Clustering Large Applications)
clara_clusters <- clara(x = data,k = 2, metric = "manhattan", stand = T, samples = 50, pamLike = T )
clara_clusters
x11()
fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point", pointsize = 2.5)+
  theme_bw()+
  labs(title = "Resultados clustering CLARA")+
  theme(legend.position = "none")

#CLUSTER JERARQUICO
matriz_distancias <- dist(x = data, method = "euclidean")
hc_euclidea_completo <- hclust(d = matriz_distancias, method = "complete")
hc_euclidea_single <- hclust(d = matriz_distancias, method = "single")
hc_euclidea_average <- hclust(d = matriz_distancias, method = "average")

par(mfrow = c(1,1))
x11()
plot(x = hc_euclidea_completo, cex = 0.6, xlab = "", ylab = "", main = "Distacia euclidea, Linkage complete")
plot(x = hc_euclidea_single, cex = 0.6, xlab = "", ylab = "", main = "Distacia euclidea, Linkage single")
plot(x = hc_euclidea_average, cex = 0.6, xlab = "", ylab = "", main = "Distacia euclidea, Linkage average")

#Metodo divisitivo
matriz_distancias <- dist(x = data, method = "euclidean")
hc_diana <- diana(x = matriz_distancias, diss = T, stand = F)
x11()
fviz_dend(x = hc_diana, cex = 0.5)+
  labs(title = "Clustering divisivo", subtitle = "Distancia euclidea")

#numero optimo de clusters
#Elbow method
fviz_nbclust(x =data,FUNcluster = kmeans, method = "wss", k.max = 10,diss = dist(data , method = "manhattan"))+
  labs(title = "Numero optimo de clusters")

#Average silhouette method
fviz_nbclust(x =data,FUNcluster = kmeans, method = "silhouette", k.max = 10,diss = dist(data , method = "manhattan"))+
  labs(title = "Numero optimo de clusters")

#Gap statistic method
fviz_nbclust(x =data,FUNcluster = kmeans, method = "gap_stat", k.max = 10,diss = dist(data , method = "manhattan"))+
  labs(title = "Numero optimo de clusters")

#Customizacion de dendrogramas
library(dendextend)

mat_distancia <- dist(data, method = "euclidean")
hc_average <- hclust(d = mat_distancia, method = "average")

set.seed(5665)
x11()
fviz_dend(x = hc_average, main = "Dendrograma ward", xlab = "observaciones", ylab = "distancia", sub = "")

#Representacion horizontal
x11()
fviz_dend(x = hc_average, main = "Dendrograma ward", xlab = "observaciones", ylab = "distancia", sub = "", horiz = T)

#colores
fviz_dend(x = hc_average,k = 3, k_colors = c("red","blue","green"), color_labels_by_k = T, rect = T, rect_border = c("blue","yellow", "red"), rect_fill = T, cex = 0.5, main = "Dendrograma ward", xlab = "observaciones", ylab = "distancia", sub = "", horiz = T)

#Diagrama dendrocircular
set.seed(5665)
x11()
fviz_dend(x = hc_average, k = 4, k_colors = c("#2E9FDF", "#00AFBB", "#E78800", "#FC4E07"), color_labels_by_k = T, cex = 0.5, type = "circular")

#Dendrograma arbol filogenetico
install.packages("igraph")
library("igraph")
set.seed(5665)
fviz_dend(x = hc_average, k = 4, k_colors = c("#2E9FDF", "#00AFBB", "#E78800", "#FC4E07"), color_labels_by_k = T, cex = 0.8, type = "phylogenic", repel = T )

