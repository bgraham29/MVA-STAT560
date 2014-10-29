require("cluster")
## Irsi data 
X=iris[1:4]

##hierarchical clustering: can choose different distance and linkage
iris.single = hclust(dist(X, method = "euclidean"), method = "single")
plot(iris.single)

## cut tree and create a silhouette  
single.cluster<-cutree(iris.single, k = 3)
single.si<-silhouette(single.cluster,dmatrix=as.matrix(dist(X, method = "euclidean")))
plot(single.si) # silhouette plot
plot(single.si, col = c("red", "green", "blue"))# with cluster-wise coloring


##kmeans clustering 
iris.kmeans<-kmeans(X,3);
kmeans.si<-silhouette(iris.kmeans$cluster,dmatrix=as.matrix(dist(X, method = "euclidean")))
plot(kmeans.si) # silhouette plot
plot(kmeans.si, col = c("red", "green", "blue"))# with cluster-wise coloring


##clustering assignments
iris.kmeans$cluster

#WSS total
iris.kmeans$tot.withinss

#BSS total
iris.kmeans$betweenss

## PAM (partitoning around medoids)
iris.pam<-pam(X,3)
si <- silhouette(iris.pam)
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue"))# with cluster-wise coloring

