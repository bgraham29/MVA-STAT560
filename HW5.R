################################################################################

M <- matrix(c(0,1,1,4,2,9,4,7,0,2,1,8,3,3,5,4,0,3,1,9,3,5,5,5,1,0,2,1,3,7,5,6,
    1,1,2,2,3,8,6,3,1,2,2,4,4,4,6,4,1,3,2,7,4,6,6,5), ncol = 2, byrow = TRUE)

df <- data.frame(M)
names(df) <- c("x", "y")

plot(df, pch = 18)

## PAM: partitioning around medoid
## k-medoids
require(cluster)


df.pam <- pam(df, 3)
plot(silhouette(df.pam))
abline(v = 0.75)
df.pam <- pam(df, 2)
plot(silhouette(df.pam))
abline(v = 0.75)
df.pam <- pam(df, 4)
plot(silhouette(df.pam))
df.pam <- pam(df, 5)
plot(silhouette(df.pam))

plot(df, col = df.pam$cluster, pch = 20, cex = 2)
abline(8, -8/6, col = "blue", lty = "dashed")

df.pam1 <- pam(df[, 1], 2)
plot(silhouette(df.pam1))
plot(df, col = df.pam1$cluster, pch = 20, cex = 2)
abline(v = 2.5, col = "blue", lty = "dashed")

df.pam2 <- pam(df[, 2], 2)
plot(silhouette(df.pam2))
plot(df, col = df.pam2$cluster, pch = 20, cex = 2)
abline(h = 5.5, col = "blue", lty = "dashed")

### 
# 2 is a geometrical interpretation


# 3: mathematical equivalencies


# 4 Seed data

seed <- read.csv(".\\Data\\seeds_clustering.csv", header = FALSE)
plot(seed)

seed.dist <- dist(seed, method = "euclidean")

# single linkage is not usefule
seed.s <- hclust(dist(seed.dist, method = "euclidean"), method = "single")
plot(seed.s)

# complete linkage better
seed.c <- hclust(dist(seed.dist, method = "euclidean"), method = "complete")
plot(seed.c)
abline(h = 60, col = "blue", lty = "dashed")
abline(h = 40, col = "red", lty = "dashed")
c.tree3 <- cutree(seed.c, k = 3)
c.tree6 <- cutree(seed.c, k = 6)


seed.a <- hclust(dist(seed.dist, method = "euclidean"), method = "average")
plot(seed.a)
abline(h = 35, col = "blue", lty = "dashed")
abline(h = 24, col = "red", lty = "dashed")

seed.w <- hclust(dist(seed.dist, method = "euclidean"), method = "ward.D2")
plot(seed.w)
abline(h = 200, col = "blue", lty = "dashed")
abline(h = 85, col = "red", lty = "dashed")


#cut tree and create a silhouette 
seed.tree <- cutree(seed.a, k = 2)
seed.si <- silhouette(seed.tree, dmatrix = as.matrix(seed.dist)) 
plot(seed.si, main =
    "Silhouette plot of \"average linkage\" for clustering seed data")

seed.pam <- pam(seed, 3)
plot(silhouette(seed.pam))

seed.pca <- princomp(seed)
seed.pc <- seed.pca$scores[, 1:2]
plot(seed.pca$scores[, 1:2], col = seed.tree, pch = 20, cex = 2, 
    main = "Six clusters")
plot(seed.pc, col = seed.pam$cluster, pch = 20, cex = 2, main = 
    "Clusters identified by PAM")

# k-means
seed.kmeans <- kmeans(seed, 3)
kmeans.si <- silhouette(seed.kmeans$cluster, seed.dist)
plot(kmeans.si)

seed.kmeans4 <- kmeans(seed, 4)
kmeans.si <- silhouette(seed.kmeans4$cluster, seed.dist)
plot(kmeans.si)

seed.kmeans5 <- kmeans(seed, 5)
kmeans.si <- silhouette(seed.kmeans5$cluster, seed.dist)
plot(kmeans.si)

seed.kmeans6 <- kmeans(seed, 6)
kmeans.si <- silhouette(seed.kmeans6$cluster, seed.dist)
plot(kmeans.si)

plot(seed.pc, col = seed.kmeans$cluster, pch = 20, cex = 2, main = 
    "Clusters identified by k-means")

seed$pam <- seed.pam$cluster
seed$kmean <- seed.kmeans$cluster


WSS <- rep(NA, 1000)
for(i in 1:100) {
    WSS[i] <- kmeans(seed, 3)$tot.withinss
}

plot(seed, col = seed.kmeans$cluster)
plot(seed, col = seed.pam$cluster)