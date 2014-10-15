setwd("D:/Teaching/ST560/Examples/")

# install.packages("datasets")

X<-iris[,-5]

# sample PCA using eigen-decomposition of covariance matrix (or corrleation if ) 
spr.X <-princomp(X)
V<-spr$loadings
L<-(spr$sdev)^2
Z <-spr$scores


## sample PCA using SVD
#gpr <- prcomp(X)
#V <- gpr$rotation
#L <- (gpr$sdev)^2
#Z <- gpr$x


# scatterplot of input x
pdf("iris_scatterplot.pdf")
pairs(X,pch=c(rep(1,50),rep(2,50),rep(3,50)),col=c(rep("blue",50),rep("red",50),rep("green",50)))
dev.off()

# scatterplot of principal components
pdf("iris_pcascatterplot_wocolor.pdf")
pairs(Z,pch=c(rep(1,50),rep(2,50),rep(3,50)),col="blue")
dev.off()

pdf("iris_pcascatterplot_wcolor.pdf")
pairs(Z,pch=c(rep(1,50),rep(2,50),rep(3,50)),col=c(rep("blue",50),rep("red",50),rep("green",50)))
dev.off()

# variances of each principal component
pdf("iris_screeplot.pdf")
par(mfrow=c(1,2))
plot(L,type="b",xlab="component",ylab="lambda",main="Scree plot")
plot(cumsum(L)/sum(L)*100,ylim=c(0,100),type="b",xlab="component",ylab="cumulative propotion (%)",main="Cum. Scree plot")
dev.off()

# biplot
pdf("iris_biplot.pdf")
par(mfrow=c(1,1))
biplot(spr, choices = 1:2, scale = 1, pc.biplot = FALSE)
dev.off()

# loadings 
pdf("iris_loadings.pdf")
par(mfrow=c(1,2))
barplot(spr$loadings[,1],main="PC1 loadings")
barplot(spr$loadings[,2],main="PC2 loadings")
dev.off()



