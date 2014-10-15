install.packages("pls")
library("pls")

require(MASS)
data(Boston) 


X <- Boston[c(-14)]

# PCA based on covariance matrix

spr <-princomp(X)
V<-spr$loadings
L<-(spr$sdev)^2
Z <-spr$scores

# variances of each principal component
pdf("BH_screeplot.pdf")
par(mfrow=c(1,2))
plot(L,type="b",xlab="component",ylab="lambda",main="Scree plot")
plot(cumsum(L)/sum(L)*100,ylim=c(0,100),type="b",xlab="component",ylab="cumulative propotion (%)",main="Cum. Scree plot")
dev.off()

pdf("BH_loadings.pdf")
par(mfrow=c(1,2))
barplot(V[,1],main="PC1 loadings")
barplot(V[,2],main="PC2 loadings")
dev.off()

# PCR using "PCR" function 
BHpcr <- pcr(medv ~ ., ncomp = 10, data =Boston)
summary(BHpcr)

# PCR using "lm" function. Use the first PCs as the covarites in the regression analysis. 
BHpcr2 <- lm(Boston$medv ~ Z[,1:3])
summary(BHpcr2)


# PCR based on correlation matrix
spr <-princomp(X,cor = TRUE)
V<-spr$loadings
L<-(spr$sdev)^2
Z <-spr$scores

BHpcr3 <- lm(Boston$medv ~ Z[,1:3])
summary(BHpcr3)
