

data<-read.table("C:/Teaching/STA6707/Data/bank2.dat")
x <- data;
 Type <- c(rep(0,100), rep(1,100))
 fType <- factor(Type, levels=0:1)
 levels(fType) <- c("GENUINE","CONTERFEIT")
##bankdata<- cbind(x, fType);
##colnames(x)<- c("Length", "Diagonal", "Type")


xmat<- as.matrix(x);
## compare sdev above with svd(x)$d;
## help(svd) and help(eigen) to see the definitions. 
## and eigenvalues of x^Tx.

svd(xmat)$d;
eigen(t(xmat)%*%xmat)$values;

## but is not centered;
xmean<- apply(xmat, 2, mean);
cx<- t(t(x)-xmean);
svd(cx)$d;
##[1] 24.434825 13.645090  6.959837  6.223913  4.117907  2.658462
eigen(t(cx)%*%cx)$values;
##[1] 597.06067 186.18848  48.43933  38.73709  16.95716   7.06742

## how it related to cov(x);
covhat<- cov(x);
eigen(covhat)$values;
#> eigen(covhat)$values;
#[1] 3.00030487 0.93562052 0.24341371 0.19465874 0.08521185 0.03551468

eigen(t(cx)%*%cx/199)$values;
##[1] 3.00030487 0.93562052 0.24341371 0.19465874 0.08521185 0.03551468
eigen(t(cx)%*%cx/200)$values
##[1] 2.9853033 0.9309424 0.2421966 0.1936855 0.0847858 0.0353371


xpca<- princomp(x);
attributes(xpca);
#> attributes(xpca)
#$names
#[1] "sdev"     "loadings" "center"   "scale"    "n.obs"    "scores"   "call"    
#
#$class
#[1] "princomp"
# help(princomp) explains the definitions.
xpca$sdev
## based on n;
#   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6 
#1.7278030 0.9648536 0.4921348 0.4400971 0.2911800 0.1879817 

dim(xpca$scores)
#[1] 200   6
cov(xpca$scores) ## based on n-1;
#              Comp.1        Comp.2        Comp.3        Comp.4        Comp.5        Comp.6
#Comp.1  3.000305e+00  7.741067e-17 -6.696285e-16  9.414174e-16 -6.417659e-16  2.118825e-16
#Comp.2  7.741067e-17  9.356205e-01 -2.739298e-16 -9.322995e-17 -2.994211e-17  1.985517e-16
#Comp.3 -6.696285e-16 -2.739298e-16  2.434137e-01 -4.772536e-17 -3.469002e-17  1.320276e-16
#Comp.4  9.414174e-16 -9.322995e-17 -4.772536e-17  1.946587e-01  5.991569e-17  1.141421e-16
#Comp.5 -6.417659e-16 -2.994211e-17 -3.469002e-17  5.991569e-17  8.521185e-02 -8.461029e-18
#Comp.6  2.118825e-16  1.985517e-16  1.320276e-16  1.141421e-16 -8.461029e-18  3.551468e-02
#

summary(xpca)
#Importance of components:
#                         Comp.1    Comp.2     Comp.3     Comp.4     Comp.5      Comp.6
#Standard deviation     1.727803 0.9648536 0.49213478 0.44009709 0.29118000 0.187981656
#Proportion of Variance 0.667517 0.2081597 0.05415542 0.04330827 0.01895819 0.007901414
#Cumulative Proportion  0.667517 0.8756767 0.92983212 0.97314039 0.99209859 1.000000000

plot(xpca);
biplot(xpca);
par(mfrow=c(2, 2));
plot(xpca$scores[,1], xpca$scores[,2], 
    pch=as.numeric(fType), xlab="PC1", ylab="PC2", main="Swiss bank notes");
plot(xpca$scores[,1], xpca$scores[,3], 
    pch=as.numeric(fType), xlab="PC1", ylab="PC3", main="Swiss bank notes");
plot(xpca$scores[,2], xpca$scores[,3], 
    pch=as.numeric(fType), xlab="PC2", ylab="PC3", main="Swiss bank notes");
plot(xpca);

## you supply a different cov matrix. 
## or you may scale the data first then use correlation matrix. 
## normalized PCA in Section 9.5.

xpca<- princomp(x, cor=T);
summary(xpca);
#Importance of components:
#                          Comp.1    Comp.2    Comp.3     Comp.4     Comp.5     Comp.6
#Standard deviation     1.7162629 1.1305237 0.9322192 0.67064796 0.51834053 0.43460313
#Proportion of Variance 0.4909264 0.2130140 0.1448388 0.07496145 0.04477948 0.03147998
#Cumulative Proportion  0.4909264 0.7039403 0.8487791 0.92374054 0.96852002 1.00000000
#> sum(xpca$sdev^2)
#[1] 6


# what if randomly create two clouds of multivariate normal;
# plot them in pca;

sep.clouds<- function(p){
set.seed(123);
x1<- matrix(rnorm(p*100), ncol=p);

x2<- matrix(rnorm(p*100), ncol=p);

x<- rbind(x1, x2);
xpca<- princomp(x);
plot(xpca$scores[,1], xpca$scores[,2], 
    pch=c(rep(1,100), rep(2, 100)), xlab="PC1", ylab="PC2", main="two random clouds");
summary(xpca);

## now let's move one cloud away a bit;

par(mfrow=c(2, 2));
x<- rbind(x1, x2);
xpca<- princomp(x);
plot(xpca$scores[,1], xpca$scores[,2], 
    pch=c(rep(1,100), rep(2, 100)), xlab="PC1", ylab="PC2", main="two random clouds");
nx2<- t(t(x2)+rep(0.5, p));
x<- rbind(x1, nx2);
xpca<- princomp(x);
plot(xpca$scores[,1], xpca$scores[,2], 
    pch=c(rep(1,100), rep(2, 100)), xlab="PC1", ylab="PC2", 
    main="two random clouds differ rep(0.5, p)");
nx2<- t(t(x2)+rep(1, p));
x<- rbind(x1, nx2);
xpca<- princomp(x);
plot(xpca$scores[,1], xpca$scores[,2], 
    pch=c(rep(1,100), rep(2, 100)), xlab="PC1", ylab="PC2", 
    main="two random clouds differ rep(1, p)");
nx2<- t(t(x2)+rep(2, p));
x<- rbind(x1, nx2);
xpca<- princomp(x);
plot(xpca$scores[,1], xpca$scores[,2], 
    pch=c(rep(1,100), rep(2, 100)), xlab="PC1", ylab="PC2", 
    main="two random clouds differ rep(2, p)");
}


