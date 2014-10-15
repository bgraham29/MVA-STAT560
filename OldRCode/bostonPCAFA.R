

## basic buid-in function in R {stats}
## princomp and factanal.


data<-read.table("C:/Teaching/STA6707/Data/bostonh.dat");
x<- data[,-4] # x4 is binary.


# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix

fit <- princomp(x, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
#fit$scores # the principal components
biplot(fit) 


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation

fit <- factanal(x, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
load <- fit$loadings;

par(mfrow=c(2, 2));
plot(load[,1:2],type="n") # plot factor 1 by 2
text(load[,1:2],labels=names(x),cex=.7) # add variable names 
plot(load[,c(1,3)],type="n") # plot factor 1 by 3
text(load[,c(1,3)],labels=names(x),cex=.7) # add variable names 
plot(load[,2:3],type="n") # plot factor 2 by 3
text(load[,2:3],labels=names(x),cex=.7) # add variable names 


fit.nr<- factanal(x, 3, rotation="none")
print(fit.nr, digits=2, cutoff=.3, sort=TRUE)
load <- fit.nr$loadings;

par(mfrow=c(2, 2));
plot(load[,1:2],type="n") # plot factor 1 by 2
text(load[,1:2],labels=names(x),cex=.7) # add variable names 
plot(load[,c(1,3)],type="n") # plot factor 1 by 3
text(load[,c(1,3)],labels=names(x),cex=.7) # add variable names 
plot(load[,2:3],type="n") # plot factor 2 by 3
text(load[,2:3],labels=names(x),cex=.7) # add variable names 


## the loadings saved in lsp files can be 
## visualized in 3-D. 

## there is a R-package {psych}. 
## http://personality-project.org/r/

library(psych);
fit1<- fa(x, 3); #MLM
> attributes(fit1)
$names
 [1] "residual"     "fit"          "fit.off"      "factors"      "n.obs"        "dof"          "objective"    "criteria"     "STATISTIC"   
[10] "PVAL"         "Call"         "r.scores"     "R2"           "valid"        "score.cor"    "weights"      "communality"  "uniquenesses"
[19] "values"       "loadings"     "fm"           "fn"          
$class
[1] "psych" "fa"  
factor.plot(fit1);
load <- fit1$loadings;

fit2<- factor.pa(x, 3); ## pc methods
fit3<- factor.minres(x, 3); 
fit4<- factor.wls(x, 3);

## for well behaved matrix, factanal using MLM is preferred. 

## Example from {psych}

data(bfi);
x<- as.matrix(bfi);
fit1<- fa(x, 5);
load <- fit1$loadings;
library(lattice);
ftype<- rep(seq(5), rep(5,5));
splom(~ load[1:25,1:5], pch=ftype)

## there is another package called {FactoMineR}
## Factor Analysis and Data Mining with R
## seems interesting stuffs!


library(FactoMineR);
data("decathlon");
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13);
plot(res.pca, habillage = 13);
