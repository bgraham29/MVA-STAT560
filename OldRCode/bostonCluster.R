
## cluster analysis

data<-read.table("C:/Teaching/STA6707/Data/bostonh.dat");
help(dist);
help(hclust);
x<- data;
bh.dist<- dist(x);
bh.cluster<- hclust(bh.dist);

library(cluster);
help(agnes);
bh.agnes<- agnes(x);
plot(bh.agnes);

## an example using agnes

data(votes.repub);
help(votes.repub);
agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)

op <- par(mfrow=c(2,2))
## daisy calculates dissimilarity. 
agn2 <- agnes(daisy(votes.repub), diss = TRUE, method = "complete")
plot(agn2)
agnS <- agnes(votes.repub, method = "flexible", par.meth = 0.6)
plot(agnS)
par(op)

#The diana-algorithm constructs a hierarchy of clusterings, 
#starting with one large cluster containing all n observations. 
#Clusters are divided until each cluster contains only a single 
#observation 

dia1<- diana(votes.repub); 
plot(dia1);
