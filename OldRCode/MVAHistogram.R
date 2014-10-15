
## swiss bank notes.

data<-read.table("C:/Teaching/STA6707/Data/bank2.dat");
 x<- data[,6]
 xf<- x[101:200]

op <- par(mfrow=c(2, 2))
 hist(xf, seq(137.75, 141, 0.1),xlab="h=0.1", ylab="diagonal", main="Swiss bank notes")
 hist(xf, seq(137.75, 141, 0.3),xlab="h=0.3", ylab="diagonal", main="Swiss bank notes")
 hist(xf, seq(137.75, 141, 0.2),xlab="h=0.2", ylab="diagonal", main="Swiss bank notes")
 hist(xf, seq(137.75, 141, 0.4),xlab="h=0.4", ylab="diagonal", main="Swiss bank notes")
par(op)

## more details using help(hist);

## kernel density estimate

 x<- data[,6]
 lim1 <- min(x)
 lim2 <- max(x)
 x1<- x[1:100]
 x2<- x[101:200]
 fh1<- density(x1)
 fh2<- density(x2)

plot.density(fh1, xlim=c(lim1,lim2), xlab="counterfeit        /         genuine")
lines(fh2, lty=2)

## more on help(density);

## you combine hist and density
xg<- x[1:100];
xf<- x[101:200];
summary(xg);
summary(xf);
#> summary(xf)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  137.8   139.2   139.5   139.4   139.8   140.6 
#> summary(xg)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  139.6   141.2   141.5   141.5   141.8   142.4

hist(xg, seq(139.50, 142.50, 0.25), freq=F, xlim=c(lim1,lim2), xlab="counterfeit  /    genuine", main="Histogram of X6");
hist(xf, seq(137.75, 140.75, 0.25), freq=F, add=T, lty=2);
lines(fh1, lty=3);
lines(fh2, lty=4);
