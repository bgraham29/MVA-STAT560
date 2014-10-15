
## swiss bank notes.

data<-read.table("C:/Teaching/STA6707/Data/bank2.dat");
x <- data;
x <- x[c(5,6)];
par(mfrow=c(1, 2), cex=0.9);
plot(x[,1], x[,2]);
x<- cbind(x, rep(1, 200));
x[1:100,3]<-rep(0,100)
plot(x[,1],x[,2], pch=as.numeric(x[,3]), main="Swiss bank notes", xlab="upper inner frame (X5)", ylab="diagonal (X6)");


library(KernSmooth)
x<- data[c(5,6)];
x <- bkde2D(x, 0.32857)
contour(x$x1, x$x2, x$fhat, nlevels=8, drawlabels=FALSE)

x <- data
i<- 2
op <- par(mfrow=c(4, 4), cex=.2)
while(i<6){
     i <- i+1
     j <- 2
while(j<6){
     j <- j+1
     
     if(i==j){
     plot(i, type="n", axes=FALSE, xlab="", ylab="", main=i, cex.main=5)}

     if(i<j){
     xx <- cbind(x[,i],x[,j],c(rep(0,100),rep(1,100)))
     zz <- bkde2D(xx[,-3], 0.4)
     contour(zz$x1, zz$x2, zz$fhat, nlevels=12, drawlabels=FALSE, xlab="X", ylab="Y")}

     if(i>j){
     yy <- cbind(x[,i],x[,j],c(rep(0,100),rep(1,100)))
     plot(yy[,-3], pch=as.numeric(yy[,3]), xlab="X", ylab="Y", cex=3)}
     }}
par(op)
