# SVD example this time with image data 
install.packages("jpeg")
library(jpeg)

### read raster image
 Xras <- readJPEG("C:/Users/bgandre/Desktop/Stats Stuff/Multivariate-ClassLee/Tulip.jpg", 
            TRUE)
 plot(1:2, type = 'n')
 rasterImage(Xtrial, 2, 1, 1, 2, angle= 1800)
 


# read jpg image  
X <- readJPEG("C:/Users/bgandre/Desktop/Stats Stuff/Multivariate-ClassLee/Tulip.jpg")
# trying to turn them into gray scale
grayX <- 0.21*X[,,1]+0.71*X[,,2]+0.07*X[,,3]

par(mfrow=c(2,2))
image(grayX,col=gray((0:32)/32), main= 'original')

# SVD 
X.svd <- svd(grayX)

# collection of u vectors (a.k.a. left singular vectors)
U<-X.svd$u

# diagonal matrices of singular values 
D<-diag(X.svd$d)

# collection of V vecvtors (a.k.a. right singular vectors) 
V<-X.svd$v

# note that X = UDV'
XSVD <- U%*% D%*% t(V)
Xnew <- X.svd$u %*% diag(X.svd$d) %*% t(X.svd$v)
n <- dim(X)[1]
p <- dim(X)[2]
titlestr <- paste('full rank SVD, rank = ', rankMatrix(grayX)) #as.character(qr(grayX)$rank))
image(XSVD,col=gray((0:32)/32), main = titlestr)


# now rank r approximation
r<- 30
Xr <- U[,1:r]%*% D[1:r,1:r]%*%t(V[,1:r])
titlestr <-  paste('rank', r, 'SVD')
image(Xr, col=gray((0:32)/32),main = titlestr)


# now rank r approximation
r<- 20
Xr <- U[,1:r]%*% D[1:r,1:r]%*%t(V[,1:r])
titlestr <-  paste('rank',as.character(r), 'SVD')
image(Xr, col=gray((0:32)/32),main = titlestr)
