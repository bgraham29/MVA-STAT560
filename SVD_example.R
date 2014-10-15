#data <-(read.table("D:/Teaching/ST560/Examples/university.DAT"))
data <- read.table("university.DAT")
X <-data.matrix(data)

X.svd <- svd(X)

# collection of u vectors (a.k.a. left singular vectors)
U<-X.svd$u

# diagonal matrices of singular values 
D<-diag(X.svd$d)

# collection of V vecvtors (a.k.a. right singular vectors) 
V<-X.svd$v

# note that X = UDV'

XSVD <- U%*% D%*% t(V)

n <- dim(X)[1]
p <- dim(X)[2]

par(mfrow=c(2,2))
persp(seq(1, n, 1), seq(1, p, 1), X, phi = 50, theta = 75,col = "red",
  xlab = "University", ylab = "Variables",
  main = "X"
)

persp(seq(1, n, 1), seq(1, p, 1), XSVD, phi = 50, theta = 75,col = "lightblue",
  xlab = "University", ylab = "Variables",
  main = "UDV'"
)

# now rank 1 approximation 

X1 <- D[1]*U[,1]%*%t(V[,1])

persp(seq(1, n, 1), seq(1, p, 1), X1, phi = 50, theta = 75,col = "lightblue",
  xlab = "University", ylab = "Variables",
  main = "rank 1"
)

# now rank 2 approximation 
X2 <- D[1,1]*U[,1]%*%t(V[,1]) + D[2,2]*U[,2]%*%t(V[,2])

# or equivalently 
X2 <- U[,1:2]%*% D[1:2,1:2]%*%t(V[,1:2])

persp(seq(1, n, 1), seq(1, p, 1), X2, phi = 50, theta = 75,col = "lightblue",
  xlab = "University", ylab = "Variables",
  main = "rank 2 "
)

# now check the approximation error decreases as the rank increases 

norm(X-X1)
norm(X-X2)
norm(X-XSVD)
