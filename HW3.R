################################################################################
require(alr3)

# Q1 
S1 <- matrix(c(2, .5, .5, 2), nrow = 2)
S2 <- matrix(c(5, 4, 4, 5), nrow = 2)
mu1 <- matrix(c(0,0))
mu2 <- matrix(c(1,1))

b01 <- 1/2 * t(mu1) %*% solve(S1) %*% mu1 + 1/2 * log(det(S1))
b02 <- 1/2 * t(mu2) %*% solve(S2) %*% mu2 + 1/2 * log(det(S2))
b1 <- -solve(S1) %*% mu1
b2 <- -solve(S2) %*% mu2
c1 <- 1/2 * solve(S1)
c2 <- 1/2 * solve(S2)

b01; b1; c1
b02; b2; c2

require(MASS)
x1 <- mvrnorm(n = 100, mu1, S1)
x2 <- mvrnorm(n = 100, mu2, S2)


plot(x2[, 1], x2[, 2], col = "blue", xlab = "x1", ylab = "x2")
points(x1[, 1], x1[, 2], col = "red")
points(1, 1, pch = 18, cex = 2, col = "blue")
points(0, 0, pch = 18, cex = 2, col = "red")
points(.5, 2, pch = 20, cex = 2.5, col = "red")
points(.5, 2, pch = 20, cex = 1.5, col = "green")
points(4, 2, pch = 20, cex = 2.5, col = "blue")
points(4, 2, pch = 20, cex = 1.5, col = "green")


M <- matrix(c(.5, 2), nrow = 2)
M <- matrix(c(4, 2), nrow = 2)

f1 <- b01 + t(b1) %*% M + t(M) %*% c1 %*% M
f2 <- b02 + t(b2) %*% M  + t(M) %*% c2 %*% M
f1;f2
################################################################################

# Q3

head(banknote)
set.seed(1)
train <- sample(1:200, 100)
table(banknote$Y[train])
bank.lda <- lda(Y ~ ., banknote, subset = train)

crossVal <- split(sample(1:200, 200), ceiling(seq_along(1:200)/20))

train <- crossVal[[10]]
train.lda <- lda(Y ~ ., banknote, subset = train)
#compute training misclassification rate
predtraincls<- predict(train.lda, banknote[train,])$class
sum(predtraincls != banknote$Y[train])/length(predtraincls)

#predictions for test data
predtestcls<- predict(train.lda, banknote[-train,])$class
#true labels
truecls <- banknote$Y[-train]
#compute testing misclassification rate
sum(predtestcls != truecls)/length(predtestcls)


#QDA
train <- crossVal[[10]]
train.qda <- qda(Y ~ ., banknote, subset = train)
predtraincls<- predict(train.qda, banknote[train,])$class
sum(predtraincls != banknote$Y[train])/length(predtraincls)

#predictions for test data
predtestcls<- predict(train.qda, banknote[-train,])$class
#true labels
truecls <- banknote$Y[-train]
#compute testing misclassification rate
sum(predtestcls != truecls)/length(predtestcls)

# NEAREST CENTROID 
train <- crossVal[[2]]
mu1 <- apply(subset(banknote[train,], banknote[train,]$Y == 1), 2, 
    mean)[-7]
mu0 <- apply(subset(banknote[train,], banknote[train,]$Y == 0), 2, 
    mean)[-7]
S1 <- cov(subset(banknote[train, -7], banknote[train, ]$Y== 1))
S0 <- cov(subset(banknote[train, -7], banknote[train, ]$Y== 0))
X <- as.matrix(banknote[train,-7])
n <- nrow(X)
predictedClass <- rep(NA, n)
for(i in 1:n){
    L1 <- (t(X[i, ]) - mu1) %*% solve(S1) %*% t(t(X[i, ]) - mu1) + log(det(S1))
    L0 <- (t(X[i, ]) - mu0) %*% solve(S0) %*% t(t(X[i, ]) - mu0) + log(det(S0))
    predictedClass[i] <- ifelse(L1 < L0, 1, 0)
}


# NEAREST CENTROID using PC projection
train <- crossVal[[4]]
PC <- eigen(cov(banknote[train,-7]))
mu1 <- apply(subset(banknote[train,], banknote[train,]$Y == 1), 2, 
    mean)[-7] %*% PC$vectors[1, ]
mu0 <- apply(subset(banknote[train,], banknote[train,]$Y == 0), 2, 
    mean)[-7] %*% PC$vectors[1, ]
X <- as.matrix(banknote[train,-7])
scores <- as.vector(X %*% PC$vectors[1, ])
predtraincls <- ifelse(sqrt((scores - mu1)^2) < 
    sqrt((scores - mu0)^2), 1, 0)
sum(predtraincls != banknote$Y[train])/length(predtraincls)

Y <- as.matrix(banknote[-train, -7])
scores <- as.vector(Y %*% PC$vectors[1, ])
predtestcls<- ifelse(sqrt((scores - mu1)^2) < 
    sqrt((scores - mu0)^2), 1, 0)
#true labels
truecls <- banknote$Y[-train]
#compute testing misclassification rate
sum(predtestcls != truecls)/length(predtestcls)

# NEAREST CENTROID using Euclidean distance
train <- crossVal[[10]]
mu1 <- matrix(apply(subset(banknote[train,], banknote[train,]$Y == 1), 2, 
    mean)[-7], ncol = 1)
mu0 <- matrix(apply(subset(banknote[train,], banknote[train,]$Y == 0), 2, 
    mean)[-7], ncol = 1)
X <- as.matrix(banknote[train,-7])
predtraincls <- rep(NA, 20)
for(i in 1:20) {
    d1 <- sum((X[i, ] - mu1)^2)
    d0 <- sum((X[i, ] - mu0)^2)
    predtraincls[i] <- ifelse(d1 < d0, 1, 0)
}
sum(predtraincls != banknote$Y[train])/length(predtraincls)

Y <- as.matrix(banknote[-train, -7])
predtestcls <- rep(NA, nrow(Y))
for(i in 1:nrow(Y)) {
    d1 <- sum((Y[i, ] - mu1)^2)
    d0 <- sum((Y[i, ] - mu0)^2)
    predtestcls[i] <- ifelse(d1 < d0, 1, 0)
}
#true labels
truecls <- banknote$Y[-train]
#compute testing misclassification rate
sum(predtestcls != truecls)/length(predtestcls)

################################################################################

R11 <- matrix(c(1.0, 0.615, 0.615, 1), nrow = 2)
R12 <- matrix(c(-.111, -.266, -.195, -.085), nrow = 2, byrow = TRUE)
R21 <- t(R12)
R22 <- matrix(c(1, -.269, -.269, 1), nrow = 2)

C <- (solve(R11)%^%(1/2))%*% R12 %*% solve(R22) %*% 
    R21 %*% (solve(R11)%^%(1/2))
D <- (solve(R22)%^%(1/2))%*% R21 %*% solve(R11) %*% 
    R12 %*% (solve(R22)%^%(1/2))
lambdac <- eigen(C)$values
lambdad <- eigen(D)$values

a <- (solve(R11)%^%(1/2)) * C
b <- (solve(R22)%^%(1/2)) * D

u <- eigen(solve(R11) %*% R12 %*% solve(R22) %*% R21)
v <- eigen(solve(R22) %*% R21 %*% solve(R11) %*% R12)

chi.star <- -(48 - 1 -1/2 *(2 + 2 + 1)) * log(prod(1 - u$values))
