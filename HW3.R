################################################################################
require(alr3)

# Q1 
S1 <- matrix(c(2, .5, .5, 2), nrow = 2)
S2 <- matrix(c(5, 4, 4, 5), nrow = 2)
mu1 <- matrix(c(0,0))
mu2 <- matrix(c(1,1))

b01 <- 1/2 * t(mu1) %*% solve(S1) %*% mu1 + 1/2 * log(det(S1))
b02 <- 1/2 * t(mu2) %*% solve(S2) %*% mu2 + 1/2 * log(det(S2))
b1 <- 1/2 * -solve(S1) %*% mu1
b2 <- 1/2 * -solve(S2) %*% mu2
c1 <- 1/2 * solve(S1)
c2 <- 1/2 * solve(S2)

b01; b1; c1
b02; b2; c2

require(MASS)
M <- mvrnorm(n = 100, mu1, S1)

f1 <- b01 + t(b1) %*% t(M) + t(M) %*% c1 %*% M


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
train <- crossVal[[1]]
PC <- eigen(cov(banknote[train, -7]))$vectors[,1]
mu1 <- apply(subset(banknote[train,], banknote[train,]$Y == 1), 2, 
    mean)[-7]
mu0 <- apply(subset(banknote[train,], banknote[train,]$Y == 0), 2, 
    mean)[-7]
n <- nrow(banknote[-train, ]))
preds <- rep(NA, nrow(banknote[-train,]))
for(i in 1:nrow
dist1 <- sqrt(sum((banknote[1, -7] - mu1)^2))
dist0 <- sqrt(sum((banknote[1, -7] - mu0)^2))
ifelse(dist1 < dist0, 1, 0)

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
