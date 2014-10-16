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