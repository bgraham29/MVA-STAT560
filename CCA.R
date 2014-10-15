################################################################################
install.packages("alr3")
require(alr3)
require(XML)

fileUrl <- "http://sfb649.wiwi.hu-berlin.de/fedc_homepage/xplore/tutorials/mvahtmlnode134.html"

temp <- htmlTreeParse(fileUrl, useInternal = TRUE)
## gave up on pulling info from here

carMarks <- read.csv("scrape.csv")
carMarks <- carMarks[, c(1, 2, 6, 5, 3, 4, 7, 8, 9, 10)]

S <- cov(carMarks[, 3:10])
Sxx <- S[1:2, 1:2]
Sxx.force <- matrix(c(1.41, rep(-1.11,2), 1.19), nrow = 2)
Sxy <- S[1:2, 3:8]
Sxy.force <- matrix(c(0.78, -0.71, -0.90, -1.04, -0.95, 0.18, -0.42, 0.82,
    0.77, 0.90, 1.12, 0.11), byrow = TRUE, nrow = 2)
Syy <- S[3:8, 3:8]
Syy.force <- matrix(c(0.74, -0.23, -0.45, -0.42, -0.28, 0.28, -0.23, 0.66, 
    0.52, 0.57, 0.85, 0.14, -0.45, 0.52, 0.72, 0.77, 0.68, -0.10, -0.42, 0.57, 
    0.77, 1.05, 0.76, -0.15, -0.28, 0.85, 0.68, 0.76, 1.26, 0.22, 0.28,
    0.14, -0.10, -0.15, 0.22, 0.32), byrow = TRUE, nrow = 6)


# "%^%" <- function(x, n) 
#	with(eigen(x), vectors %*% (values^n * t(vectors)))

require(expm) # for "%^%"
K <- (solve(Sxx)%^%(.5)) %*% Sxy %*% (solve(Syy)%^%(.5))
K.force <- (solve(Sxx.force)%^%(.5)) %*% Sxy.force %*% (solve(Syy.force)%^%(.5))
decomp <- svd(K) # note decomp$d *are* the SQRT of the eigenvalues
decomp.force <- svd(K.force)

eta <- t(decomp$u[,1]) %*% t(as.matrix(carMarks[, 3:4]))
phi <- t(decomp$v[,1]) %*% t(as.matrix(carMarks[, 5:10]))

eta <- t(decomp.force$u[,1]) %*% t(as.matrix(carMarks[, 3:4]))
phi <- t(decomp.force$v[,1]) %*% t(as.matrix(carMarks[, 5:10]))


plot(eta, phi)
text(eta, phi, labels = carMarks$Type)
