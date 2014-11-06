################################################################################
# Canonical Correlation Analysis

n <- dim(carMarks)[1]
S <- cov(carMarks[, 3:10])*(n-1)/n		# convert to MLE 
round(S, 2)

SXX <- S[1:2, 1:2]
SYY <- S[3:8, 3:8]
SXY <- S[1:2, 3:8]

K <- (SXX %^% (-1/2)) %*% SXY %*% (SYY %^% (-1/2))
UDV <- svd(K)	# remember that sqrt(eigenvalues) is used here

coeff <- UDV$d

a <- UDV$u[, 1]
b <- UDV$v[, 1]

eta <- t(-a) %*% t(as.matrix(carMarks[, 3:4]))	# price & value index
phi <- t(-b) %*% t(as.matrix(carMarks[, 5:10]))	# 
	# economy, service & handling (qualitative)
	# negative weights on design, safety, sportiness

plot(eta, phi)
text(eta, phi, labels = carMarks$Type)
# note that plot matches online figure relatively closely, but doesn't match 
#   textbook figure well at all. Textbook numbers not lining up with 
#   online dataset

## Bartlett's statistic:
# -(n - (p + q + 3)/2) * log(sum( 1 - d))

# H0: X and Y are uncorrelated
T <- -(n - (dim(UDV$u)[1] + dim(UDV$v)[1] + 3)/ 2) * log(sum(1 - UDV$d^2))
pchisq(T, dim(UDV$u)[1]*dim(UDV$v)[1], lower.tail = FALSE)

# H0: rho[2] is equal to 0 { test whether only "s" of the coefficients are nonzero
T <- -(n - (dim(UDV$u)[1] + dim(UDV$v)[1] + 3)/ 2) * log(sum(1 - UDV$d[2]^2)) 
pchisq(T, (dim(UDV$u)[1] - 1)*(dim(UDV$v)[1] - 1), lower.tail = FALSE)
