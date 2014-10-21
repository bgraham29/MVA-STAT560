################################################################################
#

food <- read.csv("DataMaybe\\food.csv", nrow = 12, stringsAsFactors = FALSE)
food$children <- as.integer(substr(food$Group, 3, 3))
food$Group <- substr(food$Group, 1, 2)

#  food expenditures of French families with 2, 3, 4, or 5 children
#  do certain household types prefer certain food types
#  MA = manual; EM = employees; CA = managers


R <- cor(food[, -c(1, 9)])	# high correlation meat / poultry (0.98)
					# small correlation milk / wine (0.01)


# note the origin has no specific meaning ("zero" consumer), so compare 
# consumption of a given family to "average family". i.e. center data
# also note p = 7 are not homoscedastic. Standardize variables so each has 
# same weight ~N(0,1)
# for convenience, Haerdle scales variables by 1/sqrt(n) for plotting

Xstar <- as.matrix(1/sqrt(12) * as.data.frame(scale(food[, 2:8])))
# same as Xstar2 below, so "scale" is doing its work correctly


Hnew <- food[, 2:8]
Hnew <- Hnew - matrix(apply(Hnew,2,mean), nrow(Hnew), ncol(Hnew), byrow=T)

D <- apply(Hnew, 2, function(x) 1/(sd(x) * sqrt(11/12)))	# Hardle uses 1/n instead of 1/(n-1)
D <- diag(D)

Xstar2 <- as.matrix(Hnew) %*% D
Xstar2 <- Xstar2 / sqrt(12)

lambda <- eigen(t(Xstar2) %*% Xstar2)$values  # these are now the same values as for 
#                                                R above, once sd values are adjusted

u <- eigen(t(Xstar2) %*% Xstar2)$vectors
tau2 <- sum(lambda[1:2]) / sum(lambda)	# 88% of the inertia

w1 <- sqrt(lambda[1]) * u[, 1]     # the meat / fruit factor
w2 <- sqrt(lambda[2]) * -u[, 2]    # the bread / wine factor

plot(w1, w2, xlim = c(-1.05, 0.6), ylim = c(-1, 0.4), col = "white",
  main = "Representation of variables in 2-dimensional space")
text(w1, w2, labels = colnames(food[, 2:8]))
abline(h = 0); abline(v = 0)

W <- (v * matrix(sqrt(lambda), nrow = nrow(v), ncol = ncol(v), byrow = TRUE))[, 1:2]


v <- 

z1 <- ((Xstar2 %*% v) * sqrt(n/p))[, 1:2]

plot(z1[, 1], z2[, 2])
abline(h = 0); abline(v = 0)
text(z1[, 1], z2[, 2], labels = food$Group, col = "blue", pos = 2)
text(z1[, 1], z2[, 2], labels = food$children, col = "red", pos = 4)
# CA5 factor lies close to 


plot(w1, w2, xlim = c(-1.7, 1.3), ylim = c(-1, 1.0), col = "white",
  main = "Representation of variables in 2-dimensional space")
text(w1, w2, labels = colnames(food[, 2:8]))
abline(h = 0); abline(v = 0)
points(z1[, 1], z2[, 2], pch = "+")
text(z1[, 1], z2[, 2], labels = food$Group, col = "blue", pos = 2)
text(z1[, 1], z2[, 2], labels = food$children, col = "red", pos = 4)


tau3 <- sum(lambda[1:3]) / sum(lambda)
tau4 <- sum(lambda[1:4]) / sum(lambda)

## Exercise 9.4 Apply to Swiss bank note data

R <- cor(BankNotes[, 1:6])
lambda <- eigen(R)$values
u <- eigen(R)$vectors

w1 <- sqrt(lambda[1]) * u[, 1]
w2 <- sqrt(lambda[2]) * u[, 2]
W <- u[, 1:2] %*% diag(sqrt(lambda[1:2]))

plot(w1, w2, col = "white",
  main = "Representation of variables in 2-dimensional space")
text(w1, w2, labels = colnames(BankNotes)[1:6])
abline(h = 0); abline(v = 0)
cbind(colnames(BankNotes)[1:6], w1, w2)

z <- (as.matrix(scale(BankNotes[, 1:6])) %*% u)[, 1:2]
plot(z[, 1], z[, 2], type = "n")
points(z[1:100, 1], z[1:100, 2], col = "blue")
points(z[101:200, 1], z[101:200, 2], col = "red")
abline(h = 0); abline(v = 0)

## Exercise 9.5 Apply to time budget data
timeBudget <- read.csv("DataMaybe\\TimeBudget.csv")

