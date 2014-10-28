################################################################################
#
# Homework Problems due 10/29/14 for STAT 560 Multivariate Analysis

# Q1

male <- read.csv("./Data/male_bird.csv")
names(male) <- c("Tail", "Wing")
female <- read.csv("./Data/female_bird.csv")
names(female) <- c("Tail", "Wing")

# use density function in package 'SciViews"

require(SciViews)

pairs(male[-31, ], diag.panel = panel.density)
pairs(female, diag.panel = panel.density)

malePC <- princomp(male)$scores
femalePC <- princomp(female)$scores

pairs(malePC, diag.panel = panel.density)
pairs(femalePC, diag.panel = panel.density)

S.male <- cov(male)
S.female <- cov(female)
male.means <- colMeans(male)
male.center <- male - rep(male.means, rep.int(nrow(male), ncol(male)))
male.chi <- diag(as.matrix(male.center) %*% solve(S.male) %*% t(as.matrix(male.center)))

S1.inverse <- solve(S.male)
for(i in 1:nrow(male.center)) { 
    male.chi[i] <- as.matrix(male.center[i, ]) %*% S1.inverse %*% 
        t(as.matrix(male.center[i, ]))
}
x <- sort(qchisq(male.chi, df = 2))
y <- rchisq(length(x), df = 2)
qqplot(y, qchisq(male.chi, df = 2), pch = 18,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 2]))
abline(0,1, col = "blue")
for(i in 1:50) { 
    y <- rchisq(45, df = 2)
    points(y, qchisq(male.chi, df = 2), col = "blue")
}


female.means <- colMeans(female)
female.center <- female - rep(female.means, rep.int(nrow(female), ncol(female)))
S2.inverse <- solve(S.female)
female.chi <- diag(as.matrix(female.center) %*% S2.inverse %*% t(as.matrix(female.center)))

qqChiSq(male)
which(male.chi == max(male.chi))
qqChiSq(male[-31, ])

qqChiSq(female)
qqChiSq(female, pch = 18)
y <- sort(female.chi)
for(i in 1:100) { 
    x <- sort(rchisq(length(y), df = 2))
    points(x, y, col = "blue")
}

out <- which(female.chi == max(female.chi))
qqChiSq(female[-out, ], pch = 18)
y <- sort(female.chi[-out])
for(i in 1:100) { 
    x <- sort(rchisq(length(y), df = 2))
    points(x, y, col = "blue")
}

# remove observation #31 from male population to complete analyses

n <- nrow(male[-31, ])
p <- ncol(male[-31, ])
S <- cov(male[-31, ])
x.bar <- apply(male[-31, ], 2, mean)


# b.1

qt(1 - .05/2, n - 1) * sqrt(S[1, 1]/n)
qt(1 - .05/2, n - 1) * sqrt(S[2, 2]/n)

# b.2
eigen(S)
lambda <- eigen(S)$values

2 * sqrt((p*(n-1)* lambda[1] * qf(.05, p, n-p, lower.tail = FALSE))/(n*(n-p)))
2 * sqrt((p*(n-1)* lambda[2] * qf(.05, p, n-p, lower.tail = FALSE))/(n*(n-p)))

plot(male[-31, ])
points(x.bar[1], x.bar[2], pch = 20, col = "red", cex = 2)

require(ellipse)
lines(ellipse(cov(male[-31, ]), centre = x.bar, level = .5), col = "blue")
abline(v = c(x.bar[1] + c(-1, 1) * qt(1 - .05/2, n - 1) * sqrt(S[1, 1]/n)))
abline(h = c(x.bar[2] + c(-1, 1) * qt(1 - .05/2, n - 1) * sqrt(S[2, 2]/n)))

function

# b.3 

a <- matrix(c(0,1), nrow = 2)
a <- matrix(c(1,0), nrow = 2)
t(a) %*% x.bar	# 189.3
sqrt((n-1)/n * p/(n-p) * qf(.05, p, n-p, lower.tail = FALSE) * 
    (t(a) %*% S %*% a))


# Q2: R code that will generate random sample of size n from multivariate
#   normal with mean m0 and covariance S0.

rmvnorm <- function(n = 100, m0, S0) {
    p <- length(m0)
    x <- matrix(rep(NA,n*p), nrow = n)
    for(i in 1:p) {
        x[ , i] <- rnorm(n)
    }
    Lambda <- diag(E.S0$values)
    phi <- E.S0$vectors
    Q <- Lambda %^% (-1/2) %*% phi 
    y <- (Q %*% t(x))
    y <- apply(y, 1, function(x) x + m0)
    return(y)
}

rMVN <- function(n = 100, m0, S0) {
    p <- length(m0)
    x <- matrix(rep(NA,n*p), nrow = n)
    for(i in 1:p) {
        x[ , i] <- rnorm(n)
    }
    y <- (x %*% (S0 %^% (1/2))) 
    y <- apply(y, 1, function(x) x + m0)
    t(y)
}


