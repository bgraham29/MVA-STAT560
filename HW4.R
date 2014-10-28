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

pairs(male, diag.panel = panel.density)
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
y <- rchisq(45, df = 2)
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

x <- sort(qchisq(female.chi, df = 2))
y <- rchisq(length(x), df = 2)
qqplot(y, x, pch = 18, 
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 2]))
abline(0,1, col = "blue")
for(i in 1:50) { 
    y <- rchisq(length(x), df = 2)
    points(y, x, col = "pink")
}

