round(cov(BankNotes[,1:6]),2)
round(cov(BankNotes[BankNotes$notes == "Counterfeit",1:6]),3)
round(cov(BankNotes[BankNotes$notes == "Genuine",1:6]),3)

ggplot(BankNotes, aes(x=low, y = upp)) + geom_point(aes(color = notes)) +
  geom_smooth(method="lm")

sales <- data.frame(sold = c(230,181,165,150,97,192,181,189,172,170), 
              price = c(125,99,97,115,120,100,80,90,95,125), 
              advert = c(200,55,105,85,0,150,85,120,110,130), 
              hrs = c(109,107,98,71,82,103,111,93,86,78))
ggplot(sales, aes(x=price, y = sold)) + geom_point() + geom_smooth(method= "lm")
round(cov(sales),2)
apply(sales,2,mean)

((sales[,1] - 172.7) %*% (sales[,2] - 104.6))/9


###### 3.2 Correlation ==> remember, it only meausres linear dependence
round(cor(BankNotes[BankNotes$notes == "Genuine",1:6]),2)
round(cor(BankNotes[BankNotes$notes == "Counterfeit",1:6]),2)

ggplot(cars, aes(x=mpg, y =wt)) + geom_point(aes(color=factor(co), shape = factor(co)))
rx2x8 <- cor(cars$mpg, cars$wt)
W <- 1/2 * log((1 + rx2x8)/(1 - rx2x8)) 
zstar <- (W-0)/sqrt(1/(dim(cars)[1]-3))	# significantly different from zero
pnorm(zstar)
Wnull <- 1/2 * log((1 + (-.75))/(1 - (-.75)))
(W-Wnull)/sqrt(1/(dim(cars)[1]-3))		# not significantly different from -0.75

round(cor(sales),3)
rx1x4 <- 0.633
W <- 1/2*log((1+rx1x4)/(1-rx1x4)) 	# Fisher's z-transform {W <- atanh(rx1x4)}
Wstar <- W - (3*W + tanh(W))/(4*(dim(sales)[1] - 1))	# Hotelling's transform for n <= 25
z <- Wstar/sqrt(1/(dim(sales)[1]-1))
2*pnorm(-abs(z))

# test for independence? under assumption of normality:
t <- rx1x4*sqrt((dim(sales)[1]-2)/(1 - rx1x4^2))
2*pt(-abs(t), df = dim(sales)[1]-2)	# reject null: rho != 0

### Summary Statistics
xbar <- as.matrix(apply(sales,2,mean))
n <- dim(sales)[1]
n^(-1) * t(sales) %*% rep(1,n)

cov(sales)
Sx <- round((n-1)^(-1) *( t(as.matrix(sales))%*%as.matrix(sales) - n^(-1)*t(as.matrix(sales))%*%rep(1,n)%*%t(rep(1,n))%*%as.matrix(sales)),1)

A <- matrix(c(0,0,1,10), nrow=1)
A %*% xbar
A%*% Sx %*% t(A)

## 3.4 Linear Model for Two Variables

summary(lm(sold ~ price, data= sales))
summary(lm(upp ~ low, data= subset(BankNotes, notes == "Genuine")))
ggplot(subset(BankNotes, notes=="Genuine"), aes(x=low, y = upp)) + geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red")
summary(lm(low ~ upp, data = subset(BankNotes, notes == "Genuine")))

## 3.5 Simple ANOVA

sales.mktg <- data.frame(shop = 1:10, 
              news = c(9,11,10,12,7,11,12,10,11,13), 
              asst = c(10,15,11,15,15,13,7,15,13,10),
              lux = c(18,14,17,9,14,17,16,14,17,15))
mktg.long <- melt(sales.mktg, id.vars = "shop", variable.name = "Strategy")
anova(lm(value ~ Strategy, data = mktg.long))

## 3.6 Multiple linear model

sales.sum <- summary(lm(sold ~ price +advert + hrs, data=sales))
p <- length(sales.sum$coef[,1])-1
r2 <- sales.sum$r.squared
n <- dim(sales)[1]
r2 - p * (1-r2)/(n - (p + 1)) # adjusted r-squared
0.907 - 3*(1-.907^2)/(10-3-1) # from book: rounding errors

X <- matrix(c(rep(1,10), rep(0,30), rep(1,10), rep(0,30), rep(1,10)), ncol=3)
solve(t(X) %*% X)	# I3
t(X) %*% unlist(sales.mktg[,-1])
beta.hat1 <-solve(t(X)%*% X) %*% t(X) %*% unlist(sales.mktg[,-1]) # (xt x) inverse xt y

a <- matrix(c(0,0))
A <- matrix(c(-1,1,0,-1,0,1), nrow = 2, byrow= T)	# contrast -mu1 + mu2 & -mu1 + mu3
beta.hat1 - solve(t(X)%*% X)%*% t(A) %*% solve(A %*% solve(t(X)%*% X) %*% t(A))%*% (A %*% beta.hat1 - a)
# returns overall mean

# Boston housing 
#1) summary statistics
round(apply(Boston, 2, mean),2)
round(apply(Boston, 2, median),2)
round(apply(Boston, 2, var),2)
round(apply(Boston, 2, sd),2)
round(cov(Boston),2)
round(cor(Boston),2)
rev(sort(abs(round(cor(Boston),2)[-14,14])))
round(sort(2*pnorm(-abs(atanh(cor(Boston)[-14,14])/(1/sqrt(dim(Boston)[1] -3))))),6)

#2) summary for transformed data
round(apply(bostonT, 2, mean),2)
round(apply(bostonT, 2, median),2)
round(apply(bostonT, 2, var),2)
round(apply(bostonT, 2, sd),2)
round(cov(bostonT),2)
round(cor(bostonT),2)
rev(sort(abs(round(cor(bostonT),2)[-14,14])))
round(sort(2*pnorm(-abs(atanh(cor(bostonT)[-14,14])/(1/sqrt(dim(bostonT)[1] -3))))),6)

summary(lm(valueT ~ ., data=bostonT))

## Exercises
# 3.11
epsilon <- resid(lm(sold ~ price +advert + hrs, data=sales))
boxplot(epsilon)
par(mfrow=c(2,2)); plot(lm(sold ~ price +advert + hrs, data=sales))
summary(lm(sold ~ price + advert + hrs, data = sales[-2,]))
