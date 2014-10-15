library(MASS);
mu<- c(1,2);
Sigma<- matrix(c(1, 0.5, 0.5, 2), ncol=2);
n<- 100;
set.seed(123);
x<- mvrnorm(n, mu, Sigma);
xbar<- apply(x, 2, mean);
Smat<- cov(x)*(n-1)/n;

## assuming Sigma known

test.stat1<- n*(2*xbar[1]-xbar[2]-0.2)^2/( t(c(2, -1))%*%Sigma%*%c(2,-1));
pvalue1<- 1-pchisq(test.stat1, 1);

#> c(test.stat1, pvalue1);
#[1] 4.72817185 0.02967241


## assuming Sigma unknown;

test.stat2<- (n-1)*(2*xbar[1]-xbar[2]-0.2)^2/( t(c(2, -1))%*%Smat%*%c(2,-1));
pvalue2<- 1- pf(test.stat2, 1, (n-1));

#> c(test.stat2, pvalue2);
#[1] 5.00931652 0.02745153


## Ex 7.14

rho.a<- function(rho, a){5*(3*a^2-2*rho*a+1)/(3-rho^2)};
rho.vec<- seq(-sqrt(3), sqrt(3), length.out=40);
a00<- rho.a(rho.vec, 0);
plot(rho.vec, a00, type="l", col=2, xlim=c(-2, 2), ylim=c(0, 100), xlab="rho", ylab="test stat");
lines(rho.vec, rho.a(rho.vec, 0.2), col=3);
lines(rho.vec, rho.a(rho.vec, 0.4), col=4);
lines(rho.vec, rho.a(rho.vec, 0.6), col=5);
lines(rho.vec, rho.a(rho.vec, 0.8), col=6);

plot(rho.vec, a00, type="l", col=2, xlim=c(-2, 2), ylim=c(0, 100), xlab="rho", ylab="test stat");
lines(rho.vec, rho.a(rho.vec, 0.5), col=3);
lines(rho.vec, rho.a(rho.vec, 1), col=4);
abline(h=5.9915);
