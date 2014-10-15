X <- matrix(c(2,2,1,3,4,3,4,5), ncol=2)
apply(X,2,mean)
S <- cov(x)
#library(Matrix)
rankMatrix(X)
eigen(S)

v1 <- eigen(S)$vectors[,1]
Lv1 <- sqrt(sum(v1^2)) # = 1

for(i in 1:4)T[i,] <- (X[i,] %*% v1)*v1
T <- X %*% v1 %*% v1	# alternative to achieve same as above

Boston1 <- Boston[,c(1,5,6,9,14)]
apply(Boston1, 2, mean)
> apply(Boston1, 2, mean)
      crim        nox         rm        rad       medv 
 3.6135236  0.5546951  6.2846344  9.5494071 22.5328063 

round(cov(Boston1),2)
round(cor(Boston1),2)


> round(cov(Boston1),2)
       crim   nox    rm    rad   medv
crim  73.99  0.42 -1.33  46.85 -30.72
nox    0.42  0.01 -0.02   0.62  -0.46
rm    -1.33 -0.02  0.49  -1.28   4.49
rad   46.85  0.62 -1.28  75.82 -30.56
medv -30.72 -0.46  4.49 -30.56  84.59
> round(cor(Boston1),2)
      crim   nox    rm   rad  medv
crim  1.00  0.42 -0.22  0.63 -0.39
nox   0.42  1.00 -0.30  0.61 -0.43
rm   -0.22 -0.30  1.00 -0.21  0.70
rad   0.63  0.61 -0.21  1.00 -0.38
medv -0.39 -0.43  0.70 -0.38  1.00

min(diag(cov(Boston1))) #  0.01342764 = nox
# Max = 0.70 rm X medv

Boston1$id <- 1:506

Boston1.long <- melt(Boston1, id.vars="id", variable.name = "variable", value.name = "value")
Boston1.scale <- ddply(Boston1.long, c("variable"), summarise, scale = (value-min(value))/(max(value) - min(value)))
Boston1.scale$id <- Boston1.long$id
ggplot(Boston1.long, aes(x=variable, y=value)) + 
   geom_line(aes(group = id)) + ggtitle("Parallel plot with non-scaled variables")
ggplot(Boston1.scale, aes(x=variable, y=scale)) + 
   geom_line(aes(group = id)) + ggtitle("Parallel plot with scaled variables") +
   ylab("")

scatterplotMatrix(Boston1[,-6])	# library(car)

corrplot(round(cor(Boston1[,-6]),2), method = "circle", type = "lower", tl.col = "black")
corrplot(round(cor(Boston1[,-6]),2), method = "number", type = "upper", 
   add = TRUE, tl.pos = "t", cl.pos = "n", tl.col = "black")





