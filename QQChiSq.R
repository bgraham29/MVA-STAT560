#Chi-square probability plot
# calculate distance values (squared distances of data points from mu)
qqChiSq <- function(x1, pch = 1){
  u <- NULL
  mu <- apply(x1,2,mean)
  Sigma <- cov(x1)
  N <- nrow(x1)
  for(i in 1:N){
    u[i] <- as.matrix(x1[i,] - mu)%*%solve(as.matrix(Sigma))%*%t(as.matrix(x1[i,] - mu))
  }
  Ranks <- sort(u)
  p <- ncol(x1)
  Probs <- qchisq((1:N - .5)/N,p)
  pict <- plot(Probs, Ranks, main = expression(paste("Q-Q Plot for ", chi[(2)]^2,
     " distribution")), pch = pch, xlab = "Theoretical", ylab = "Observed")
  abline(0,1) # order of plot in book
  pict
}
