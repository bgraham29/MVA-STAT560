################################################################################

setwd("C:\\Users\\Mom\\Desktop\\Multivar")

data(Boston)
names(Boston)
dim(Boston)
data <- Boston[,c(1,5,6,7,8,9,10,11,12,13,14)]
#  names(data)

data$id <- 1:nrow(data)
data$col <- 1
data$col[data$medv > mean(data$medv)] <- 2


data.scale <- data

transform <- function(x){(x-min(x))/(max(x)-min(x))}


for(i in 1:11) {
    data.scale[, i] <- transform(data.scale[, i])
}

scale.long <- melt(data.scale, c("id", "col"), variable.name = "var", 
                value.name = "level")


ggplot(boston.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw() + xlab("Variable") + 
    ggtitle("Original Data")

#### k rank approximation of data (k = 1, 2, 3)
data.svd <- svd(data[,1:11])
U <- data.svd$u
D <- diag(data.svd$d)
V <- data.svd$v

X1 <- D[1] * U[,1] %*% t(V[, 1])
X1 <- data.frame(X1)
X1[,12:13] <- data[, 12:13]
colnames(X1) <- colnames(data)
X1.long <- melt(X1, c("id", "col"), variable.name = "var", 
                value.name = "level")

ggplot(X1.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw()+ xlab("Variable") + 
    ggtitle("Rank 1 Approximation")

E1 <- data[, 1:11] - X1[, 1:11]
E1[, 12:13] <- data[, 12:13]
colnames(E1) <- colnames(data)
E1.long <-  melt(E1, c("id", "col"), variable.name = "var", 
                value.name = "level")

ggplot(E1.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw()+ xlab("Variable") + 
    ggtitle("Errors for Rank 1 Approximation")



X2 <- U[,1:2] %*%  D[1:2, 1:2] %*% t(V[, 1:2])
X2 <- data.frame(X2)
X2[,12:13] <- data[, 12:13]
colnames(X2) <- colnames(data)
X2.long <- melt(X2, c("id", "col"), variable.name = "var", 
                value.name = "level")

ggplot(X2.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw()+ xlab("Variable") + 
    ggtitle("Rank 2 Approximation")


E2 <- data[, 1:11] - X2[, 1:11]
E2[, 12:13] <- data[, 12:13]
colnames(E2) <- colnames(data)
E2.long <-  melt(E2, c("id", "col"), variable.name = "var", 
                value.name = "level")

ggplot(E2.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw()+ xlab("Variable") + 
    ggtitle("Errors for Rank 2 Approximation")


X3 <- U[,1:3] %*%  D[1:3, 1:3] %*% t(V[, 1:3])
X3 <- data.frame(X3)
X3[,12:13] <- data[, 12:13]
colnames(X3) <- colnames(data)
X3.long <- melt(X3, c("id", "col"), variable.name = "var", 
                value.name = "level")

ggplot(X3.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw()+ xlab("Variable") + 
    ggtitle("Rank 3 Approximation")


E3 <- data[, 1:11] - X3[, 1:11]
E3[, 12:13] <- data[, 12:13]
colnames(E3) <- colnames(data)
E3.long <-  melt(E3, c("id", "col"), variable.name = "var", 
                value.name = "level")

ggplot(E3.long, aes(x=var, y = level, color = factor(col))) +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("red", "forestgreen"), 
        labels = c("below mean price", "above mean price")) +
    theme_bw()+ xlab("Variable") + 
    ggtitle("Errors for Rank 3 Approximation")


boston.pca <- princomp(data[, 1:11], cor = TRUE)
PC <- data.frame(boston.pca$scores)
PC$col <- data$col
pairs(PC[,1:3])

means <- apply(data[, 1:11], 2, mean)
sds <- apply(data[, 1:11], 2, sd)

boston.scaled <- data
for(i in 1:11) {
   boston.scaled[, i] <- (data[, i] - means[i]) / sds[i]
}

pca2 <- princomp(boston.scaled[, 1:11])
PC2 <- data.frame(pca2$scores)
pairs(PC2[, 1:3])

