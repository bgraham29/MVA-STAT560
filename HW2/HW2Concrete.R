dt################################################################################

concrete <- read.csv("C:\\Users\\Mom\\Desktop\\MultiVar\\Concrete.csv", 
    header = T, skip = 1)

names(concrete)

concreteData <- concrete[, -9]

conPCA <- princomp(concreteData, cor = TRUE)

conPCA$loadings[,2]

conPCA$loadings[,8]

round(cor(concreteData),2)


conMeans <- apply(concreteData, 2, mean)
conSDS <- apply(concreteData, 2, sd)

conScale <- concreteData
for(i in 1:ncol(concreteData)) {
   conScale[, i] <- (concreteData[, i] - conMeans[i]) / conSDS[i]
}

con2 <- princomp(conScale)
con2$loadings[, 2]
con2$loadings[, 8]

cor(conScale)

PCs <- data.frame(conPCA$scores)

x <- lm(Strength ~ Cement + Slag + Ash + Water + Plasticizer + Coarse + Fine +
    Age, data = concrete)

y <- lm(Y ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5, data = PCs)
