library(MASS)
set.seed(1)
##reading the data
admission <- read.table("admission.DAT",header=FALSE)
names(admission) <- c('GPA', 'GMAT', 'class')

X <- admission[,1:2]
n1 <- sum(admission$class==1)
n2 <- sum(admission$class==2)
n3 <- sum(admission$class==3)

plot(X,pch=c(rep(1,n1),rep(2,n2), rep(3,n3)),col=c(rep("blue",n1),rep("red",n2), rep("green",n3)))

train<- sample(1:85,42)
table(admission$class[train])

# LDA
z<-lda(class ~., admission, subset = train)
#compute training misclassification rate
predtraincls<- predict(z, admission[train,])$class
sum(predtraincls != admission$class[train])/length(predtraincls)

#predictions for test data
predtestcls<- predict(z, admission[-train,])$class
#true labels
truecls <- admission$class[-train]
#compute testing misclassification rate
sum(predtestcls != truecls)/length(predtestcls)

#plot below is to just give you an idea how the decision boundary looks in 2-d plane.

x<-seq(2, 4, by = .01)
y<-seq(300, 700, by = 5)
GPA<-rep(x,length(y))
GMAT<-rep(y, each = length(x))  
testgrid <- data.frame(GPA,GMAT)
predtraincls<- predict(z, testgrid)$class


plot(testgrid[predtraincls==1,],pch=1, col = "blue")
points(testgrid[predtraincls==2,],pch=2, col = "red")
points(testgrid[predtraincls==3,],pch=3, col = "green")




# QDA
z<-qda(class ~., admission,  subset = train)
#compute training misclassification rate
predtraincls<- predict(z, admission[train,])$class
sum(predtraincls != admission$class[train])/length(predtraincls)

#predictions for test data
predtestcls<- predict(z, admission[-train,])$class
#true labels
truecls <- admission$class[-train]
#compute testing misclassification rate
sum(predtestcls != truecls)/length(predtestcls)


#plot below is to just give you an idea how the decision boundary looks in 2-d plane.

x<-seq(2, 4, by = .01)
y<-seq(300, 700, by = 5)
GPA<-rep(x,length(y))
GMAT<-rep(y, each = length(x))  
testgrid <- data.frame(GPA,GMAT)
predtraincls<- predict(z, testgrid)$class


plot(testgrid[predtraincls==1,],pch=1, col = "blue")
points(testgrid[predtraincls==2,],pch=2, col = "red")
points(testgrid[predtraincls==3,],pch=3, col = "green")



