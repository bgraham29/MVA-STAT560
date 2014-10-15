
## 10 US Cities. 
data <- read.table("C:/Teaching/STA6707/Data/popul.dat")
 x<- data[c(1)]
 f<- summary(x)
 f
boxplot(x, main="Boxplot", xlab = "U.S. cities")

#x <- cbind(x,rep(0,15)) 
# Type <- c(rep(0,15))
# fType <- factor(Type, levels=0:0)
#levels(fType) <- c("U.S. cities")
#x[,2]<- fType
#colnames(x)=c("Population", "City")
#boxplot(Population ~ City, data=x, main="Boxplot", xlab = "U.S. cities")

# Swiss bank notes. 
# the first half (100 obs) are genuine; the second half conterfeit. 
# x1 is the length of the bill;
# x6 is the length of the diagonal of the central picture. 

data<-read.table("C:/Teaching/STA6707/Data/bank2.dat")
x <- data[c(1,6)]
## x has two columns (x1 and x6 from the original data). 
 Type <- c(rep(0,100), rep(1,100))
 fType <- factor(Type, levels=0:1)
 levels(fType) <- c("GENUINE","CONTERFEIT")
x[,3]<- fType
colnames(x)<- c("Length", "Diagonal", "Type")

boxplot(Length ~ Type, data=x, at=1:2, main="Swiss bank notes");
boxplot(Diagonal ~ Type, data=x, at=1:2, main="Swiss bank notes");


## car mileages. (1983). 

data <- read.table("C:/Teaching/STA6707/Data/carc.dat")
x<- data[c(3,14)]
headquarter <- factor(x[,2])
levels(headquarter) <- c("US","JAPAN", "EU")
x <- cbind(x, headquarter)
colnames(x)<- c("Mileage", "HeadInt", "HeadFac")
boxplot(Mileage ~ HeadFac, data=x, at=1:3, main="car data")
