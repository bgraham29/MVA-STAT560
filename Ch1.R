setwd("C:\\Users\\bgandre\\Desktop\\Stats Stuff\\Multivariate-ClassLee")

play <- read.csv("C:\\Users\\bgandre\\Desktop\\Stats Stuff\\Multivariate-ClassLee\\PlayData.csv", header=TRUE)
play$id <- as.character(play$id)

play2 <- data.frame(t(play))
names(play2) <- as.character(play[,1])
play2 <- play2[-1,]
for(i in names(play2)){  play2[,i] <- as.numeric(as.character(play2[,i]))}
play2$Virus <- rep(c("Dengue","UV","Mock"), each = 20)
play2$Time <- rep(rep(c("6hr","18hr","30hr","48hr"), each = 5),3)
play2$Time <- factor(play2$Time, levels = c("6hr","18hr","30hr","48hr"))
play2 <- play2[,c(1373,1374,1:1372)]

for(i in 3:(dim(play2)[2])){
  hist(play2[,i], main = paste("Histogram of", names(play2)[i]))
  readline("Pause")
}

play.long <- melt(play, id.vars=c("id"), variable.name = "Group", value.name  = "intensity")
play.long$id <- as.character(play.long$id)
play.long$Virus <- rep(c("Dengue","UV","Mock"), each = 27440)
play.long$Time <- rep(rep(c("6hr","18hr","30hr","48hr"), each = 6860),3)
play.long$Time <- factor(play.long$Time, levels = c("6hr","18hr","30hr","48hr"))

BankNotes <- read.table("C:\\Users\\bgandre\\Desktop\\Stats Stuff\\Multivariate-ClassLee\\DataMaybe\\bank2.dat")
names(BankNotes) <- c("len","htR","htL","low","upp","diag")
BankNotes$notes <- rep(c("Genuine","Counterfeit"))
BankNotes$notes <- factor(BankNotes$notes, levels = c("Genuine","Counterfeit"))

library(aplpack)
boxplot(BankNotes)


cities <- data.frame(City = c("Tokyo","Mexico City","Seoul","New York","Sao Paulo","Bombay","Delhi","Shanghai",
   "Los Angeles","Osaka","Jakarta","Calcutta","Cairo","Manila","Karachi"), Pop = c(3420,2280,2230,2190,2020,
   1985,1970,1815,1800,1680,1655,1565,1560,1495,1430))

boxplot(cities$Pop)
quantile(cities$Pop,.75)
quantile(cities$Pop,.25)
median(cities$Pop)
max(cities$Pop)
min(cities$Pop)

ggplot(cities, aes(x=1, y=Pop)) + geom_boxplot() + geom_point()

cars <- data.frame(Ctry = rep(c("US","Japan","EU"),each = 5),
   mpg = c(12,16.8,18.8,22,30,18,22,25,30.5,35,14,19,23,25,28)) 
cars$Ctry <- factor(cars$Ctry, levels = c("US","Japan","EU"))
ggplot(cars, aes(x=Ctry, y=mpg)) + geom_boxplot()


boxplot(BankNotes$diag ~ BankNotes$notes)
boxplot(BankNotes$len ~ BankNotes$notes)
faces(BankNotes[c(50:55,150:155),-7], fill = TRUE)

ggplot(BankNotes, aes(x=diag, fill = notes)) + geom_density(alpha = .5)
ggplot(BankNotes, aes(x=diag)) + geom_histogram(binwidth = .2, position = "identity")
ggplot(subset(BankNotes, notes == "Genuine"), aes(x=diag)) + geom_histogram(binwidth = .6, position = "identity")

#play.long
ggplot(subset(play.long, id == "11073"), aes(x=intensity, fill = Virus)) + 
  geom_density(alpha = .2) + facet_grid(.~Time)

ggplot(BankNotes, aes(x=low, y = diag)) + geom_point(aes(color=notes, size = len), shape = 1)
plot3d(BankNotes$low, BankNotes$diag, BankNotes$upp,col.var = as.integer(BankNotes$notes), #library(car)?
   col=rep(c("blue","red"), each=100), type = "s")

scatterplotMatrix(BankNotes[,1:6])
faces(BankNotes[1:50,-7], fill = TRUE, nrow = 7)

ggplot(BankNotes, aes(x=htL, y = low)) + geom_point(aes(color=notes), size = 5, show_guide = FALSE) + theme_bw()
ggplot(BankNotes, aes(x=htL, y= low)) + stat_density2d()
ggpairs(BankNotes[,1:6])

library(andrews)
andrews(BankNotes[96:105,1:6])	# use PCA-suggested order (Chapter 10); use max of 20 variables
andrews(BankNotes[96:105,c(6,5,4,3,2,1)], xlim = c(0,1))

BankNotes$id <- 1:200
BN.long <- melt(BankNotes[,1:8], id.vars=c("id","notes"), variable.name = "measure", value.name = "mm")
BNsmall <- subset(BankNotes, id %in% 96:105)
BN.scale <- melt(BNsmall[,7:14], id.vars=c("id","notes"), variable.name = "measure", value.name = "scale")
ggplot(BN.scale, aes(x=measure, y = scale, color = notes)) + geom_line(aes(group = id)) +
  scale_color_manual(values = c("black","red"))
BNsmall$ScaleLen <- with(BNsmall, (len - min(len))/(max(len) - min(len)))
BNsmall$ScaleHtR <- with(BNsmall, (htR - min(htR))/(max(htR) - min(htR)))
BNsmall$ScaleHtL <- with(BNsmall, (htL - min(htL))/(max(htL) - min(htL)))
BNsmall$ScaleLow <- with(BNsmall, (low  - min(low))/(max(low) - min(low)))
BNsmall$ScaleUpp <- with(BNsmall, (upp - min(upp))/(max(upp) - min(upp)))
BNsmall$ScaleDiag <- with(BNsmall, (diag - min(diag))/(max(diag) - min(diag)))


cars <- read.table("C:\\Users\\bgandre\\Desktop\\Stats Stuff\\Multivariate-ClassLee\\DataMaybe\\carc.dat")
names(cars) <- c("Model","price","mpg","r78","r77","headroom","rear","trunk","wt","length","diam","displ","gear","co")
str(cars)
cars$r78[cars$r78 == "."] <- NA; cars$r78 <- as.numeric(cars$r78)
cars$r78[cars$r77 == "."] <- NA; cars$r77 <- as.numeric(cars$r77)



cars.long <- melt(cars, c("Model","co"), variable.name = "measure", value.name = "amt")
car.scale <- ddply(cars.long, c("measure"), summarise, scale = (amt - min(amt,na.rm=T))/(max(amt, na.rm=T) - min(amt,na.rm=T)))
cars.long$scale <- car.scale$scale
ggplot(subset(cars.long, measure == "wt" | measure == "displ"), aes(x=measure, y=scale, color = factor(co))) +
  geom_line(aes(group = Model))
ggplot(subset(cars.long, measure == "wt" | measure == "mpg"), aes(x=measure, y=scale, color = factor(co))) +
  geom_line(aes(group = Model))
ggplot(subset(cars.long, measure == "displ" | measure == "gear"), aes(x=measure, y=scale, color = factor(co))) +
  geom_line(aes(group = Model)) # include "co" to see convergence to subgroups
ggplot(subset(cars.long, measure == "headroom" | measure == "rear" | measure == "trunk"), aes(x=measure, y=scale, color = factor(co))) +
  geom_line(aes(group = Model)) # look for outliers ==> confirm with boxplots
ggplot(subset(cars.long, measure == "headroom" | measure == "rear" | measure == "trunk"), aes(x=measure, y=amt)) +
  geom_boxplot() # look for outliers ==> confirm with boxplots
ggplot(cars.long, aes(x=measure, y=scale, color = factor(co))) +
  geom_line(aes(group = Model)) + scale_color_manual(values = c("red","black","green"), labels = c("Europe","US","Japan")) + theme_bw() 

## HEXAGON PLOTS
library(hexbin)

boston <- read.table("C:\\Users\\bgandre\\Desktop\\Stats Stuff\\Multivariate-ClassLee\\DataMaybe\\bostonh.dat")
names(boston) <- c("crime","perLarge","perNonretail","river","NO","rooms","owner","employment","highway","taxRate","teacher","black","status","value")

bostonScale <- matrix(apply(boston, 2, function(x) (x - min(x))/(max(x) - min(x))), nrow = 506)
bostonScale <- data.frame(bostonScale)
names(bostonScale) <- names(boston)
bostonScale$cleanAir <- ifelse(boston$NO <= median(boston$NO), "Low","High")
boston$cleanAir <- ifelse(boston$NO <= median(boston$NO), "low", "High")
bostonScale$houseValue <- ifelse(boston$value <= median(boston$value), "Low","High")
boston$houseValue <- ifelse(boston$value <= median(boston$value), "Low", "High"); 
boston$houseValue <- factor(boston$houseValue, levels = c("Low","High"))
bostonScale$id <- 1:506
bostonScale.long <- melt(bostonScale, c("id", "cleanAir"), variable.name = "V", value.name = "scale")
ggplot(bostonScale.long, aes(x=V, y= scale, color = cleanAir)) + geom_line(aes(group = id)) +
  scale_color_manual(values = c("red","black"), labels = c("high NO", "low NO"))
pairs(boston[,c(1:5,14)])
scatterplotMatrix(boston[,c(1:5,14)], diagonal = "boxplot", groups = boston$cleanAir, by.groups = TRUE)

ggplot(boston, aes(x=log(crime), fill = value > median(value))) + geom_density(alpha = .5)
ggplot(boston, aes(x=log(crime), y = value)) + geom_point(aes(color = houseValue), size = 3)
ggplot(boston, aes(x=houseValue, y=crime)) + geom_boxplot()
ggplot(boston, aes(x=cleanAir, y=log(perLarge))) + geom_boxplot()

ggplot(boston, aes(x=crime, y=perLarge)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=perLarge, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=houseValue, y=perLarge)) + geom_boxplot()

ggplot(boston, aes(x=log(perNonretail), y=log(value))) + geom_point(aes(color=houseValue)) + 
   geom_smooth(method = "lm",se=FALSE)
ggplot(boston, aes(x=perNonretail, fill = houseValue)) + geom_density(alpha = .5)
ggplot(boston, aes(x=perLarge, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=houseValue, y=perLarge)) + geom_boxplot()


ggplot(boston, aes(x=river, y=value)) + geom_point(aes(color=houseValue))

ggplot(boston, aes(x=NO, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=houseValue, y=NO)) + geom_boxplot()

ggplot(boston, aes(x=rooms, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=houseValue, y=rooms)) + geom_boxplot()

ggplot(boston, aes(x=owner^2.5, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=houseValue, y=owner^2.5)) + geom_boxplot()

ggplot(boston, aes(x=owner, y=employment)) + geom_point(aes(color=houseValue))

ggplot(boston, aes(x=employment, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=houseValue, y=employment)) + geom_boxplot()

ggplot(boston, aes(x=highway, y=value)) + geom_point(aes(color=houseValue))
ggplot(boston, aes(x=highway)) + geom_density(alpha = 0.5)
ggplot(boston, aes(x=highway)) + geom_histogram(binwidth = 2)
ggplot(boston, aes(x=houseValue, y=highway)) + geom_boxplot()

ggplot(boston, aes(x=taxRate, y=value)) + geom_point(aes(color=houseValue)) + geom_smooth(method="lm")
ggplot(boston, aes(x=taxRate, fill = houseValue)) + geom_density(alpha = 0.5)
ggplot(boston, aes(x=taxRate)) + geom_histogram(binwidth = 50)
ggplot(boston, aes(x=houseValue, y=taxRate)) + geom_boxplot()

ggplot(boston, aes(x=houseValue, y = teacher)) + geom_boxplot()\
ggplot(boston, aes(x=value, y=teacher)) + geom_point(aes(color=houseValue))

pairs(boston[,c(12,3,7,11)])
pairs(boston[,c(12,3,7,11)], col = c("red","black")[unclass(boston$houseValue)], diag.panel = panel.hist, upper.panel = panel.cor)
ggplot(boston, aes(x=log(black), y=value)) + geom_point(aes(color=black > 3 & black < 100))

ggplot(boston, aes(x=sqrt(status), y = log(value))) + geom_point(aes(color = houseValue))

bostonT <- data.frame(crimeT = log(boston$crime), perLargeT = boston$perLarge/10,
            perNonretailT = log(boston$perNonretail), riverT = boston$river,
            NOT = log(boston$NO), roomsT = log(boston$rooms), ownerT = boston$owner^2.5/10000,
            employT = log(boston$employment), hwyT = log(boston$highway), 
            taxT = log(boston$taxRate), teachT = exp(0.4*boston$teacher)/1000,
            blackT = boston$black/100, statusT = sqrt(boston$status), valueT = log(boston$value))
boxplot(boston[,1:14])
boxplot(bostonT)

