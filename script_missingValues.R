data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)
summary(data)

#Missing data visualisation 
library(naniar)
jpeg(file = "missingData.jpg")
vis_miss(data)
dev.off()

#Detecting missingness mechanism
library(ggplot2)
library(naniar)

#Variable WHR2
ggplot(data, aes(x=WHR2, y=Age)) + geom_miss_point()
M <- as.numeric(is.na(WHR2))
boxplot(Age ~ M)
t.test(Age[M==1],Age[M==0], var.equal=FALSE)

ggplot(data, aes(x=WHR2, y=Wt)) + geom_miss_point()
M <- as.numeric(is.na(WHR2))
boxplot(Wt ~ M)
t.test(Wt[M==1],Wt[M==0], var.equal=FALSE)

#Variables Fat and Fat2 
ggplot(data, aes(x=Fat, y=Age)) + geom_miss_point()
M <- as.numeric(is.na(Fat))
boxplot(Age ~ M)
t.test(Age[M==1],Age[M==0], var.equal=FALSE)

ggplot(data, aes(x=Fat, y=Wt)) + geom_miss_point()
M <- as.numeric(is.na(Fat))
boxplot(Wt ~ M)
t.test(Wt[M==1],Wt[M==0], var.equal=FALSE)

ggplot(data, aes(x=Fat2, y=Age)) + geom_miss_point()
M <- as.numeric(is.na(Fat2))
boxplot(Age ~ M)
t.test(Age[M==1],Age[M==0], var.equal=FALSE)

ggplot(data, aes(x=Fat2, y=Wt)) + geom_miss_point()
M <- as.numeric(is.na(Fat2))
boxplot(Wt ~ M)
t.test(Wt[M==1],Wt[M==0], var.equal=FALSE)

#Variables Syst and Diast 
ggplot(data, aes(x=Syst, y=Age)) + geom_miss_point()
MSyst <- as.numeric(is.na(Syst))
boxplot(Age ~ MSyst, xlab="Missing values Syst")
t.test(Age[MSyst==1],Age[MSyst==0], var.equal=FALSE)

ggplot(data, aes(x=Diast, y=Age)) + geom_miss_point()
MDiast <- as.numeric(is.na(Diast))
boxplot(Age ~ MDiast, xlab="Missing values Diast")
t.test(Age[MDiast==1],Age[MDiast==0], var.equal=FALSE)

ggplot(data, aes(x=Syst2, y=Age)) + geom_miss_point()
MSyst2 <- as.numeric(is.na(Syst2))
boxplot(Age ~ MSyst2, xlab="Missing values Syst2")
t.test(Age[MSyst2==1],Age[MSyst2==0], var.equal=FALSE)

ggplot(data, aes(x=Diast2, y=Age)) + geom_miss_point()
MDiast2 <- as.numeric(is.na(Diast2))
boxplot(Age ~ MDiast2, xlab="Missing values Diast2")
t.test(Age[MDiast2==1],Age[MDiast2==0], var.equal=FALSE)