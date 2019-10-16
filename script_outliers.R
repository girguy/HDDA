library(naniar)
library(ggplot2)
library(naniar)

data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)

# Boxplot of attributes that contains no significant outliers
png(file = "boxPlotWt.png")
bplot <- boxplot(Wt, Wt2, main="Pourcentage of body weigth")
abline(h=2, col='red')
dev.off()

png(file = "boxPlotBmi.png")
boxplot(BMI, BMI2, main="Body mass index")
abline(h=2, col='red')
dev.off()

png(file = "boxPlotFat.png")
boxplot(Fat, Fat2, main="Pourcentage of body fat")
abline(h=2, col='red')
dev.off()

png(file = "boxPlotWhr.png")
boxplot(WHR, WHR2, main="Waist-to-hip ratio")
abline(h=2, col='red')
dev.off()

png(file = "boxPlotSyst.png")
boxplot(Syst, Syst2, main="Systolic blood pressure")
abline(h=2, col='red')
dev.off()

png(file = "boxPlotDiast.png")
boxplot(Diast, Diast2, main="Diastolic blood pressure")
abline(h=2, col='red')
dev.off()

# Syst attribute for oultlier analysis
# z-Score
zSyst <- scale(Syst)
# Graph of the z-Score
png(file = "zScoreSyst.png")
plot(zSyst, type='h', main="z-Score fot systolic blood pressure")
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Syst)


# Syst2 attribute for oultlier analysis
# z-Score
zSyst2 <- scale(Syst2)
# Graph of the z-Score
png(file = "zScoreSyst2.png")
plot(zSyst2, type='h', main="z-Score for systolic(2) blood pressure")
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Syst2)


# Diast attribute for oultlier analysis
# z-Score
zDiast <- scale(Diast)
# Graph of the z-Score
png(file = "zScoreDiast.png",)
plot(zDiast, type='h', main="z-Score for diastolic blood pressure")
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Diast)


# Diast2 attribute for oultlier analysis
# Boxplot
boxplot(Diast2) # OUTLIERS
# z-Score
zDiast2 <- scale(Diast2)
# Graph of the z-Score
png(file = "zScoreDiast2.png")
plot(zDiast2, type='h', main="z-Score for diastolic(2) blood pressure")
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Diast2)

# Delete rows with NA
data <- na.omit(data)
n.outliers   <- 4 # Mark as outliers the 4 most extreme points

# Mahalanobis Distance analysis
data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)

x <- subset(data, select = -c(Informed, Fat,Fat2, Fat.Ft2,BMI.BM2,Dst.Ds2))
# Not considering the variable Fat and Fat2 
xx <- na.omit(x)

m<-colMeans(x,na.rm=TRUE)
S<-var(x,na.rm=TRUE)
d<-mahalanobis(x,m,S)
png(file = "mahaDistance.png")
plot(d,type="h", main = "Mahalanobis distance")
abline(h=qchisq(0.95,11), col='red')
dev.off()