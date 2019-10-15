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

# Syst
hw <- data.frame(data$Age, data$Syst)
#m.dist.order contains ordered indices
m.dist.order <- order(mahalanobis(hw, colMeans(hw), cov(hw)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(hw))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
png(file = "mahaSyst.png")
plot(hw, pch=pch, main="Mahalanobis distance for systolic blood pressure") 
abline(h=2, col='red')
dev.off()

# Syst2
hw <- data.frame(data$Age, data$Syst2)
#m.dist.order contains ordered indices
m.dist.order <- order(mahalanobis(hw, colMeans(hw), cov(hw)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(hw))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
png(file = "mahaSyst2.png")
plot(hw, pch=pch, main="Mahalanobis distance for systolic(2) blood pressure") 
abline(h=2, col='red')
dev.off()

# Diast
hw <- data.frame(data$Age, data$Diast)
#m.dist.order contains ordered indices
m.dist.order <- order(mahalanobis(hw, colMeans(hw), cov(hw)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(hw))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
png(file = "mahaDiast.png")
plot(hw, pch=pch, main="Mahalanobis distance for diastolic blood pressure") 
abline(h=2, col='red')
dev.off()

# Diast2
hw <- data.frame(data$Age, data$Diast2)
#m.dist.order contains ordered indices
m.dist.order <- order(mahalanobis(hw, colMeans(hw), cov(hw)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(hw))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
png(file = "mahaDiast2.png")
plot(hw, pch=pch, main="Mahalanobis distance for diastolic(2) blood pressure") 
abline(h=2, col='red')
dev.off()





