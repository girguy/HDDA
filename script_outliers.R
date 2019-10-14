library(naniar)
library(ggplot2)
library(naniar)

data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)

# Boxplot of attributes that contains no significant outliers
boxplot(Wt)
boxplot(Wt2)
boxplot(BMI)
boxplot(BMI2)
boxplot(Fat)
boxplot(Fat2)
boxplot(WHR) 
boxplot(WHR2) 


# Syst attribute for oultlier analysis
# Boxplot
boxplot(Syst)
# z-Score
zSyst <- scale(Syst)
# Graph of the z-Score
png(file = "zScoreSyst.png")
plot(zSyst, type='h')
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Syst)


# Syst2 attribute for oultlier analysis
# Boxplot
boxplot(Syst2) # OUTLIERS
# z-Score
zSyst2 <- scale(Syst2)
# Graph of the z-Score
png(file = "zScoreSyst2.png")
plot(zSyst2, type='h')
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Syst2)


# Diast attribute for oultlier analysis
# Boxplot
boxplot(Diast) # OUTLIERS
# z-Score
zDiast <- scale(Diast)
# Graph of the z-Score
png(file = "zScoreDiast.png")
plot(zDiast, type='h')
abline(h=2, col='red')
dev.off()
# Histogram 
hist(Diast)


# Diast2 attribute for oultlier analysis
# Boxplot
boxplot(Diast2) # OUTLIERS
# z-Score
zDiast2 <- scale(zDiast2)
# Graph of the z-Score
png(file = "zScoreDiast2.png")
plot(zDiast2, type='h')
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
plot(hw, pch=pch) 
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
plot(hw, pch=pch) 
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
plot(hw, pch=pch) 
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
plot(hw, pch=pch) 
abline(h=2, col='red')
dev.off()





