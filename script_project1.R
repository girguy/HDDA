# Import data
data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)


#--------------------------------------------------
# Question 2 : Missing data 
#--------------------------------------------------

# Missing data visualisation 
library(naniar)
vis_miss(data)

# Detecting missingness mechanism : 
# Visualisation : geom_miss_point and boxplot
# Statistical test : t-test
library(ggplot2)
library(naniar)

# Variable WHR2 w.r.t Age and Wt 
ggplot(data, aes(x=WHR2, y=Age)) + geom_miss_point()
M <- as.numeric(is.na(WHR2))
boxplot(Age ~ M)
t.test(Age[M==1],Age[M==0], var.equal=FALSE)

ggplot(data, aes(x=WHR2, y=Wt)) + geom_miss_point()
M <- as.numeric(is.na(WHR2))
boxplot(Wt ~ M)
t.test(Wt[M==1],Wt[M==0], var.equal=FALSE)
# -> No significant difference between means for both tests (no rejection of null hypothesis)

# Variables Fat and Fat2 w.r.t Age and Wt
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
# -> No significant difference between means for all tests (no rejection of null hypothesis)

# Variables Syst and Diast w.r.t Age 
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
# -> Significant difference between means (rejection of the null hypothesis for each test)

#--------------------------------------------------
# Question 3.1: Statistical and graphical summary 
#--------------------------------------------------
library(naniar)
library(ggplot2)

# Statistical summary 
summary(data)

summary(data[Informed == 0,-1])
summary(data[Informed == 1,-1])

# ---------------- Age----------------  
mean(Age[Informed==0])
mean(Age[Informed==1])
sd(Age[Informed==0],na.rm = T)
sd(Age[Informed==1],na.rm = T)
#graphs
par (mfrow =c(2, 2))
boxplot(Age~Informed)
hist(Age[Informed == 0], col = "red", xlab = "Age", main = "Informed = 0")
plot(ecdf(Age[Informed == 0]), main = "Distribution function: CDF", xlab = "Age", col = "red")
plot(ecdf(Age[Informed == 1]), main = "Distribution function: CDF", xlab = "Age", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
hist(Age[Informed == 1], col = "blue", xlab = "Age", main = "Informed = 1")

# ---------------- Wt-Wt2 ----------------  
mean(Wt[Informed==0])
mean(Wt[Informed==1])
sd(Wt[Informed==0],na.rm = T)
sd(Wt[Informed==1],na.rm = T)
mean(Wt2[Informed==0])
mean(Wt2[Informed==1])
sd(Wt2[Informed==0],na.rm = T)
sd(Wt2[Informed==1],na.rm = T)
#graphs
par (mfrow =c(2, 3))
boxplot(Wt~Informed)
hist(Wt[Informed == 0], col = "red", xlab = "Wt - Weight", main = "Informed = 0")
hist(Wt[Informed == 1], col = "blue", xlab = "Wt - Weight", main = "Informed = 1")
boxplot(Wt2~Informed)
hist(Wt2[Informed == 0], col = "red", xlab = "Wt2 - Weight 4 weeks after", main = "Informed = 0")
hist(Wt2[Informed == 1], col = "blue", xlab = "Wt2 - Weight 4 weeks after", main = "Informed = 1")
par (mfrow =c(1, 2))
plot(ecdf(Wt[Informed == 0]), main = "Distribution function: CDF", xlab = "Wt - Weight", col = "red")
plot(ecdf(Wt[Informed == 1]), main = "Distribution function: CDF", xlab = "Wt - Weight", col = "blue", add = T)
legend(x = "right", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(Wt2[Informed == 0]), main = "Distribution function: CDF", xlab = "Wt2 - Weight 4 weeks after", col = "red")
plot(ecdf(Wt2[Informed == 1]), main = "Distribution function: CDF", xlab = "Wt2 - Weight 4 weeks after", col = "blue", add = T)
legend(x = "right", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))


# ---------------- BMI,BMI2, BMI.BM2 ----------------
mean(BMI[Informed==0],na.rm = T)
mean(BMI[Informed==1],na.rm = T)
sd(BMI[Informed==0],na.rm = T)
sd(BMI[Informed==1],na.rm = T)
mean(BMI2[Informed==0],na.rm = T)
mean(BMI2[Informed==1],na.rm = T)
sd(BMI2[Informed==0],na.rm = T)
sd(BMI2[Informed==1],na.rm = T)
mean(BMI.BM2[Informed==0],na.rm = T)
mean(BMI.BM2[Informed==1],na.rm = T)
sd(BMI.BM2[Informed==0],na.rm = T)
sd(BMI.BM2[Informed==1],na.rm = T)
#graphs
par (mfrow =c(3, 3))
boxplot(BMI~Informed)
hist(BMI[Informed == 0], col = "red", xlab = "BMI", main = "Informed = 0")
hist(BMI[Informed == 1], col = "blue", xlab = "BMI", main = "Informed = 1")
boxplot(BMI2~Informed)
hist(BMI2[Informed == 0], col = "red", xlab = "BMI2", main = "Informed = 0")
hist(BMI2[Informed == 1], col = "blue", xlab = "BMI2", main = "Informed = 1")
boxplot(BMI.BM2~Informed)
hist(BMI.BM2[Informed == 0], col = "red", xlab = "BMI.BM2", main = "Informed = 0")
hist(BMI.BM2[Informed == 1], col = "blue", xlab = "BMI.BM2", main = "Informed = 1")
par (mfrow =c(1, 3))
plot(ecdf(BMI2[Informed == 0]), main = "Distribution function: CDF", xlab = "BMI2", col = "red")
plot(ecdf(BMI2[Informed == 1]), main = "Distribution function: CDF", xlab = "BMI2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(BMI[Informed == 0]), main = "Distribution function: CDF", xlab = "BMI", col = "red")
plot(ecdf(BMI[Informed == 1]), main = "Distribution function: CDF", xlab = "BMI", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(BMI.BM2[Informed == 0]), main = "Distribution function: CDF", xlab = "BMI.BM2", col = "red")
plot(ecdf(BMI.BM2[Informed == 1]), main = "Distribution function: CDF", xlab = "BMI.BM2", col = "blue", add = T)
legend(x = "right", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))


# ---------------- Fat,Fat2,Fat.Ft2 ----------------
mean(Fat[Informed==0],na.rm = T)
mean(Fat[Informed==1],na.rm = T)
sd(Fat[Informed==0],na.rm = T)
sd(Fat[Informed==1],na.rm = T)
mean(Fat2[Informed==0],na.rm = T)
mean(Fat2[Informed==1],na.rm = T)
sd(Fat2[Informed==0],na.rm = T)
sd(Fat2[Informed==1],na.rm = T)
mean(Fat.Ft2[Informed==0],na.rm = T)
mean(Fat.Ft2[Informed==1],na.rm = T)
sd(Fat.Ft2[Informed==0],na.rm = T)
sd(Fat.Ft2[Informed==1],na.rm = T)
#graphs
par (mfrow =c(3, 3))
boxplot(Fat~Informed)
hist(Fat[Informed == 0], col = "red", xlab = "Fat", main = "Informed = 0")
hist(Fat[Informed == 1], col = "blue", xlab = "Fat", main = "Informed = 1")
boxplot(Fat2~Informed)
hist(Fat2[Informed == 0], col = "red", xlab = "Fat2", main = "Informed = 0")
hist(Fat2[Informed == 1], col = "blue", xlab = "Fat2", main = "Informed = 1")
boxplot(Fat.Ft2~Informed)
hist(Fat.Ft2[Informed == 0], col = "red", xlab = "Fat.Ft2", main = "Informed = 0")
hist(Fat.Ft2[Informed == 1], col = "blue", xlab = "Fat.Ft2", main = "Informed = 1")
par (mfrow =c(1, 3))
plot(ecdf(Fat[Informed == 0]), main = "Distribution function: CDF", xlab = "Fat", col = "red")
plot(ecdf(Fat[Informed == 1]), main = "Distribution function: CDF", xlab = "Fat", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(Fat2[Informed == 0]), main = "Distribution function: CDF", xlab = "Fat2", col = "red")
plot(ecdf(Fat2[Informed == 1]), main = "Distribution function: CDF", xlab = "Fat2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(Fat.Ft2[Informed == 0]), main = "Distribution function: CDF", xlab = "Fat.Ft2", col = "red")
plot(ecdf(Fat.Ft2[Informed == 1]), main = "Distribution function: CDF", xlab = "Fat.Ft2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))


# ---------------- WHR,WHR2----------------
mean(WHR[Informed==0],na.rm = T)
mean(WHR[Informed==1],na.rm = T)
sd(WHR[Informed==0],na.rm = T)
sd(WHR[Informed==1],na.rm = T)
mean(WHR2[Informed==0],na.rm = T)
mean(WHR2[Informed==1],na.rm = T)
sd(WHR2[Informed==0],na.rm = T)
sd(WHR2[Informed==1],na.rm = T)
#graphs
par (mfrow =c(2, 3))
boxplot(WHR~Informed)
hist(WHR[Informed == 0], col = "red", xlab = "WHR", main = "Informed = 0")
hist(WHR[Informed == 1], col = "blue", xlab = "WHR", main = "Informed = 1")
boxplot(WHR2~Informed)
hist(WHR2[Informed == 0], col = "red", xlab = "WHR2", main = "Informed = 0")
hist(WHR2[Informed == 1], col = "blue", xlab = "WHR2", main = "Informed = 1")
par (mfrow =c(1, 2))
plot(ecdf(WHR[Informed == 0]), main = "Distribution function: CDF", xlab = "WHR", col = "red")
plot(ecdf(WHR[Informed == 1]), main = "Distribution function: CDF", xlab = "WHR", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(WHR2[Informed == 0]), main = "Distribution function: CDF", xlab = "WHR2", col = "red")
plot(ecdf(WHR2[Informed == 1]), main = "Distribution function: CDF", xlab = "WHR2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))

# ---------------- Syst,Syst2----------------
mean(Syst[Informed==0],na.rm = T)
mean(Syst[Informed==1],na.rm = T)
sd(Syst[Informed==0],na.rm = T)
sd(Syst[Informed==1],na.rm = T)
mean(Syst2[Informed==0],na.rm = T)
mean(Syst2[Informed==1],na.rm = T)
sd(Syst2[Informed==0],na.rm = T)
sd(Syst2[Informed==1],na.rm = T)
#graphs
par (mfrow =c(2, 3))
boxplot(Syst~Informed)
hist(Syst[Informed == 0], col = "red", xlab = "Syst", main = "Informed = 0")
hist(Syst[Informed == 1], col = "blue", xlab = "Syst", main = "Informed = 1")
boxplot(Syst2~Informed)
hist(Syst2[Informed == 0], col = "red", xlab = "Syst2", main = "Informed = 0")
hist(Syst2[Informed == 1], col = "blue", xlab = "Syst2", main = "Informed = 1")
par (mfrow =c(1, 2))
plot(ecdf(Syst[Informed == 0]), main = "Distribution function: CDF", xlab = "Syst", col = "red")
plot(ecdf(Syst[Informed == 1]), main = "Distribution function: CDF", xlab = "Syst", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(Syst2[Informed == 0]), main = "Distribution function: CDF", xlab = "Syst2", col = "red")
plot(ecdf(Syst2[Informed == 1]), main = "Distribution function: CDF", xlab = "Syst2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))


# ---------------- Diast,Diast2,Dst.Ds2----------------
mean(Diast[Informed==0],na.rm = T)
mean(Diast[Informed==1],na.rm = T)
sd(Diast[Informed==0],na.rm = T)
sd(Diast[Informed==1],na.rm = T)
mean(Diast2[Informed==0],na.rm = T)
mean(Diast2[Informed==1],na.rm = T)
sd(Diast2[Informed==0],na.rm = T)
sd(Diast2[Informed==1],na.rm = T)
mean(Dst.Ds2[Informed==0],na.rm = T)
mean(Dst.Ds2[Informed==1],na.rm = T)
sd(Dst.Ds2[Informed==0],na.rm = T)
sd(Dst.Ds2[Informed==1],na.rm = T)
#graphs
par (mfrow =c(3, 3))
boxplot(Diast~Informed)
hist(Diast[Informed == 0], col = "red", xlab = "Diast", main = "Informed = 0")
hist(Diast[Informed == 1], col = "blue", xlab = "Diast", main = "Informed = 1")
boxplot(Diast2~Informed)
hist(Diast2[Informed == 0], col = "red", xlab = "Diast2", main = "Informed = 0")
hist(Diast2[Informed == 1], col = "blue", xlab = "Diast2", main = "Informed = 1")
boxplot(Dst.Ds2~Informed)
hist(Dst.Ds2[Informed == 0], col = "red", xlab = "Dst.Ds2", main = "Informed = 0")
hist(Dst.Ds2[Informed == 1], col = "blue", xlab = "Dst.Ds2", main = "Informed = 1")
par (mfrow =c(1, 3))
plot(ecdf(Diast[Informed == 0]), main = "Distribution function: CDF", xlab = "Diast", col = "red")
plot(ecdf(Diast[Informed == 1]), main = "Distribution function: CDF", xlab = "Diast", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(Diast2[Informed == 0]), main = "Distribution function: CDF", xlab = "Diast2", col = "red")
plot(ecdf(Diast2[Informed == 1]), main = "Distribution function: CDF", xlab = "Diast2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))
plot(ecdf(Dst.Ds2[Informed == 0]), main = "Distribution function: CDF", xlab = "Dst.Ds2", col = "red")
plot(ecdf(Dst.Ds2[Informed == 1]), main = "Distribution function: CDF", xlab = "Dst.Ds2", col = "blue", add = T)
legend(x = "topleft", legend = c("Not Informed", "Informed"), fill = c("red", "blue"))


#--------------------------------------------------
# Question 3.2: Correlation structure 
#--------------------------------------------------

# Retrieve all quantitative variables 
dataC <- subset(data, select = -c(Informed))

# Correlation using pairwise case-deletion 
cor<-cor(dataC,use="pairwise.complete.obs")

# Visualisation of correlation matrix
library(corrplot)
corrplot(cor)

# Scatter plot matrix 
dataR <- subset(data, select = -c(Wt2, BMI2, Fat2, Syst2,Diast2,WHR2, Fat.Ft2,BMI.BM2,Dst.Ds2))
colvector <- as.integer(Informed)+1
pairs(dataR[,2:8], pch = 16,  cex = 0.5, col = colvector)


#--------------------------------------------------
# Question 3.3: Impact of qualitative variable 
#--------------------------------------------------

# Impact on the variable Age -> boxplot and ecdf
boxplot(Age ~ Informed, varwidth=TRUE, main="Age", names=c("Not informed","Informed"))
plot(ecdf(Age[Informed==1]), main ="Age")
plot(ecdf(Age[Informed==0]), col="red", add=TRUE)
# T-test
t.test(Age[Informed==0],Age[Informed==1])
#Rejection of null hypothesis of no difference in the means

# Impact on variable BMI.BM2 -> boxplot, ecdf
boxplot(BMI.BM2 ~ Informed, varwidth=TRUE, main ="Difference between BMI and BMI2", , names=c("Not informed","Informed"))
plot(ecdf(BMI.BM2[Informed==1]), main ="Difference between BMI and BMI2")
plot(ecdf(BMI.BM2[Informed==0]), col="red", add=TRUE)
#T-test 
t.test(BMI.BM2[Informed==0],BMI.BM2[Informed==1])
#Rejection of null hypothesis of no difference in the means

#--------------------------------------------------
# Question 3.4: Outliers  
#--------------------------------------------------

library(naniar)
library(ggplot2)

# Boxplot of attributes
boxplot(Wt, Wt2, main="Pourcentage of body weigth")
boxplot(BMI, BMI2, main="Body mass index")
boxplot(Fat, Fat2, main="Pourcentage of body fat")
boxplot(WHR, WHR2, main="Waist-to-hip ratio")
boxplot(Syst, Syst2, main="Systolic blood pressure")
boxplot(Diast, Diast2, main="Diastolic blood pressure")

# Syst attribute for outlier analysis
# z-Score
zSyst <- scale(Syst)
# Graph of the z-Score
plot(zSyst, type='h')
abline(h=2, col='red')
# Histogram 
hist(Syst, freq=FALSE, col="gray", xlab="Systolic")
curve(dnorm(x, mean=mean(Syst, na.rm = TRUE), sd=sd(Syst, na.rm = TRUE)), add=TRUE, col="red") 

# Syst2 attribute for outlier analysis
# z-Score
zSyst2 <- scale(Syst2)
# Graph of the z-Score
plot(zSyst2, type='h')
abline(h=2, col='red')
# Histogram 
hist(Syst2, freq=FALSE, col="gray", xlab="Systolic 2")
curve(dnorm(x, mean=mean(Syst2, na.rm = TRUE), sd=sd(Syst2, na.rm = TRUE)), add=TRUE, col="red")

# Diast attribute for outlier analysis
# z-Score
zDiast <- scale(Diast)
# Graph of the z-Score
plot(zDiast, type='h')
abline(h=2, col='red')
# Histogram 
hist(Diast, freq=FALSE, col="gray", xlab="Diastolic")
curve(dnorm(x, mean=mean(Diast, na.rm = TRUE), sd=sd(Diast, na.rm = TRUE)), add=TRUE, col="red")

# Diast2 attribute for outlier analysis
# z-Score
zDiast2 <- scale(Diast2)
# Graph of the z-Score
plot(zDiast2, type='h')
abline(h=2, col='red')
# Histogram 
hist(Diast2, freq=FALSE, col="gray", xlab="Diastolic 2")
curve(dnorm(x, mean=mean(Diast2, na.rm = TRUE), sd=sd(Diast2, na.rm = TRUE)), add=TRUE, col="red")

# Mahalanobis Distance analysis

x <- subset(data, select = -c(Informed, Fat,Fat2, Fat.Ft2,BMI.BM2,Dst.Ds2))
#Not considering the variables Fat and Fat2 to avoid a large reduction of data set size. 

m<-colMeans(x,na.rm=TRUE)
S<-var(x,na.rm=TRUE)
d<-mahalanobis(x,m,S)
plot(d,type="h", main = "Mahalanobis distance")
abline(h=qchisq(0.95,11), col='red')
identify(d)
#Chosen cutoff : 95% quartile of the corresponding chi-square distribution. 