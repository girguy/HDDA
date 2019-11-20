library(naniar)
library(ggplot2)
library(corrplot)
library(rrcovNA)
library(corrplot)
library(qgraph)

data <- read.table("hotel-maids.txt", sep=";", header=TRUE, na.strings = "NA")
attach(data)

# Retrieve all quantitative variables and
# columns with linear relationships with other colums in
# order to avoid singular covariance matrices
dataC <- subset(data, select = -c(Informed, Fat.Ft2, BMI.BM2, Dst.Ds2))

# Pourcentage of missing values in every columns
colMeans(is.na(dataC))
# Only wt and wt2 are complete.

# Variable with missing values :
# Age : 1 -> delete the row

# BMI : 1 -> 0 (thanks to deletions)

# BMI2 : 2 -> 0 (thanks to deletions)

# FAT : 7 -> 6 (thanks to deletions) => lin. reg with Wt
plot(data[,3], data[,7],  xlab="Wt", ylab="FAT")

# FAT2 : 19 -> 14 (thanks to deletions) => lin. reg with Wt
plot(data[,3], data[,8],  xlab="Wt", ylab="FAT2")

# WHR : 2 -> 1 (thanks to deletions) => take the mean

# WHR2 : 8 -> 5 (thanks to previous deletion) => lin. reg with WHR
plot(data[,9], data[,10], xlab="WHR", ylab="WHR2")

# Syst(2), Diast(2) : Deletions of rows, only a loss of 12%.

X <- dataC[-c(13,14,16,28,29,30,36,55,60),] #66 obs (88%)
colMeans(is.na(X))
# We can observe that columns 'WHR', 'Syst', 'Syst2' and 'Diast'
# Only miss one value respectively. Let's replace them by the mean.
for(i in c(8, 10, 11, 12)){
  X[is.na(X[,i]), i] <- mean(X[,i], na.rm = TRUE)
}
colMeans(is.na(X))

# Missing values of columns 'Fat', 'Fat2' and 'WHR2'
# will be fill thanks to linear regression

# For WHR2
trainSet <- subset(X, select=c("WHR2", "WHR"))
trainSet <- na.omit(trainSet)
# Linear model 
linmodel <- lm(trainSet[,1]~trainSet[,2], data=trainSet)
# Replacing NAs in the dataset by the regression values
X[["WHR2"]][which(is.na(X["WHR2"]))] <- linmodel$coefficients[2]*X[["WHR"]][which(is.na(X["WHR2"]))]
+ linmodel$coefficients[1]


# For 'Fat' and 'Fat2'
colNames <- c("Fat","Fat2")
for(i in colNames){
  print(i)
  trainSet <- subset(X, select=c(i, "Wt"))
  trainSet <- na.omit(trainSet)
  linmodel <- lm(trainSet[,1]~trainSet[,2], data=trainSet)
  X[[i]][which(is.na(X[i]))] <- linmodel$coefficients[2]*X[["Wt"]][which(is.na(X[i]))]
  + linmodel$coefficients[1]
}

colMeans(is.na(X))

" -------------- "
" QUESTION 3.1   "
" -------------- "
# Not done yet


" -------------- "
" QUESTION 3.2.1 "
" -------------- "
# Usual covariance and correlation matrix
cov <- cov(X)
cor <- cov2cor(cov)
corrplot(cor)

# Robust covariance and correlation matrix
library(MASS)
cov_mcd <- cov.mcd(X)
cor_mcd <- cov2cor(cov_mcd$cov)
corrplot(cor_mcd)

# Scatter plot matrix 
dataR <- subset(X, select = -c(Wt2, BMI2, Fat2, Syst2,Diast2,WHR2))
colvector <- as.integer(Informed)+1
pairs(X[,2:8], pch = 16,  cex = 0.4, col = colvector)

" -------------- "
" QUESTION 3.2.2 "
" -------------- "
# a)
qgraph(cov)


# b)


# c)




" -------------- "
" QUESTION 3.3.1 "
" -------------- "
# Covariance or correlation matrix ?
# There are variables with different units, so the correlation
# matrix would be a better choice in order to compare the variables

# Robust or non robust ?


" -------------- "
" QUESTION 3.3.2 "
" -------------- "


" -------------- "
" QUESTION 3.3.3 "
" -------------- "

