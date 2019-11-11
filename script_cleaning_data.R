library(ggplot2)
library(corrplot)
"
This function fill NAs values of vector columns with the mean of this
column
Parameters
- columns -> It is a vector containing the column where the
                          replacement has to be done
- X -> Dataset
"
function_mean<- function(columns, X) {
  for(i in columns){
    X[is.na(X[,i]), i] <- mean(X[,i], na.rm = TRUE)
  }
  return (X)
}

"
This function fills NAs values with their linear regression of the 
form y = mx + b
Parameters
- X -> Dataset
- col1 -> corresponds to the y
- col2 -> corresponds to the x
"
function_linear_regression <- function(X, col1, col2){
  trainSet <- subset(X, select=c(col1, col2))
  trainSet <- na.omit(trainSet)
  # Linear model 
  linmodel <- lm(trainSet[,1]~trainSet[,2], data=trainSet)
  # Replacing NAs in the dataset by the regression values
  X[[col1]][which(is.na(X[col1]))] <- linmodel$coefficients[2]*X[[col2]][which(is.na(X[col1]))] + linmodel$coefficients[1]
  return (X)
}


data <- read.table("hotel-maids.txt", sep=";", header=TRUE, na.strings = "NA")
attach(data)

"
(Retrieve all quantitative variables and)
columns with linear relationships with other colums in
order to avoid singular covariance matrices
"
dataC <- subset(data, select = -c(Fat.Ft2, BMI.BM2, Dst.Ds2))
# Deletion of multiple values
# Those deletions will be expalined in the report
X <- dataC[-c(13,14,16,28,29,30,36,55,60),] #66 obs (88%)

library(dplyr)
X_0 <- filter(X, Informed==0)
X_1 <- filter(X, Informed==1)

X_0 <- X_0[,-c(Informed)]
X_1 <- X_1[,-c(Informed)]

"
We can observe that columns 'WHR', 'Syst', 'Syst2' and 'Diast'
Only miss one value respectively. Let's replace them by the mean
In function of the qualitative variable : 'informed'.
"
# There is no missing values inside columns of interest in dataset X_1.
X_0 <- function_mean(c(8, 10, 11, 12), X_0)

"
Missing values of columns 'Fat', 'Fat2' and 'WHR2'
will be fill thanks to linear regression in function of the
qualitative variable : 'informed' and the period when the 
measure were taken.
"
plot(X_0[,2], X_0[,6],  xlab="Wt", ylab="FAT")
plot(X_0[,3], X_0[,7],  xlab="Wt2", ylab="FAT2")
plot(X_0[,8], X_0[,9], xlab="WHR", ylab="WHR2")

plot(X_1[,2], X_1[,6],  xlab="Wt", ylab="FAT")
plot(X_1[,3], X_1[,7],  xlab="Wt2", ylab="FAT2")
plot(X_1[,8], X_1[,9], xlab="WHR", ylab="WHR2")

X_0 <- function_linear_regression(X_0, "WHR2", "WHR")
X_0 <- function_linear_regression(X_0, "Fat", "Wt")
X_0 <- function_linear_regression(X_0, "Fat2", "Wt2")

X_1 <- function_linear_regression(X_1, "WHR2", "WHR")
X_1 <- function_linear_regression(X_1, "Fat", "Wt")
X_1 <- function_linear_regression(X_1, "Fat2", "Wt2")
#Merging of the two dataset X_0 and X_1
X <- rbind(X_0, X_1)
colMeans(is.na(X)) # No missing value.

#Imputated dataset 
covI <- cov(X)
corI <- cor(X)
corrplot(corI)
plot(X[,"Wt2"], X[,"Fat2"],  xlab="Wt2", ylab="FAT2")


#Original dataset 
cov <- cov(dataC[,-c(Informed)], use="pairwise.complete.obs")
cor <- cor(dataC[,-c(Informed)], use="pairwise.complete.obs")
corrplot(cor)
plot(dataC[,"Wt2"], dataC[,"Fat2"],  xlab="Wt2", ylab="FAT2")