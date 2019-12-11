" ------------------------------------------------------ "
" Handling of missing data                               "
" ------------------------------------------------------ "

"
This function fills NAs values of vector columns with the mean of this
column
Parameters
- columns -> It is a vector containing the columns where the
                          replacement has to be done
- X -> Dataset
"
function_mean<- function(columns, X) 
{
  for(i in columns)
  {
    X[is.na(X[,i]), i] <- mean(X[,i], na.rm = TRUE)
  }
  return (X)
}

"
This function fills NAs values with a linear regression of the 
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


data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)


# Deletion of some particular observations 
# Those deletions are justified in the report
X <- data[-c(13,14,16,28,29,30,36,55,60),] #66 obs (88%)
summary(X)

library(dplyr)
X_0 <- filter(X, Informed==0)
X_1 <- filter(X, Informed==1)

summary(X_1)
summary(X_0)

"
We can observe that columns 'WHR', 'Syst', 'Syst2' and 'Diast' in X_0
Only miss one value respectively. Let's replace them by the mean
taking into account the qualitative variable : 'informed'.
"
X_0 <- function_mean(c(9, 11, 12, 13), X_0)
"
Only one missing value for WHR2 in X_1
"
X_1 <- function_mean(c(10), X_1)

"
Missing values of columns 'Fat', 'Fat2' and 'WHR2'
will be filled thanks to linear regression taking into account the
qualitative variable : 'informed' and the period when the 
measure were taken.
"
plot(X_0[,"Wt"], X_0[,"Fat"],  xlab="Wt", ylab="Fat" , main="Relation Wt and Fat - Control group")
plot(X_0[,"Wt2"], X_0[,"Fat2"],  xlab="Wt2", ylab="Fat2", main="Relation Wt2 and Fat2 - Control group")
plot(X_0[,"WHR"], X_0[,"WHR2"], xlab="WHR", ylab="WHR2", main="Relation WHR and WHR2 - Control group")

plot(X_1[,"Wt"], X_1[,"Fat"],  xlab="Wt", ylab="Fat", main="Relation Wt and Fat - Informed group")
plot(X_1[,"Wt2"], X_1[,"Fat2"],  xlab="Wt2", ylab="Fat2", main="Relation Wt2 and Fat2 - Informed group")

X_0 <- function_linear_regression(X_0, "WHR2", "WHR")
X_0 <- function_linear_regression(X_0, "Fat", "Wt")
X_0 <- function_linear_regression(X_0, "Fat2", "Wt2")

X_1 <- function_linear_regression(X_1, "Fat", "Wt")
X_1 <- function_linear_regression(X_1, "Fat2", "Wt2")

# Merging of the two dataset X_0 and X_1
XFinal <- rbind(X_0, X_1) # --> FINAL DATASET 
colMeans(is.na(XFinal)) # No missing value.

#Computation of BMI.BM2, Dst.Ds2, Fat.Ft2
XFinal[,"BMI.BM2"] <- XFinal[,"BMI"] - XFinal[,"BMI2"]
XFinal[,"Fat.Ft2"] <- XFinal[,"Fat"] - XFinal[,"Fat2"]
XFinal[,"Dst.Ds2"] <- XFinal[,"Diast"] - XFinal[,"Diast2"]

colMeans(is.na(XFinal))

saveRDS(XFinal, file = "dataFinal.Rds")

"
Comparison of the correlation matrix for the original dataset 
and the imputed dataset.
"
X <- XFinal[,-c(Informed)]
library(corrplot)
#Imputated dataset 
covI <- cov(X)
corI <- cor(X)
corrplot(corI)

#Original dataset 
cov <- cov(data[,-c(Informed)], use="pairwise.complete.obs")
cor <- cor(data[,-c(Informed)], use="pairwise.complete.obs")
corrplot(cor)
