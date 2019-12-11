# Loading of data set.
data <- readRDS(file = "dataFinal.Rds")
attach(data)

" ------------------------------------------------------ "
" QUESTION 3 : Preliminaries                             "
" ------------------------------------------------------ "
#Age 
boxplot(Age ~ Informed, varwidth=TRUE, main="Age", names=c("NI","I"), xlab = NA)
t.test(Age[Informed==0],Age[Informed==1])

#Wt 
boxplot(Wt ~ Informed, axes = TRUE, main="Weight 1", names=c("NI","I"), xlab = NA)
boxplot(Wt2 ~ Informed, axes = TRUE, main="Weight 2", names=c("NI","I"), xlab = NA)
t.test(Wt[Informed==0],Wt[Informed==1])
t.test(Wt2[Informed==0],Wt2[Informed==1])

#BMI
boxplot(BMI ~ Informed, axes = TRUE, main="BMI 1",names=c("NI","I"), xlab = NA)
boxplot(BMI2 ~ Informed, axes = TRUE, main="BMI 2",names=c("NI","I"), xlab = NA)
t.test(BMI[Informed==0],BMI[Informed==1])
t.test(BMI2[Informed==0],BMI2[Informed==1])

#WHR 
boxplot(WHR ~ Informed, axes = TRUE, main="WHR 1",names=c("NI","I"), xlab = NA)
boxplot(WHR2 ~ Informed, axes = TRUE, main="WHR 2",names=c("NI","I"), xlab = NA)
t.test(WHR[Informed==0],WHR[Informed==1])
t.test(WHR2[Informed==0],WHR2[Informed==1])

#Fat 
boxplot(Fat ~ Informed, axes = TRUE, main="Fat 1",names=c("NI","I"), xlab = NA)
boxplot(Fat2 ~ Informed, axes = TRUE, main="Fat 2",names=c("NI","I"), xlab = NA)
t.test(Fat[Informed==0],Fat[Informed==1])
t.test(Fat2[Informed==0],Fat2[Informed==1])

#Syst
boxplot(Syst ~ Informed, varwidth=TRUE, main="Syst",names=c("NI","I"), xlab = NA)
boxplot(Syst2 ~ Informed, varwidth=TRUE, main="Syst2",names=c("NI","I"), xlab = NA)
t.test(Syst[Informed==0],Syst[Informed==1])
t.test(Syst2[Informed==0],Syst2[Informed==1])

#Diast 
boxplot(Diast ~ Informed, varwidth=TRUE, main="Diast",names=c("NI","I"), xlab = NA)
boxplot(Diast2 ~ Informed, varwidth=TRUE, main="Diast2",names=c("NI","I"), xlab = NA)
t.test(Diast[Informed==0],Diast[Informed==1])
t.test(Diast2[Informed==0],Diast2[Informed==1])

#BMI.BM2
boxplot(BMI.BM2 ~ Informed, varwidth=TRUE, main ="BMI.BMI2",names=c("NI","I"), xlab = NA)
t.test(BMI.BM2[Informed==0],BMI.BM2[Informed==1])

#Fat.Ft2
boxplot(Fat.Ft2 ~ Informed, varwidth=TRUE, main ="Fat.Fat2",names=c("NI","I"), xlab = NA)
t.test(Fat.Ft2[Informed==0],Fat.Ft2[Informed==1])

#Dst.Ds2
boxplot(Dst.Ds2 ~ Informed, varwidth=TRUE, main ="Diast.Diast2",names=c("NI","I"), xlab = NA)
t.test(Dst.Ds2[Informed==0],Dst.Ds2[Informed==1])

#MANOVA test : test of comparison of the mean vectors 
x <- cbind(Age, Wt, Wt2, BMI, BMI2, Fat, Fat2,
           WHR, WHR2, Syst, Syst2, Diast, Diast2)
y <- Informed
summary(manova(x ~ y), test="Wilks")

" ------------------------------------------------------ "
" QUESTION 4 : Logistic regression                       "
" ------------------------------------------------------ "
#First test performed without the difference variables as they contain the same information
GLM1 <- glm(Informed ~ Age + Wt + Wt2 + BMI + BMI2 + Fat + Fat2 +
              WHR + WHR2 + Syst + Syst2 + Diast + Diast2, family=binomial(logit))
summary(GLM1)

#Test with the variables that had a significant difference for the two groups
GLM2 <- glm(Informed ~ Age + BMI.BM2 + Fat.Ft2, family=binomial(logit))
summary(GLM2)

#Forward selection 
library(MASS)
GLM0 <- glm(Informed~ 1, family=binomial(logit))
stepAIC(GLM0, scope=Informed ~ Age + Wt + Wt2 + BMI + BMI2 + BMI.BM2 + Fat + Fat2 + Fat.Ft2 +
          WHR + WHR2 + Syst + Syst2 + Diast + Diast2 + Dst.Ds2, direction="forward")

#Optimal model  
GLM3 <- glm(formula = Informed ~ Age + BMI.BM2 + Syst + Fat.Ft2 + BMI + 
              Wt2, family = binomial(logit))
summary(GLM3)

#Checking multicolinearity between the variables of the optimal model 
library(car)
vif(lm(Informed ~ Age + BMI.BM2 + Syst + Fat.Ft2 + BMI + Wt2))

#Residuals 
plot(residuals(GLM3, type="pearson"), type="h", ylab="Pearson residuals")
#identify(residuals(GLM3, type="pearson"))
GLM3$fitted[24]
GLM3$fitted[35]

library(dplyr)
X_0 <- filter(data, Informed==0)
X_1 <- filter(data, Informed==1)
colMeans(X_0)
colMeans(X_1)
#24 have values closer to the mean values of the Informed group
#35 have values closer to the mean values of the Not Informed group.

#Fitted values 
plot(GLM3$linear.predictors, GLM3$fitted, col=Informed+1, pch=16, xlab="Linear predictor", ylab="Fitted values") 
abline(h=0.5, col="green", lty= 2)
#points(GLM3$linear.predictors[24], GLM3$fitted[24], col="blue", pch=16)
#points(GLM3$linear.predictors[35], GLM3$fitted[35], col="blue", pch=16)

#Predictions 
threshold <- 0.5 
predict <- as.integer(GLM3$fitted >= threshold)

#Confusion matrix 
confM <- table(Informed, predict)
#Empirical error rate :
#(FN + FP)/n
(confM[2,1]+confM[1,2])/nrow(data)

#Leave-one-out cross-validation (handmade) 
proba <- NULL
linear.predictors <- NULL
for(i in 1:length(Informed))
{
  newdata <- data[-i,]
  GLM <- glm(Informed ~ Age + BMI.BM2 + Syst + Fat.Ft2 + BMI + Wt2, family=binomial(logit), data=newdata)
  proba[i] <- exp(GLM$coef%*%c(1,as.numeric(data[i,c(2,16,11,15,5,4)])))/(1+exp(GLM$coef%*%c(1,as.numeric(data[i,c(2,16,11,15,5,4)]))))
  linear.predictors[i] <- as.matrix(cbind(rep(1,length(Informed)),data[i,c(2,16,11,15,5,4)])) %*% GLM$coef
}

#Fitted probabilities
plot(linear.predictors, proba, col=Informed+1, pch=16, xlab="Linear predictor", ylab="Fitted probabilities") 
abline(h=0.5, col="green", lty= 2)

#Confusion matrix - error 
confMatrix <- table(Informed, as.integer(proba >= threshold))
(confMatrix[2,1] + confMatrix[1,2])/nrow(data)


" ------------------------------------------------------ "
" QUESTION 5 : LDA                                       "
" ------------------------------------------------------ "
#Discriminant power
discriPower <- function(svd)
{
  g <- 2
  n <- length(Informed)
  l1 <- (g-1)*(svd)^2 / n 
  g1 <- l1/(1+l1)
  
  return(g1)
}

#Error rate 
error <- function(confusionM)
{
  (confusionM[2,1] + confusionM[1,2])/nrow(data)
}

library(MASS)
#LDA with full dataset except difference variables 
x <- data[,c("Age","Wt", "Wt2","BMI", "BMI2","Fat","Fat2","WHR","WHR2","Syst","Syst2","Diast","Diast2")]
ldafull <- lda(x=x, grouping=Informed)
ldafull <- lda(x=x, grouping=Informed, prior = c(0.5, 0.5))
discriPower(ldafull$svd) 

#LDA with variables replaced by the difference variables
xd <- data[,c("Age","Wt", "Wt2","WHR","WHR2","Syst","Syst2","BMI.BM2", "Fat.Ft2", "Dst.Ds2")]
ldadiff <- lda(x=xd, grouping=Informed)
discriPower(ldadiff$svd) 

#Scores
scores <- predict(ldafull,x)$x 
plot(scores, Informed)
#abline(v=-0.306304, col="green", lty= 2) #Classification threshold
boxplot(scores ~ Informed)

#Classification 
class <- predict(ldafull, x)$clas
confMFull <- table(Informed, class)
error(confMFull)

#Classification rule 
mu0 <- mean(predict(ldafull,x)$x[Informed==0])
mu1 <- mean(predict(ldafull,x)$x[Informed==1])
threshold <- as.double(((mu0 + mu1)/2) + (log(ldafull$prior[2] / ldafull$prior[1])/(mu0 - mu1)))
#-0.306304
classR <- as.integer(predict(ldafull,x)$x >= threshold) 
table(Informed, classR)

#Simplification trial 
dataI <- subset(data, select = -c(Informed,Fat.Ft2, BMI.BM2,Dst.Ds2))
gamma <- NULL
for(i in 1:length(dataI))
{
  newdata <- dataI[,-i]
  model <- lda(x=newdata, grouping=Informed)
  gamma[i] <- discriPower(model$svd)
}

x1 <- data[,c("Age","Wt", "Wt2","BMI", "BMI2","Fat","Fat2","WHR","WHR2","Syst","Diast", "Diast2")] #without Syst2
x1 <- data[,c("Age","Wt", "Wt2","BMI", "BMI2","Fat","Fat2","WHR","WHR2","Syst","Diast")] #without Diast2
x1 <- data[,c("Age","Wt", "Wt2","BMI", "BMI2","Fat","Fat2","WHR","WHR2","Syst")] #without Diast
x1 <- data[,c("Age","Wt2","BMI", "BMI2","Fat","Fat2","WHR","WHR2","Syst")] #without Wt
x1 <- data[,c("Age","BMI", "BMI2","Fat","Fat2","WHR", "WHR2", "Syst")] #without Wt2
ldares <- lda(x=x1, grouping=Informed)
discriPower(ldares$svd)

#Trial only with variables identified as containing info
xr <- data[,c("Age", "BMI.BM2","Fat.Ft2")]
ldaress <- lda(x=xr, grouping=Informed)
discriPower(ldaress$svd) 

#Final simplified model  
xs <- data[,c("Age","BMI", "BMI2","Fat","Fat2","WHR", "WHR2", "Syst")]
ldaless <- lda(x=xs, grouping=Informed)
discriPower(ldaless$svd) 

#Classification simplified model 
classless <- predict(ldaless, xs)$clas
confMLess <- table(Informed, classless)
error(confMLess) 

#Homoscedasticity assumption:
#boxplot graphics
boxplot(Age ~ Informed)
boxplot(Wt ~ Informed)
boxplot(Wt2 ~ Informed)
boxplot(BMI ~ Informed)
boxplot(BMI2 ~ Informed)
boxplot(Fat ~ Informed)
boxplot(Fat2 ~ Informed)
boxplot(WHR ~ Informed)
boxplot(WHR2 ~ Informed)
boxplot(Syst ~ Informed)
boxplot(Syst2 ~ Informed)
boxplot(Diast ~ Informed)
boxplot(Diast2 ~ Informed)

#F-test
m0 <- as.matrix(filter(data, Informed ==0))[,2:14]
m1 <- as.matrix(filter(data, Informed ==1))[,2:14]
var.test(m0, m1) # p-value > 0.05 we accept H0: var(m0) = var(m1)


