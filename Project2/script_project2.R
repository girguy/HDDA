# Loading of data set.
data <- readRDS(file = "dataFinal.Rds")
attach(data)

# Data set without the qualitative variable 
dataF <- data[,-c(Informed)]

" ------------------------------------------------------ "
" QUESTION 3.1 : Robust outlier detection                "
" ------------------------------------------------------ "
library(MASS)
library(dplyr)
X_0 <- filter(data, Informed==0)[,-1]
X_1 <- filter(data, Informed==1)[,-1]

# MAHALANOBIS DISTANCE
d <- mahalanobis(dataF, center = colMeans(dataF), cov(dataF))
d_0 <- mahalanobis(X_0, center = colMeans(X_0), cov(X_0))
d_1 <- mahalanobis(X_1, center = colMeans(X_1), cov(X_1))

# ROBUST MAHALANOBIS DISTANCE
h <- floor(nrow(dataF)*0.75)
resrob <- cov.rob(dataF, method = "mcd", quantile.used = h, cor = T) # minimum covariance determinant

h0 <- floor(nrow(X_0)*0.75)
resrob_0 <- cov.rob(X_0, method = "mcd", quantile.used = h0, cor = T) # minimum covariance determinant

h1 <- floor(nrow(X_1)*0.75)
resrob_1 <- cov.rob(X_1, method = "mcd", quantile.used = h1, cor = T) # minimum covariance determinant

dR <- mahalanobis(dataF, center = resrob$center, resrob$cov) # Robust distance Mahalanobis X
dR_0 <- mahalanobis(X_0, center = resrob_0$center, resrob_0$cov) # Robust distance Mahalanobis X_0
dR_1 <- mahalanobis(X_1, center = resrob_1$center, resrob_1$cov) # Robust distance Mahalanobis X_1

# PLOTS
# X_0 & X_1
par(mfrow=c(1,3))
plot(d_0, ylab = "Mahalanobis distance", type = "h", col = "red", main = "Not Informed", ylim = c(0,25))
abline(h = qchisq(0.95, ncol(X_0)), col = "green")
plot(dR_0, ylab = "Robust Mahalanobis distance", type = "h", col = "red", main = "Not Informed")
abline(h = qchisq(0.95, ncol(X_0)), col = "green")
identify(dR_0)
plot(d_0, dR_0, xlab = "Mahalanobis distance", ylab = "Robust distance MCD", col = "red", main = "DD-plot: Not Informed", xlim = c(0,25))
abline(v = qchisq(0.95, ncol(X_0)), col = "green")
abline(h = qchisq(0.95, ncol(X_0)), col = "green")
identify(d_0, dR_0)
plot(d_1, ylab = "Mahalanobis distance", type = "h", col = "blue", main = "Informed")
abline(h = qchisq(0.95, ncol(X_1)), col = "green")
identify(d_1)
plot(dR_1, ylab = "Robust Mahalanobis distance", type = "h", col = "blue", main = "Informed")
abline(h = qchisq(0.95, ncol(X_1)), col = "green")
identify(dR_1)
plot(d_1, dR_1, xlab = "Mahalanobis distance", ylab = "Robust distance MCD", col = "blue", main = "DD-plot: Informed")
abline(v = qchisq(0.95, ncol(X_1)), col = "green")
abline(h = qchisq(0.95, ncol(X_1)), col = "green")
identify(d_1, dR_1)
# Global data set 
par(mfrow=c(1,3))
plot(d, ylab = "Mahalanobis distance", type = "h")
abline(h = qchisq(0.95, ncol(dataF)), col = "red")
identify(d)
plot(dR, ylab = "Robust Mahalanobis distance", type = "h")
abline(h = qchisq(0.95, ncol(dataF)), col = "red")
identify(dR)
plot(d, dR, xlab = "Mahalanobis distance", ylab = "Robust distance MCD", main = "DD-plot")
points(d[1:28], dR[1:28], col = "red")
points(d[29:66], dR[29:66], col = "blue")
abline(v = qchisq(0.95, ncol(dataF)), col = "green")
abline(h = qchisq(0.95, ncol(dataF)), col = "green")
identify(d, dR)
par(mfrow=c(1,1))

" ------------------------------------------------------ "
" QUESTION 3.2.1 : Robust estimation correlation matrix  "
" ------------------------------------------------------ "
# Classical covariance and correlation matrix
library(corrplot)
cov <- cov(dataF)
cor <- cor(dataF)
corrplot(cor)

# Robust covariance and correlation matrix
library(MASS)
h = floor(nrow(dataF)*0.75)
resrob <- cov.rob(dataF, quantile.used = h, method="mcd", cor=T)
covrob <- resrob$cov
corrob <- resrob$cor
corrplot(corrob)

# Possible explanation of the decrease in correlation with the robust technique
plot(dataF[,"Syst2"],dataF[,"Diast2"], xlab="Syst2", ylab="Diast2")
points(dataF[resrob$best,"Syst2"], dataF[resrob$best,"Diast2"], col="red", pch=19)


" ------------------------------------------------------ "
" QUESTION 3.2.2 : Graphical models                      "
" ------------------------------------------------------ "

# a) Graphical model based on the classic covariance matrix 
# Standardizating of the data 
dataS <- apply(dataF, 2, scale)

library(matlib)
covS <- cov(dataS)
covSInv <- inv(covS)

library(qgraph)
label = c("Age", "Wt", "Wt2", "BMI", "BMI2", "Fat", "Ft2", "WHR", "WHR2", "Sys", "Sy2", "Dst" ,"Ds2")
qgraph(covSInv, fade=FALSE, labels=label)


# b) Graphical model based on the L1-regularized covariance matrix
# L1-regularized estimation of cov matrix
library(huge)

nS <- nrow(dataS)
pS <- ncol(dataS)

# Determination of the appropriate value of the penalization parameter lambda
seqlambda <- seq(0, 1, by=0.1)
BIC <- NULL 
for(lambda in seqlambda) 
{ 
  l1reg <- huge(as.matrix(dataS), lambda, method="glasso", cov.output=TRUE) 
  l1prec <- solve(as.matrix(l1reg$cov[[1]])) 
  l1loglik <- l1reg$loglik * nS/2 
  BIC <- c(BIC, -2*l1loglik 
           + (pS + sum(l1prec[upper.tri(l1prec, diag=TRUE)] != 0) * log(nS))) 
}  
plot(seqlambda, BIC, type="b", xlab="Lambda") 
lambda <- seqlambda[which.min(BIC)]
#Analysis of the BIC results in an appropriate value of 0 

#We thus look the evolution of the nb of non-zero parameters for the estimation as a function of lambda
seqlambda <- seq(0, 1, by=0.1) 
nbNonZero <- NULL
for(lambda in seqlambda) 
{ 
  l1reg <- huge(as.matrix(dataS), lambda, method="glasso", cov.output=TRUE) 
  covRegS <- solve(as.matrix(l1reg$cov[[1]]))
  nbNonZero <- c(nbNonZero, length(which(covRegS!=0)))
}  
plot(seqlambda, nbNonZero, type="b", xlab="Lambda")

# L1-regularized estimation of covariance matrix
L1S <- huge(as.matrix(dataS), seqlambda[5], method="glasso", cov.output=TRUE)
covRegS <- solve(as.matrix(L1S$cov[[1]]))

covRegSInv <- inv(covRegS)

# Graphical models based on the regularized estimation 
label = c("Age", "Wt", "Wt2", "BMI", "BMI2", "Fat", "Ft2", "WHR", "WHR2", "Sys", "Sy2", "Dst" ,"Ds2")
qgraph(covRegSInv, fade=FALSE, labels = label)

# c) Normality assumption - graphs 
hist(Age, freq=FALSE, col="gray", xlab="Age")
curve(dnorm(x, mean=mean(Age, na.rm = TRUE), sd=sd(Age, na.rm = TRUE)), add=TRUE, col="red") 

par(mfcol =c(1,2))
hist(Wt, freq=FALSE, col="gray", xlab="Wt", ylim=c(0.00, 0.02))
curve(dnorm(x, mean=mean(Wt, na.rm = TRUE), sd=sd(Wt, na.rm = TRUE)), add=TRUE, col="red") 
hist(Wt2, freq=FALSE, col="gray", xlab="Wt2", ylim=c(0.00, 0.02))
curve(dnorm(x, mean=mean(Wt2, na.rm = TRUE), sd=sd(Wt2, na.rm = TRUE)), add=TRUE, col="red") 

par(mfcol =c(1,2))
hist(BMI, freq=FALSE, col="gray", xlab="BMI")
curve(dnorm(x, mean=mean(BMI, na.rm = TRUE), sd=sd(BMI, na.rm = TRUE)), add=TRUE, col="red") 
hist(BMI2, freq=FALSE, col="gray", xlab="BMI2")
curve(dnorm(x, mean=mean(BMI2, na.rm = TRUE), sd=sd(BMI2, na.rm = TRUE)), add=TRUE, col="red") 

par(mfcol =c(1,2))
hist(Fat, freq=FALSE, col="gray", xlab="Fat", ylim=c(0.00, 0.06))
curve(dnorm(x, mean=mean(Fat, na.rm = TRUE), sd=sd(Fat, na.rm = TRUE)), add=TRUE, col="red") 
hist(Fat2, freq=FALSE, col="gray", xlab="Fat2")
curve(dnorm(x, mean=mean(Fat2, na.rm = TRUE), sd=sd(Fat2, na.rm = TRUE)), add=TRUE, col="red") 

par(mfcol =c(1,2))
hist(WHR, freq=FALSE, col="gray", xlab="WHR", ylim=c(0, 8))
curve(dnorm(x, mean=mean(WHR, na.rm = TRUE), sd=sd(WHR, na.rm = TRUE)), add=TRUE, col="red") 
hist(WHR2, freq=FALSE, col="gray", xlab="WHR2")
curve(dnorm(x, mean=mean(WHR2, na.rm = TRUE), sd=sd(WHR2, na.rm = TRUE)), add=TRUE, col="red") 

par(mfcol =c(1,2))
hist(Syst, freq=FALSE, col="gray", xlab="Syst", ylim=c(0.00, 0.04))
curve(dnorm(x, mean=mean(Syst, na.rm = TRUE), sd=sd(Syst, na.rm = TRUE)), add=TRUE, col="red") 
hist(Syst2, freq=FALSE, col="gray", xlab="Syst2", ylim=c(0.00, 0.04))
curve(dnorm(x, mean=mean(Syst2, na.rm = TRUE), sd=sd(Syst2, na.rm = TRUE)), add=TRUE, col="red")

par(mfcol =c(1,2))
hist(Diast, freq=FALSE, col="gray", xlab="Diast", ylim=c(0.00,0.04))
curve(dnorm(x, mean=mean(Diast, na.rm = TRUE), sd=sd(Diast, na.rm = TRUE)), add=TRUE, col="red")
hist(Diast2, freq=FALSE, col="gray", xlab="Diast2")
curve(dnorm(x, mean=mean(Diast2, na.rm = TRUE), sd=sd(Diast2, na.rm = TRUE)), add=TRUE, col="red")


" ------------------------------------------------------ "
" QUESTION 3.3 : Visualisation in 2D                     "
" ------------------------------------------------------ "
# PCA 
pca.res <- princomp(dataF, cor = TRUE)

library("factoextra")
library("FactoMineR")
# 2D plot
ind <- get_pca_ind(pca.res)
fviz_pca_ind (pca.res,geom="point", habillage=data$Informed)

"
plot(pca.res$scores[,1], pca.res$scores[,2])
identify(pca.res$scores[,1], pca.res$scores[,2])
"

summary(pca.res)
#We can work with the 3 first principal components knowing
#that they explain 83.3% of the total variance.

# Scree plot
fviz_eig(pca.res, addlabels = TRUE, ylim = c(0, 50))

# Correlation circle
fviz_pca_var(pca.res,
             repel = TRUE)

var <- get_pca_var(pca.res)
head(var$cor)

# Contribution of each variables in PC1 and in PC2
par(mfrow = c(2, 1))
par(mar=rep(2.5,4), oma = c(2, 2, 0, 0))
label = c("Age", "Wt", "Wt2", "BMI", "BMI2", "Fat", "Ft2", "WHR", "WHR2", "Sys", "Sy2", "Dst" ,"Ds2")
barplot(pca.res$loadings[,1], main=paste("Dim",1), names.arg=label, las=2)
barplot(pca.res$loadings[,2], main=paste("Dim",2), names.arg=label, las=2)
par(mfrow=c(1,1))

# tSNE : 
library(Rtsne)
for(i in c(5, 10, 15, 20)){
  tsne <- Rtsne(dataF, dims=2, initial_dims=13, perplexity=i)
  plot(tsne$Y[,1],tsne$Y[,2], col=data$Informed+1, main=paste("Perplexity",i))
}

tsne <- Rtsne(dataF, dims = 2, initial_dims=13, perplexity=10)
plot(tsne$Y, col=data$Informed+1, main=paste("Perplexity",10), xlab="tSNE1", ylab="tSNE2")
#No discriminating power when the variables are considered all together.