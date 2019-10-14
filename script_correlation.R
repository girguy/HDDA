#Correlation structure of quantitative variables. 
dataC <- subset(data, select = -c(Informed))

cor<-cor(dataC,use="pairwise.complete.obs")

#Visualisation of correlation matrix
library(corrplot)
corrplot(cor)

#Plots of some correlated variables 
plot(Wt,BMI, pch=16)
cor(dataC[,c("Wt", "BMI")], use="pairwise.complete.obs")

plot(BMI, WHR, pch=16)
cor(dataC[,c("BMI", "WHR")], use="pairwise.complete.obs")

plot(Age,Syst, pch=16)
cor(dataC[,c("Age", "Syst")], use="pairwise.complete.obs")

