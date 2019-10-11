data <- read.table("hotel-maids.txt", sep="\t", header=TRUE, na.strings = "NA")
attach(data)
summary(data)

#Missing data visualisation 
library(naniar)
png(file = "missingData.png")
vis_miss(data)
dev.off()

#Detecting missingness mechanism
library(ggplot2)
library(naniar)
ggplot(data, aes(x=Fat2, y=Age)) + geom_miss_point()

M <- as.numeric(is.na(Loss))
t.test(Height[M==1],Height[M==0], var.equal=FALSE)