library(ggplot2)
library(corrplot)

X <- readRDS(file = "dataFinal.Rds")

" First point "

# Empirical covariance and correlation matrix
cov <- cov(X)
cor <- cov2cor(cov)
corrplot(cor)

# Robust covariance and correlation matrix done with mcd
library("MASS")
n <- length(X[,1])
cov_mcd <- cov.rob(X, method = "mcd", quantile.used = n*0.75)
cor_mcd <- cov2cor(cov_mcd$cov)
corrplot(cor_mcd)

" Second point "

pca.res <- prcomp(X, scale = TRUE) 

library("factoextra")
library("FactoMineR")
# 2D plot
ind <- get_pca_ind(pca.res)
fviz_pca_ind (pca.res, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              axes = 1:2,
              repel = TRUE
)

eigen_res <- eigen(cor_mcd)
var_ratio <- 100*eigen_res$values/sum(eigen_res$values)
sum(var_ratio[1:3])

# We can work with the 3 first principal components knowing
# that they explain 83.03% of the total variation.

# scree plot
fviz_eig(pca.res, addlabels = TRUE, ylim = c(0, 50))
# This choice can not be justify b the scree plot because there is
# no obvious elbow on the graph.

" Third point "

# Correlation circle
fviz_pca_var(pca.res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

var <- get_pca_var(pca.res)
head(var$cor)

# Contributions des variables à PC1
fviz_contrib(pca.res, choice = "var", axes = 1, top = 13)
# Contributions des variables à PC2
fviz_contrib(pca.res, choice = "var", axes = 2, top = 13)

