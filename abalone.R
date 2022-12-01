library(data.table)
library(naniar)
library(FactoMineR)
library(reshape2)
library(devtools)
library(factoextra)
library(ggplot2)


#### ---- description données ----- 
# import data
abalone <- read.csv('abalone.csv')
abalone$Type <- as.factor(abalone$Type)
summary(data)

# 1. Moyennes
moyennes <- round(colMeans(abalone[2:9]),3)
moyennes <- as.data.frame(moyennes)
t(moyennes)


# 2. Matrice de corrélation
corr <- round(cor(abalone[2:9]), digits = 2)
melted_corr <- melt(corr)

corr_matrix <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) )
corr_matrix


Dataset_PCA <- abalone[2:9]
R <- cor(Dataset_PCA)
# Réaliser l'analyse en composante principale :
res<-PCA(Dataset_PCA, graph=TRUE)

Dataset_PCA2 <- cbind(Dataset_PCA, abalone[1])
res2 <- PCA(Dataset_PCA2, ncp = 2, graph = TRUE, 
           quali.sup = c(9))
#plot(res2, choix = "ind")
fviz_pca_ind(res2.pca, geom="point")
