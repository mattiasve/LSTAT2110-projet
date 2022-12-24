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
summary(abalone)

# 1. Moyennes
moyennes <- round(colMeans(abalone[2:9]),3)
moyennes <- as.data.frame(moyennes)
t(moyennes)

#https://shapbio.me/courses/biolB215s14/abalone_cleaning.html
# clean dataset first!


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
res<-PCA(Dataset_PCA, graph=FALSE)


# Vecteurs propres et valeurs propres :
VaP <- eigen(R)$values
VaP <- as.data.frame(VaP)
t(VaP)

# pourcentage de variance et variance cumulée
par(mfrow=c(1,2))
barplot(res$eig[,"percentage of variance"], xlab = "Composantes", ylab="Pourcentage de variance")
barplot(res$eig[,"cumulative percentage of variance"],xlab="Composantes",ylab="Pourcentage cumulatif de variance")

# Combien de composantes retenir
par(mfrow=c(1,1))
barplot(res$eig[,"eigenvalue"], xlab = "Composantes", ylab="Valeur propre")
abline(h = 1, lty = "dashed")

#Valeurs propres et pourcentages de variance
par(mfrow=c(1,2))
plot(res$eig[,"eigenvalue"],type="l",xlab="Composantes",ylab="Valeurs propres")
plot(res$eig[,"percentage of variance"],type="l",xlab="Composantes",ylab="Pourcentage de variance")

#####  RESULTAT ET DISCUSSION #####
### Analyse variables ####
# Variables mal représentées par les 2 Premières composantes:
cos2 <- round(sort(rowSums(res$var$cos2[,1:2])), digits = 3)   
contrib <- round(sort(rowSums(res$var$contrib[,1:2])), digits = 3)
# Pour vérifier pour les 2 composantes suivantes, changer le "1:2" et "3:4" ci-dessus
cos2 <- as.data.frame(cos2)
contrib <- as.data.frame(contrib)
df_var <- data.frame(cos2, contrib)
colnames(df_var) <- c('cos2 [-]', 'contribution [%]')
t(df_var)

# Réaliser l'analyse en composante principale 
## cercle de corrélation
plot.PCA(res, choix = "var", axes=c(1,2))

# Interprétation des composantes principales
## test Student sur la nullité du coef de corrélation
res_t = dimdesc(res, axes=c(1,2))
res_t$Dim.1$quanti #résultat bizarre pour la p-valeur !! 
res_t$Dim.2$quanti

#### Analyse individus #####
# individus mal représenté sur les 2 premières composantes
cos2_ind <- as.data.frame(round(sort(rowSums(res$ind$cos2[,1:2])), digits = 3))
contrib_ind <- as.data.frame(round(sort(rowSums(res$ind$contrib[,1:2])), digits = 3))
df_ind <- data.frame(cos2_ind, contrib_ind)
colnames(df_ind) <- c('cos2 [-]', 'contribution [%]')
t(df_ind)  # trop d'individus -> sert à rien
#visualize summary
summary(df_ind)
# boxplot ?
ggplot() +
  geom_boxplot(data=df_ind, aes(y='cos2'))

# carte des individus
## basique
plot.PCA(res, choix = "ind", axes=c(1,2), label='none')
## avec sexe
Dataset_PCA2 <- cbind(Dataset_PCA, abalone[1])
res2 <- PCA(Dataset_PCA2, ncp = 2, graph = TRUE, 
           quali.sup = c(9))
plot(res2, choix = "ind", habillage = 9,
     col.hab=c('lightsalmon', 'darkolivegreen4', 'steelblue1'), 
     label='none') +theme_gray()


