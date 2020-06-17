require(ggplot2)
require(corrplot)
require(ade4)


## Univariate analysis.
summary(XXData)
head(XXData)
boxplot(XXData, main="Boxplots des données")

## ACP.

# boxplot centré réduit.
boxplot(scale(XXData, center=T, scale=T),main="Boxplot des données centrées et réduites")

# Visualisation de la matrice de corrélation.
cor(XXData)
corrplot(cor(XXData), main="Corrélation entre les variables", type="upper")

# Diagonalisation de la matrice de corrélation et étude des valeurs propres.
eigen(cor(XXData))
XXData.pca <- dudi.pca(XXData, scale=T, center=T, scannf=F, nf=4) #https://www.rdocumentation.org/packages/ade4/versions/1.7-15/topics/dudi.pca
screeplot(XXData.pca, type="bar", main="Inertie en fonction de la composante PCA") # Critere de Kaiser
barplot(XXData.pca$eig/sum(XXData.pca$eig)*100, names.arg=c("V1","V2","V3","V4","V5","V6","V7","V8"), ylim=c(0,100), ylab="Pourcentage de la variance",xlab="Composante",main="Pourcentage de la variance expliqué par composante") # Critere du coude






## Classification.