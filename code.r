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
XXData.cor <- cor(XXData)
corrplot(XXData.cor, main="Corrélation entre les variables", type="upper")

# Diagonalisation de la matrice de corrélation et étude des valeurs propres.
XXData.eigen <- eigen(XXData.cor)
XXData.eigen$values
XXData.pca <- dudi.pca(XXData, scale=T, center=T, scannf=F, nf=3) #https://www.rdocumentation.org/packages/ade4/versions/1.7-15/topics/dudi.pca
screeplot(XXData.pca, type="bar", main="Inertie en fonction de la composante PCA") # Critere de Kaiser
barplot(XXData.pca$eig/sum(XXData.pca$eig)*100, names.arg=c("V1","V2","V3","V4","V5","V6","V7","V8"), ylim=c(0,100), ylab="Pourcentage de la variance",xlab="Composante",main="Pourcentage de la variance expliqué par composante") # Critere du coude
score(XXData.pca)
XXData.pca$co
XXData.pca$c1
plot(XXData.pca$li)
s.corcircle(XXData.pca$co)
inertia.dudi(XXData.pca, row.inertia=T)$row.abs
scatter(XXData.pca)


## Classification.

dist = dist(XXData)
ward = hclust(dist,method = "ward.D")
#pdf("Groupement méthode de Ward.pdf")
plot(ward,main="Méthode Ward")
rect.hclust(ward, border = c("blue","red"),k = 20)
dev.off()
inertie = sort(ward$height,decreasing = T)
#pdf("Inertie_Ward.pdf")
plot(inertie)
dev.off()
