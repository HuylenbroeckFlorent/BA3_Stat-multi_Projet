require(ggplot2)
require(corrplot)
require(ade4)

require(cluster) #CLARA


## Univariate analysis.
summary(XXData)
head(XXData)
boxplot(XXData, main="Boxplots des données")

## ACP.

# boxplot centré réduit.
boxplot(scale(XXData, center=T, scale=T),main="Boxplot des données centrées et réduites")

# Visualisation de la matrice de corrélation.
pairs(XXData)
XXData.cor <- cor(XXData)
XXData.cor
corrplot(XXData.cor, type="upper")

# Diagonalisation de la matrice de corrélation et étude des valeurs propres.
XXData.eigen <- eigen(XXData.cor)
XXData.eigen$values
XXData.pca <- dudi.pca(XXData, scale=T, center=T, scannf=F, nf=4) #https://www.rdocumentation.org/packages/ade4/versions/1.7-15/topics/dudi.pca
screeplot(XXData.pca, type="bar", main="Inertie en fonction de la composante PCA") # Critere de Kaiser
plot(XXData.pca$eig/sum(XXData.pca$eig)*100, type='b',names.arg=c("V1","V2","V3","V4","V5","V6","V7","V8"), ylim=c(0,100), ylab="Pourcentage de la variance expliqué",xlab="Composante",main="Pourcentage de la variance expliqué par composante") # Critere du coude
for(i in 1:8){
  print(sum(XXData.pca$eig[1:i])/sum(XXData.pca$eig)*100)
}

score(XXData.pca)
XXData.pca$co
XXData.pca$c1
XXData.pca$eig
XXData.pca$li
plot(XXData.pca$li)
s.corcircle(XXData.pca$co)
inertia.dudi(XXData.pca, row.inertia=T)$row.abs
scatter(XXData.pca, posieig="none", clab.row=0)

XXData.pca2 <- prcomp(XXData, scale=T, center=T)
summary(XXData.pca2)
plot(XXData.pca2)
  

## Classification.
# Determine number of clusters
wss <- (nrow(XXData)-1)*sum(apply(XXData,2,var)) #https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/
for (i in 2:15) wss[i] <- sum(kmeans(XXData,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Nombre de clusters",
     ylab="Variance intra-groupe")  


XXData.k <- kmeans(XXData.pca2$x, 3)
str(XXData.k)
plot(XXData.pca2$x, col=XXData.k$clust, pch=16)

##CLARA
XXData.clara <- clara(XXData, 3, metric = "euclidean", stand = FALSE, samples = 5, pamLike = FALSE)
plot(XXData,col=XXData.clara$clustering)
print(XXData.clara)
