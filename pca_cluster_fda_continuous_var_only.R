#pca sur toutes les variables sauf quality, et en utilisant quality comme var "supplémentaire"

wine<-read.csv("small_wine.csv", sep = ",")
wine <- wine[,2:13]
library(FactoMineR)
library(car)
library(ggplot2)
library(Rcmdr)


wine.PCA<-wine[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", 
                   "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol", "quality")]
res<-PCA(wine.PCA , scale.unit=TRUE, ncp=5, quanti.sup=c(12: 12), graph = FALSE)

png("graph/pca2.png",width = 1600, height=1600,pointsize = 20)
par(mfrow=c(2,2))

plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", col.ind.sup="blue", 
         col.quali="magenta", label=c("ind", "ind.sup", "quali"),new.plot=TRUE)
plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black", col.quanti.sup="blue", 
         label=c("var", "quanti.sup"), lim.cos2.var=0)
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", col.ind.sup="blue", 
         col.quali="magenta", label=c("ind", "ind.sup", "quali"),new.plot=TRUE)
plot.PCA(res, axes=c(3, 4), choix="var", new.plot=TRUE, col.var="black", col.quanti.sup="blue", 
         label=c("var", "quanti.sup"), lim.cos2.var=0)
dev.off()

#summay + description + dimdesc 
summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")
res$eig
res$var
res$ind
res$quanti.sup
dimdescwine<-dimdesc(res, axes=1:5)
remove(wine.PCA)
write.infile(res,file="pca2.txt", sep="\t")
write.infile(dimdescwine,file="pca2dimdesc.txt", sep="\t")

#Selection des dimensions
res$eig[1:11,]#Valeurs propres
fviz_screeplot(res) ##Graphique des valeurs propres qui descendent trop lentement
sum(res$eig[,2]>100/11)#Valeurs propres > 1/nbvar


#clustering ... on peut test avec 2 ou 4 clusters
res.hcpc<-HCPC(res ,nb.clust=0,consol=F,min=3,max=10,graph=TRUE)
res.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]
res.hcpc$desc.var
res.hcpc$desc.axes
res.hcpc$desc.ind


winehcpc<- res.hcpc$data.clust

#FDA en utilisant le code de cedric taverne ... permet de discriminer les 2 groupes
res2<-FDA(winehcpc[,1:11],groups = winehcpc$clust )
