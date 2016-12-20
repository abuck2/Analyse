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

png("pca2.png",width = 1600, height=800,pointsize = 20)
par(mfcol=c(1,2))


plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black", col.quanti.sup="blue", 
         label=c("var", "quanti.sup"), lim.cos2.var=0)

plot.PCA(res, axes=c(3, 4), choix="var", new.plot=TRUE, col.var="black", col.quanti.sup="blue", 
         label=c("var", "quanti.sup"), lim.cos2.var=0)
dev.off()
png("pcaind.png",width = 1600, height=800,pointsize = 20)
par(mfcol=c(1,2))
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", col.ind.sup="blue", 
        col.quali="magenta", label=c("ind", "ind.sup", "quali"),new.plot=TRUE)
plot.PCA(res, axes=c(3, 4), choix="ind", habillage="none", col.ind="black", col.ind.sup="blue", 
        col.quali="magenta", label=c("ind", "ind.sup", "quali"),new.plot=TRUE)
dev.off()
summay + description + dimdesc 
summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")
res$eig
res$var$cor[,1:4]
res$ind
res$quanti.sup
dimdescwine<-dimdesc(res, axes=1:5)
remove(wine.PCA)
write.infile(res,file="pca2.txt", sep="\t")
write.infile(dimdescwine,file="pca2dimdesc.txt", sep="\t")
xtable(res$var$cor[,1:4], digits=3)
xtable(res$var$contrib[,1:4], digits=3)
#Selection des dimensions
res$eig[1:11,]#Valeurs propres
fviz_screeplot(res) ##Graphique des valeurs propres
sum(res$eig[,2]>100/11)#Valeurs propres > 1/nbvar
xtable(res$eig, digits=3)



