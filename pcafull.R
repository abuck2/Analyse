#library
library(FactoMineR)
library(Rcmdr)
#pca sur toutes les variables
wine.PCA<-wine[, c("fixed.acidity", "volatile.acidity", "citric.acid", 
                   "residual.sugar", "chlorides", "free.sulfur.dioxide", 
                   "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol", "quality")]
res<-PCA(wine.PCA , scale.unit=TRUE, ncp=5, graph = FALSE)
png("graph/pcafull.png",width = 1600, height=1600,pointsize = 20)
par(mfrow=c(2,2))
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE, title="Plot variables dimension 1 et 2")
plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black", 
         col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0, 
         title="Plot individus dimension 1 et 2")
plot.PCA(res, axes=c(3, 4), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE, title="Plot individus dimension 3 et 4")
plot.PCA(res, axes=c(3, 4), choix="var", new.plot=TRUE, col.var="black", 
         col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0, 
         title="Plot individus dimension 3 et 4")
dev.off()

summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")
res$eig$eigenvalue
res$var
res$ind
dimdesc(res, axes=1:5)
remove(wine.PCA)
write.infile(res,file="pcafull.txt", sep="\t")

