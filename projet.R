## setup
setwd("/home/alexis/Documents/stats/annee2/analyse/data/")
wine<-read.csv("winequality-red.csv", sep = ";")
library(FactoMineR)

## Echantillonage
set.seed(5)
index<-sample(dim(wine)[1], 200)
small_wine<-wine[index,]
dim(wine)
dim(small_wine)
write.csv(x = small_wine, file="small_wine.csv")
str(small_wine)

## creation des variables categorielles : 5 categories
cat5<-function(vector){ 
  size<-diff(range(vector))/5 #taille des catégories
  begin<-range(vector)[1] #début de la première categorie
  separations<-c(begin, begin+size, begin+2*size, begin+3*size, begin+4*size, begin+5*size) #Separation entre les categories
  return(cut(vector, separations)) #creation des categories
} 
small_wine$fixed.acidity<-cat5(small_wine$fixed.acidity)
small_wine$volatile.acidity<-cat5(small_wine$volatile.acidity)
small_wine$citric.acid<-cat5(small_wine$citric.acid)
small_wine$residual.sugar<-cat5(small_wine$residual.sugar)
small_wine$chlorides<-cat5(small_wine$chlorides)
str(small_wine)
# a transformer avec for loop sur 1:5 small_wine[,i]

#PCA
PCA(small_wine[,6:11])
