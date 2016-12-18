## setup
setwd("/home/alexis/Documents/stats/annee2/analyse/projet/")
rm(list=ls())
source("projet.R")


## creation des variables categorielles : 5 categories pour alcool, citric acid, quality, residual.sugar,free.sulfur
cat5<-function(vector){ 
  size<-diff(range(vector))/5 #taille des catégories
  begin<-range(vector)[1] #début de la première categorie
  separations<-c(begin, begin+size, begin+2*size, begin+3*size, begin+4*size, begin+5*size) #Separation entre les categories
  return(cut(vector, separations)) #creation des categories
} 

wine$alcohol<-cat5(wine$alcohol)
wine$citric.acid<-cat5(wine$citric.acid)
wine$residual.sugar<-cat5(wine$residual.sugar)
wine$free.sulfur.dioxide<-cat5(wine$free.sulfur.dioxide)
wine$quality<-as.factor(wine$quality)
str(wine)
wine_pca<-wine[,c(3,4,6,11,12)]
str(wine_pca)

#MCA
MCA(wine_pca) #MCA : Variables
mc<-MCA(wine_pca, graph = F)
mc
summary(mc)

mc$eig[1:10,] #Valeurs propres
fviz_screeplot(mc, ncp=15) ##Graphique des valeurs propres qui descendent trop lentement
plot(mc) #Biplot des variables et individus
fviz_mca_biplot(mc) #Similaire mais en plus beau
