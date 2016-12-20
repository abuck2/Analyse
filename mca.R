## setup
setwd("/home/alexis/Documents/stats/annee2/analyse/projet/")
rm(list=ls())
source("projet.R")


## creation des variables categorielles : 5 categories pour alcool, citric acid, quality, residual.sugar,free.sulfur
cat5<-function(vector){  
  separations<-c(quantile(vector,seq(0,1,.25)))  #Separations entre les catégories 
  return(cut(vector, breaks = separations,   #Creation des categories
             labels=c("low","middle low","middle high","high"),  #Noms des catégories
             ordered_result = T)) #Variable ordinal
}

wine$alcohol<-cat5(wine$alcohol)
wine$citric.acid<-cat5(wine$citric.acid)
wine$residual.sugar<-cat5(wine$residual.sugar)
wine$free.sulfur.dioxide<-cat5(wine$free.sulfur.dioxide)
wine$quality<-as.factor(wine$quality)
str(wine)
wine_pca<-wine[,c(3,4,6,11,12)]
str(wine_pca)

##MCA
MCA(wine_pca) #MCA : Variables
mc<-MCA(wine_pca, graph = F)
mc
summary(mc)

#Selection des dimensions
mc$eig[1:11,]#Valeurs propres
fviz_screeplot(mc, ncp=21) ##Graphique des valeurs propres qui descendent trop lentement
sum(mc$eig[,2]>100/21)#pourcentage de variance > 100/dim

#Visualisation des variables et individus dans le plan formé par les deux premières dimensions
cat <- get_mca_var(mc)
categories <- rownames(cat$coord)
categories
plot(mc) #Biplot des variables et individus
fviz_mca_biplot(mc) #Similaire mais en plus beau
fviz_mca_var(mc) #graphiques reprenant uniquement les catégories
fviz_mca_ind(mc) #graphiques reprenant uniquement les individus

plot(mc, choix = "var")#visualisation des variable

##contributions des categories aux différentes dimensions
round(var$coord, 2)
corrplot(var$contrib, is.corr = FALSE)
fviz_contrib(mc, choice = "var", axes = 1)
fviz_contrib(mc, choice = "var", axes = 2, top = 10)

