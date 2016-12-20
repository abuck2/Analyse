## setup
setwd("/home/alexis/Documents/stats/annee2/analyse/projet/")
rm(list=ls())
source("projet.R")
source("pca_cluster_fda_continuous_var_only.R")
hclust(wine, method = "ward.D2")
HCPC


#clustering ... on peut test avec 2 ou 4 clusters
res.hcpc<-HCPC(res ,nb.clust=0,consol=F,min=3,max=10,graph=TRUE)
res.hcpc$data.clust[,ncol(res.hcpc$data.clust),drop=F]
res.hcpc$desc.var
res.hcpc$desc.axes
res.hcpc$desc.ind

winehcpc<- res.hcpc$data.clust

res.hcpc_c<-HCPC(res ,nb.clust=0,consol=F,min=3,max=10,graph=TRUE, method = "complete")
res.hcpc_c<-HCPC(res ,nb.clust=0,consol=F,min=3,max=10,graph=TRUE, method = "single")

