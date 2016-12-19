#setup
setwd("~/Documents/Analyse")

wine<-read.csv("small_wine.csv", sep = ",")
wine <- wine[,2:13]
library(car)
library(ggplot2)
library(xtable)
library(pastecs)
library(Rcmdr)

#stat descriptives
statdescwine<-t(stat.desc(wine,T,T))
statdescwine<-statdescwine[,c("mean","std.dev","median","min","max")]

statdescwine <-apply(statdescwine,2,round, digits=3)
xtable(statdescwine)

corwine <- cor(wine[,c("alcohol","chlorides","citric.acid","density","fixed.acidity","free.sulfur.dioxide","pH","quality",
                      "residual.sugar","sulphates","total.sulfur.dioxide","volatile.acidity")], use="complete")

corspearmanwine <-cor(wine[,c("alcohol","chlorides","citric.acid","density","fixed.acidity","free.sulfur.dioxide","pH","quality","residual.sugar","sulphates",
            "total.sulfur.dioxide","volatile.acidity")], method="spearman", use="complete")
xtable(corspearmanwine)
#shapiro wilk test
shapiwine<-as.data.frame(sapply(wine,shapiro.test)[1:2,])


#qqplot
png(file="qqplot.png", width = 1600, height=1600,pointsize = 40)

myqqplots<-function(index) 
{
  qqPlot(wine[,index], main=names(wine[index]),ylab="sample quantile")

}
par(mfrow=c(3,4), oma=c(0,0,2,0))
sapply(1:12,FUN=myqqplots)
title(main="Quantile-quantile plot of normal distribution",outer=T)
dev.off()


#boxplot 
png(file="boxplot.png", width = 1600, height=1600,pointsize = 40)

myboxplots<-function(index) 
{
  boxplot(wine[,index], main=names(wine[index]))
  
}
par(mfrow=c(3,4),oma=c(0,0,2,0))
sapply(1:12,FUN=myboxplots)
title(main="Boxplots de toutes les variables",outer=T)
dev.off()

#plot des histogrammes en utilisant ggplot2

gg1<-ggplot(wine, aes(x=fixed.acidity)) + geom_histogram(bins=15, colour="black", fill="white") 
gg2<-ggplot(wine, aes(x=volatile.acidity)) + geom_histogram(bins=15, colour="black", fill="white")
gg3<- ggplot(wine, aes(x=citric.acid)) + geom_histogram(bins=15, colour="black", fill="white")
gg4<- ggplot(wine, aes(x=residual.sugar)) + geom_histogram(bins=15, colour="black", fill="white")
gg5<- ggplot(wine, aes(x=chlorides)) + geom_histogram(bins=15, colour="black", fill="white")
gg6<- ggplot(wine, aes(x=free.sulfur.dioxide)) + geom_histogram(bins=15, colour="black", fill="white")
gg7<- ggplot(wine, aes(x=total.sulfur.dioxide)) + geom_histogram(bins=15, colour="black", fill="white")
gg8<- ggplot(wine, aes(x=density)) + geom_histogram(bins=15, colour="black", fill="white")
gg9<- ggplot(wine, aes(x=pH)) + geom_histogram(bins=15, colour="black", fill="white")
gg10<- ggplot(wine, aes(x=sulphates)) + geom_histogram(bins=15, colour="black", fill="white")
gg11<- ggplot(wine, aes(x=alcohol)) + geom_histogram(bins=15, colour="black", fill="white")
gg12<- ggplot(wine, aes(x=quality)) + geom_histogram(bins=15, colour="black", fill="white")


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(gg1,gg2,gg3,gg4,gg5,gg6,gg7,gg8,gg9,gg10,gg11,gg12,cols=3)
savePlot("graph/histogram.png")
#scatterplotmatrix en utilisant rcmdr
png(file="scatterplot.png", width = 1600, height=1600,pointsize = 18)
winescater<-scatterplotMatrix(~alcohol+chlorides+citric.acid+density+fixed.acidity+free.sulfur.dioxide+pH+quality+residual.sugar+sulphates+total.sulfur.dioxide+volatile.acidity,
                  reg.line=lm, smooth=TRUE, spread=FALSE, span=0.5, ellipse=FALSE, 
                  levels=c(.5, .9), id.n=0, diagonal = 'density', data=wine, cex.labels=1.5)
dev.off()

#faire les boxplot et qqplot en ggplot2
#exporter les tables descriptives en latex