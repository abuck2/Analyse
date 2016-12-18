#Transformation en logarithme des variables non normales

wine$log_alcohol <- with(wine, log(alcohol))
wine$log_fixed.acidity <- with(wine, log(fixed.acidity))
wine$log_free.sulfur.dioxide <- with(wine, log(free.sulfur.dioxide))
wine$log_sulphates <- with(wine, log(sulphates))
wine$log_total.sulphur.dioxide <- with(wine, log(total.sulfur.dioxide))
wine$log_residual.sugar <- with(wine, log(residual.sugar))
wine$log_citric.acid <- with(wine, log(citric.acid))
#subset de log_citric acid pour enlever les -inf du au log
wine$log_citric.acid[wine$citric.acid==0] -< 0


gg1<-ggplot(wine, aes(x=log_alcohol)) + geom_histogram(bins=15, colour="black", fill="white") 
gg2<-ggplot(wine, aes(x=log_fixed.acidity)) + geom_histogram(bins=15, colour="black", fill="white")
gg3<- ggplot(wine, aes(x=log_free.sulfur.dioxide)) + geom_histogram(bins=15, colour="black", fill="white")
gg4<- ggplot(wine, aes(x=log_sulphates)) + geom_histogram(bins=15, colour="black", fill="white")
gg5<- ggplot(wine, aes(x=log_total.sulphur.dioxide)) + geom_histogram(bins=15, colour="black", fill="white")
gg6<- ggplot(wine, aes(x=log_residual.sugar)) + geom_histogram(bins=15, colour="black", fill="white")
gg7<- ggplot(wine, aes(x=log_citric.acid)) + geom_histogram(bins=15, colour="black", fill="white")

multiplot(gg1,gg2,gg3,gg4,gg5,gg6,gg7,cols=3)

#seuls sulfur.dioxide+sulphates sont normaux