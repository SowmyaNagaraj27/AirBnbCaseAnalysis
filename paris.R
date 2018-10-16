paris<- read.csv("C:/Users/Sowmya/Desktop/fall2018/IDS515/AirBNB/M-0897X2 paris.csv")
View(paris)

boxplot(paris)

#outliers
boxOut=boxplot.stats(paris$savwish)$out
boxnoOut=ifelse(paris$savwish %in% boxOut, NA, paris$savwish)
boxplot(boxnoOut)
paris$savwish<-boxnoOut


boxplot(paris)
boxOut=boxplot.stats(paris$price)$out
boxnoOut=ifelse(paris$price %in% boxOut, NA, paris$price)
boxplot(boxnoOut)
paris$price<-boxnoOut

boxplot(paris)
boxOut=boxplot.stats(paris$reviews)$out
boxnoOut=ifelse(paris$reviews %in% boxOut, NA, paris$reviews)
boxplot(boxnoOut)
paris$reviews<-boxnoOut

#NA
sum(is.na(paris$rating))
paris$rating[is.na(paris$rating)] =mean(paris$rating, na.rm = TRUE)

sum(is.na(paris$sentiment))
paris$sentiment[is.na(paris$sentiment)] =mean(paris$sentiment, na.rm = TRUE)

sum(is.na(paris$uprice))
paris$uprice[is.na(paris$uprice)] =mean(paris$uprice, na.rm = TRUE)

#########################################Regression model#############################################
install.packages("MASS")
library(MASS)

parisModel<-lm(price~., data = paris)
summary(parisModel)


install.packages("car")
library(car)

outlierTest(parisModel)
vif(parisModel)

sqrt(vif(parisModel)) > 2

qqPlot(parisModel, main="QQ Plot")

install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(parisModel)

###################################################################
myFormula= price ~ sentiment

parisModel<-lm(myFormula, data = paris)
summary(parisModel)

paris<-paris[c(-1)]

paris1<-lm(sentiment~., data = paris)
summary(paris1)
