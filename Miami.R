rm(list = ls())
miami <- read.csv("C:/Users/Sowmya/Desktop/fall2018/IDS515/CanadianAirlines/M-0897X1 miami.csv")
View(miami)
head(miami)
miami<-miami[c(-1)]
col(miami)
class(miami$price)
miami$price<-as.numeric(miami$price)
boxplot(miami)

install.packages("magrittr")
library(magrittr)
#Outliers
boxplot(miami$savwish)
outmiamii=boxplot.stats(miami$savwish)$out
outmiamii
savNoOut=ifelse(miami$savwish %in% outmiamii, NA, miami$savwish)
boxplot(savNoOut)
miami$savwish<-savNoOut
boxplot(miami)

reviewOut=boxplot.stats(miami$reviews)$out
reviewNoout=ifelse(miami$reviews %in% reviewOut, NA, miami$reviews)
boxplot(reviewNoout)
miami$reviews<-reviewNoout
boxplot(miami$accommodates)
boxplot(miami)

accomodateOut=boxplot.stats(miami$accommodates)$out
accomodateNoOut=ifelse(miami$accommodates %in% accomodateOut, NA, miami$accommodates)
boxplot(accomodateNoOut)
miami$accommodates<-accomodateNoOut

boxplot(miami$beds)
bedaOut=boxplot.stats(miami$beds)$out
bedNoOut=ifelse(miami$beds %in% bedaOut, NA, miami$beds)
boxplot(bedNoOut)
miami$beds<-bedNoOut

boxplot.stats(miami$savwish)


#Missing values
sum(is.na(miami$reviews))
miami$reviews[is.na(miami$reviews)] =mean(miami$reviews, na.rm = TRUE)

sum(is.na(miami$rating))
miami$rating[is.na(miami$rating)] =mean(miami$rating, na.rm = TRUE)


sum(is.na(miami$accommodates))
miami$accommodates[is.na(miami$accommodates)] =mean(miami$accommodates, na.rm = TRUE)

sum(is.na(miami$savwish))
miami$savwish[is.na(miami$savwish)] =mean(miami$savwish, na.rm = TRUE)

sum(is.na(miami$sentiment))
miami$sentiment[is.na(miami$sentiment)] =mean(miami$sentiment, na.rm = TRUE)

sum(is.na(miami$beds))
miami$beds[is.na(miami$beds)] =mean(miami$beds, na.rm = TRUE)

#linear regression
install.packages("MASS")
library(MASS)
#pairs(miami)

result1<-lm(sentiment ~ ., data = miami)
summary(result1)
