library(dplyr)
library(ggplot2)


data <- read.csv("2019.csv")
colnames(data)
unikalne <- sapply(data, function(x) length(unique(x)))
iloscNa <- sapply(data, function(x) sum(is.na(x)))
dlugosc <- sapply(data, function(x)length(x))
typ <- sapply(data, function(x) typeof(x[[1]]))

secik <- as.data.frame(cbind(typ, dlugosc, iloscNa, unikalne))
colnames(secik) <- c("Typ", "iloscWierszy", "iloscNA", "iloscUnikalnych")

sumara <- as.data.frame(summary(data))

dane <- c("Typ danych","Ilosc unikalnych", "Minimum", "Maksimum", "Median")

statisticsVectorScore <- c(typeof(data$Score[[1]]), length(unique(data$Score)), min(data$Score), max(data$Score), median(data$Score))
statsScore <- as.data.frame(cbind(dane, statisticsVectorScore))

statisticsVectorGDP.per.capita <- c(typeof(data$GDP.per.capita[[1]]), length(unique(data$GDP.per.capita)), min(data$GDP.per.capita), max(data$GDP.per.capita), median(data$GDP.per.capita))
statsGDP.per.capita <- as.data.frame(cbind(dane, statisticsVectorGDP.per.capita))

statisticsVectorSocial.support <- c(typeof(data$Social.support[[1]]), length(unique(data$Social.support)), min(data$Social.support), max(data$Social.support), median(data$Social.support))
statsSocial.support <- as.data.frame(cbind(dane, statisticsVectorSocial.support))

statisticsVectorHealthy.life.expectancy <- c(typeof(data$Healthy.life.expectancy[[1]]), length(unique(data$Healthy.life.expectancy)), min(data$Healthy.life.expectancy), max(data$Healthy.life.expectancy), median(data$Healthy.life.expectancy))
statsHealthy.life.expectancy <- as.data.frame(cbind(dane, statisticsVectorHealthy.life.expectancy))

statisticsVectorFreedom.to.make.life.choices <- c(typeof(data$Freedom.to.make.life.choices[[1]]), length(unique(data$Freedom.to.make.life.choices)), min(data$Freedom.to.make.life.choices), max(data$Freedom.to.make.life.choices), median(data$Freedom.to.make.life.choices))
statsFreedom.to.make.life.choices <- as.data.frame(cbind(dane, statisticsVectorFreedom.to.make.life.choices))

statisticsVectorGenerosity <- c(typeof(data$Generosity[[1]]), length(unique(data$Generosity)), min(data$Generosity), max(data$Generosity), median(data$Generosity))
statsGenerosity <- as.data.frame(cbind(dane, statisticsVectorGenerosity))

statisticsVectorPerceptions.of.corruption <- c(typeof(data$Perceptions.of.corruption[[1]]), length(unique(data$Perceptions.of.corruption)), min(data$Perceptions.of.corruption), max(data$Perceptions.of.corruption), median(data$Perceptions.of.corruption))
statsPerceptions.of.corruption <- as.data.frame(cbind(dane, statisticsVectorPerceptions.of.corruption))






