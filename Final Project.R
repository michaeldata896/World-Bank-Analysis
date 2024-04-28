---
title: Final Project
author: Michael and Timmy
date: May 7, 2019
output:
  html_document: default
---

Background and Motivation:

Our goal for this analysis is to determine the relationship between several pairs of variables from the 2016 World Bank dataset on country data. Specifically, we seek to determine if there are relationships between fertility and GNI, Life expectancy and CO2 emissions, Export Ratio and Life Expectancy, and Deforestation and CO2. Overall, we believe these combinations of variables will help us answer pressing and relevant questions about our changing world. Whether this be through determining an unlikely relationship, or finding an outcome that challenges a misconception, we wanted to use our data to accrue a real understanding of the world in this day and age. 

DATA VARIABLES:
Country - Name of Country
Fertility16 - Fertility rate of a certain country in 2016
GNI - Gross national income per capita in US dollars 
LifeExp - Life expectancy in years
CO2 - Carbon Dioxide emissions, metric tons per capita
NetExport - Percentage of GDP made up by net exports 
NetForest - Change in forest land area in sq kms between 1994 and 2014
HDI - Human Development Index

```{r setup, include=FALSE}
rm(list = ls()) 
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
WBData <- read.csv("/Users/michaelchau/Downloads/WB.2016.csv")
WBData2 <- WBData[,c("Country", "Fertility16", "GNI", "LifeExp", "CO2", "Exports", "Imports", "Forest14", "Forest94")]
dim(WBData2)
names(WBData2)
attach(WBData2)
```

#Creating Variables we need

```{r}
WBData2$NetExport <- Exports - Imports
WBData2$NetForest <- Forest14 - Forest94
WBData2$Sum <- (LifeExp *.6) + (GNI *.4)
WBData2$HDI <- NA
WBData2$HDI[WBData2$Sum > 0] <- "Low HDI"
WBData2$HDI[WBData2$Sum > 600] <- "Medium HDI"
WBData2$HDI[WBData2$Sum > 1670] <- "High HDI"
WBData2$HDI[WBData2$Sum > 3000] <- "Very High HDI"
```

#Data Cleaning

```{r}
WBData2 <- na.omit(WBData2)
WBData2[,c(-1,-13)] <- round(WBData2[,c(-1,-13)], 2)
```

Our data cleaning process consisted of manipulating variables into more adequate ones for the hypotheses we wished to test. We first created a new data set (WBData2) that contained only the variables we wished to analyze. We next had to establish variables that were unique to our purposes. We first created a variable NetExport, exports minus imports. This, and its components, are expressed as a percentage of GDP. The intuition behind combining them was that net exports, not its components, is a factor of the economic GDP equation ( GDP = Consumption + Investment + Gov Spending + Net Exports). The original data had several countries with 70 or 80 percent values in both components columns. This was deceiving as if a country that has 70% in both columns it means that zero percent of its GDP is made up by net exports. The next variable we created was NetForest, which was the difference in sq kilometers of forest between 2014 and 1994, which was useful to compare to CO2 emissions, as forests sequester carbon. Finally we created a variable for HDI, which was calculated using information from the United Nations Development Programme (UNDP). After creating new variables we omitted NAs and rounded all continuous variables. 

#Fertility and HDI

```{r}
boxplot(WBData2$Fertility16 ~ WBData2$HDI, col = 'yellow', main = 'Fertility Rate by HDI')
means <- tapply(WBData2$Fertility16, WBData2$HDI, mean)
points(means, col = "red", pch = 19, cex = 1.2)
text(x=c(1:4), y=means+.4, labels = round(means,2))

sds <- tapply(WBData2$Fertility16, WBData2$HDI, sd)
round(max(sds)/min(sds),1)
aov1 <- aov(WBData2$Fertility16 ~ WBData2$HDI)
summary.aov(aov1)

TukeyHSD(aov1)
par(mar=c(5,15,4,2))
plot(TukeyHSD(aov1), las = 1)
```

Our first analysis was between fertility rate in 2016 and our previously created categorical HDI variable. We found lower HDI groups to have higher mean fertility rates than higher HDI groups, Implying fertility falls with development. Our tukey test, t-tests which correct for family wise error rate, found that there was a statistically significant difference in mean fertility between all pairs (Low HDI and High HDI, etc.)

#CO2 and NET FOREST

```{r}
library(car)
source("http://www.reuningscherer.net/s&ds230/Rfuncs/regJDRS.txt")

plot(log(WBData2$CO2) ~ WBData2$NetForest, col="red", xlab = "Net Forest", ylab = "CO2")
mtext(paste("Sample Correlation =", round(cor(log(WBData2$CO2),WBData2$NetForest),3)), cex=1.2)
mtext("Net Forest vs CO2", cex=1.2, line = 1)
lm1 <- lm(log(WBData2$CO2) ~ WBData2$NetForest)
summary(lm1)
myResPlots2(lm1)
```
![Data from Professor Mendelsohn](/Users/michaelchau/Downloads/CO2.Chart-2.jpg)
We next ran a linear regression of CO2 predicted by NetForest, our change in forest area between 2014 and 1994. We found there was a negligible regression. It is a widely held misconception that the forestry industry and deforestation are a contributor to CO2 emissions. While it is true that forests sequester carbon and cutting them down releases the gas, we found a very small regression between the two variables. We believe this is because of a change the forestry industry went through in the 1990s when its switched from cutting down old growth forests to cutting down managed, renewable forests. Managed forests are able to sequester much more carbon as there are less gaps in the tree cover. Thus, less land area is needed to hold the same amount of carbon. So, better managed forests are able to take carbon out of the atmosphere. In fact, it is estimated by Yale professor of economics Robert Mendelsohn that the forestry industry has contributed a net negative, or an abatement, to greenhouse gas pollution. 

#NET EXPORT and GNI

```{r}
options(scipen=999)

myCor <- function(x,y){
  plot(x,y,pch=19, col="red", xlab = "GNI", ylab = "NetExport")
  mtext(paste("Sample Correlation =", round(cor(x,y),3)), cex=1.2)
}
x <- WBData2$GNI
y <- WBData2$NetExport
myCor(x,y)
mtext("NetExport vs GNI", cex=1.2, line = 1)

options(scipen=999)
permCor <- function(x, y, n_samp = 10000, plot = T){
   corResults <- rep(NA, n_samp)
   for (i in 1:n_samp){
      corResults[i] <- cor(x, sample(y))
   }
   pval <- mean(abs(corResults) >= abs(cor(x,y)))
   if (plot == T){
      #Make histogram of permuted correlations
      hist(corResults, col = "yellow", main = "", xlab = "Correlations", breaks = 50,
           xlim = range(corResults,cor(x,y)))
      mtext("Permuted Sample Correlations", cex = 1.2, line = 1)
      mtext(paste("Permuted P-value =",round(pval,5)), cex = 1, line = 0)
      abline(v = cor(x,y), col="blue", lwd=3)
      text(cor(x,y)-.03, 0,paste("Actual Correlation =", round(cor(x,y),2)),srt = 90, adj = 0)
   }
   if (plot == F){
      return(round(pval,5))
   }  
}
permCor(x,y)
cor.test(x,y)
```

#Bootstrap Correlation for Net Export and GNI

```{r}
cor1test <- cor.test(WBData2$NetExport, WBData2$GNI)$conf.int

#get number of rows in dataset
N <- nrow(WBData2)

#Specify how many boostrap samples to take
n_samp <- 1000

corResults <- rep(NA, n_samp)

for(i in 1:n_samp){
  #get vector of rows in our fake sample
  s <- sample(1:N, N , replace=T)
  
  #Calculate unique rows in our sample
  sVals <- as.numeric(names(table(s)))
  
  #Calculate how many times each row shows up
  sCounts <-  as.vector(table(s))
  
  #Get bootstrapped GNI and NetExport values
  fakeGNI <-  rep(WBData2$GNI[sVals], sCounts)
  fakeNetExport <-  rep(WBData2$NetExport[sVals], sCounts)
  
  #Get bootstrapped correlation and regression slope
  corResults[i] <- cor(fakeGNI, fakeNetExport)
}
(ci_r <- quantile(corResults, c(.025, .975)))

hist(corResults, col = "blue", main = "Bootstrapped Correlations", xlab = "Sample Correlation", breaks = 50)
abline(v = ci_r, lwd = 3, col = "red")
abline(v = cor1test, lwd = 3, col = "green", lty = 2)
legend("topleft", c("Theoretical CI","Boot CI"), lwd=3, col = c("green","red"), lty = c(2,1))
```

Furthermore, we also decided to examine the relationship between GNI and NetExport, which is the difference between the variables export and import. This topic has recently received much more attention from our trade war with China. It is commonly believed that a positive net export translates to an overall stronger economy. Looking at our results, there does appear to be a moderately strong correlation between GNI and NetExport without any significant outliers in the correlation plot. Our permutation test gives us a permuted p-value of 0.000 so we can conclude that there is a statistically significant non-zero correlation between GNI and NetExport. Looking at our bootstrapped correlation plot, it does appear that the gap between the left side theoretical CI and Boot CI is wider than the right. This can be attributed to several minor outliers on the left side of the correlation plot. However, we think that these minor outliers are not significant enough to remove from the correlation because they play an important role in the overall data.  

#MULTIPLE REGRESSION

```{r}
GNIpred <- lm(WBData2$GNI ~ WBData2$LifeExp + WBData2$NetExport + WBData2$NetForest + WBData2$Fertility16 + WBData2$CO2)

#Get summary information for model
summary(GNIpred)

#Run a Box Cox procedure on model 
boxCox(GNIpred)

#Transform regression according to box-cox
LogGNIpred <- lm(log(WBData2$GNI) ~ WBData2$LifeExp + WBData2$NetExport + WBData2$NetForest + WBData2$Fertility16 + WBData2$CO2)
summary(LogGNIpred)

#Backwards stepwise regression
LogGNIpred2 <- lm(log(WBData2$GNI) ~ WBData2$LifeExp + WBData2$NetExport + WBData2$Fertility16 + WBData2$CO2)
summary(LogGNIpred2)

#Next Step
LogGNIpred3 <- lm(log(WBData2$GNI) ~ WBData2$LifeExp + WBData2$NetExport + WBData2$CO2)
summary(LogGNIpred3)

#Get residual plots
myResPlots2(LogGNIpred3)
```

Our multiple regression analysis predicted the natural log of GNI per capita based on the majority of the variables in our data set. We decided to omit our HDI variable for this regression as GNI itself was a component of that variable. Our backwards, stepwise regression showed that life expectancy, net exports, and CO2 emissions were all significant predictors of GNI. Each of these predictors has a positive coefficient, meaning that as the value of each predictor increased, so did the mean of logGNI. We were surprised to see that Fertility was not a significant predictor of GNI, especially after we found it to be related to our HDI variable, of which GNI was a component. This implies that the relation may have been due mostly to Life Expectancy, the other part of our HDI variable. Overall, the model had a high adjusted r^2 value of .826, meaning that ~83% of the variance in GNI can be explained by this model. 

Conclusion and Summary:

Overall, we were able to determine that there are differences in mean fertility rates by HDI using ANOVA and tukey. Next, we discover that there was not a significant regression for CO2 predicted by NetForest. This is despite the fact that there is a widely held belief that deforestation is a large contributor to CO2 emissions. We believe that other factors such as increased usage of managed forests, which sequester more carbon, may account for this. In our analysis of NetExport and GNI, we determined that was a moderately strong correlation between the two variables. To further investigate this correlation, we ran a permutation test, where we were able to determine that there is a statistically significant non-zero correlation between GNI and NetExport. Finally, in our multiple regression model, using backwards, stepwise regression, we observed that LifeExp, NetExport, and CO2 emissions were all statistically significant predictors of GNI.

Our original intention in examining this dataset was to analyze and answer relevant questions that have great significance in society. Overall, we believe that we were able to accomplish this goal and discover some surprising conclusions along the way. We hope that this analysis can inspire further dialogue and examination into this topic with real-world implications.
