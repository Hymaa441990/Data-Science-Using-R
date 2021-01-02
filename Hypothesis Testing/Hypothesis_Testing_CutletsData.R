#-----------Assumptions---------------------------------
#Null Hypothesis - Both units diameters are same
#Alternative Hypothesis - Both units diameters not same
#-------------------------------------------------------

library(dplyr)
library(ggplot2)

Cutlets <- read.csv("C:/Hymaa/Data Science/Hypothesis Testing/Cutlets.csv")
head(Cutlets)
str(Cutlets)

# Rename variables
Cutlets <- Cutlets %>% rename(UnitA=Unit.A,UnitB=Unit.B)
names(Cutlets)

#Understanding Data Through Visualization
ggplot(Cutlets, aes(x = UnitA, y = UnitB)) + geom_point() +stat_smooth()

#Finding Outliers
par(mfrow=c(1, 2))
boxplot(Cutlets$UnitA,main="Unit A",sub=paste("Outliers are : ",boxplot.stats(Cutlets$UnitA)$out))
boxplot(Cutlets$UnitB,main="Unit B",sub=paste("Outliers are : ",boxplot.stats(Cutlets$UnitB)$out))

summary(Cutlets$UnitA)
details <- summary(Cutlets$UnitB)

#Impute outliers
out <- boxplot.stats(Cutlets$UnitB)$out
out_lst <- Cutlets$UnitB == out
impute_mean<-mean(Cutlets$UnitB[-out_lst])
Cutlets$UnitB[which(Cutlets$UnitB == out)] <- impute_mean

#Data Distribution
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(Cutlets$UnitA), main="Density Plot: Unit A", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Cutlets$UnitA), 2))) 
polygon(density(Cutlets$UnitA), col="red")

plot(density(Cutlets$UnitB), main="Density Plot: Unit B", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Cutlets$UnitB), 2))) 
polygon(density(Cutlets$UnitB), col="red")

Dstribution <- function(v)
{
  if (between(v,-0.5,0.5) == T) print("Distribution : Approximately Symmetric") 
  else if (between(v,-1,-0.5) == T) print("Distribution : Moderatly Skewed") 
  else if (between(v,-1,-0.5) == T) print("Distribution : Highly Skewed") 
}

Dstribution(round(e1071::skewness(Cutlets$UnitA), 2))
Dstribution(round(e1071::skewness(Cutlets$UnitB), 2))


#Testing differences in means

X = rnorm(Cutlets$UnitA)
Y = rnorm(Cutlets$UnitB)
Cutlets.t.test <- t.test(X,Y)

Cutlets.t.test$p.value
Cutlets.t.test$conf.int

#---------------------Test Result------------------------------------------------
# As P -Value 0.69 > 0.05, reserach rejected null Hypothesis.
# Which indiactes there is significant difference between 2 units cutlet diameters
#---------------------------------------------------------------------------------
