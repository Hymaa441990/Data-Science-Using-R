library(lattice)
library(ggpubr)

startups_data <- read.csv("C:/Hymaa/Data Science/Multi-Linear Regression/50_Startups.csv")
head(startups_data)
str(startups_data)

colnames(startups_data)[1] <- "RD_Spend"
colnames(startups_data)[3] <- "Marketing_Spend"

names(startups_data)

#Understanding Variables Graphically
ggplot(startups_data, aes(x = RD_Spend, y = Profit)) + geom_point() +stat_smooth()
ggplot(startups_data, aes(x = Administration, y = Profit)) + geom_point() +stat_smooth()
ggplot(startups_data, aes(x = Marketing_Spend, y = Profit)) + geom_point() +stat_smooth()
scatter.smooth(x=startups_data$State, y=startups_data$Profit, main="Weight ~ Calories")  # scatterplot

#Box plot - To spot outliers in data

par(mfrow=c(1, 3)) # divide graph area in 2 columns
boxplot(startups_data$RD_Spend, main="RD Spend", sub=paste("Outlier rows: ", boxplot.stats(startups_data$RD_Spend)$out))
boxplot(startups_data$Administration, main="Administration", sub=paste("Outlier rows: ", boxplot.stats(startups_data$Administration)$out))
boxplot(startups_data$Marketing_Spend, main="Marketing Spend", sub=paste("Outlier rows: ", boxplot.stats(startups_data$Marketing_Spend)$out))

#Density plot - To see distribution of the predictor variable

library(e1071)
par(mfrow=c(1, 3))  # divide graph area in 2 columns

plot(density(startups_data$RD_Spend), main="Density Plot: R.D. Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(startups_data$RD_Spend), 2))) 
polygon(density(startups_data$RD_Spend), col="red")

plot(density(startups_data$Administration), main="Density Plot: Administration", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(startups_data$Administration), 2))) 
polygon(density(startups_data$Administration), col="red")

plot(density(startups_data$Marketing_Spend), main="Density Plot: Marketing Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(startups_data$Marketing_Spend), 2))) 
polygon(density(startups_data$Marketing_Spend), col="red")


#Correlation Between and X and y
cor(startups_data$RD_Spend, startups_data$Profit)
cor(startups_data$Administration, startups_data$Profit)
cor(startups_data$Marketing_Spend, startups_data$Profit)
cor(startups_data[,-4])

startups_data_num <- startups_data[,-4]

startups_data_num <- data.frame(scale(startups_data_num))

#Scatter Plot Matrix :
pairs(startups_data)
pairs(startups_data_num)

#Correlation Matrix :
cor(startups_data_num)

#Regression Model and Its Summary
Profit_Model_all <- lm(Profit~.,data=startups_data)
summary(Profit_Model_all)
names(summary(Profit_Model_all))

rsquare_vals<-c()
adj_r_square_vals<-c()

rsquare_vals <- rbind(rsquare_vals,rall=summary(Profit_Model_all)$r.squared)
adj_r_square_vals <- rbind(adj_r_square_vals,all=summary(Profit_Model_all)$adj.r.squared)

Profit_Model_Exclu_State <- lm(Profit~.,data=startups_data_num)
rsquare_vals <- rbind(rsquare_vals,rExclu_State=summary(Profit_Model_Exclu_State)$r.squared )
adj_r_square_vals <- rbind(adj_r_square_vals,Exclu_State=summary(Profit_Model_Exclu_State)$adj.r.squared)

Profit_Model_Exclu_Admin <- lm(Profit~.,data=startups_data_num[,-2])
rsquare_vals <- rbind(rsquare_vals,rExclu_Admin=summary(Profit_Model_Exclu_Admin)$r.squared)
adj_r_square_vals <- rbind(adj_r_square_vals,Exclu_Admin=summary(Profit_Model_Exclu_Admin)$adj.r.squared)

Profit_Model_on_RD <- lm(Profit~.,data=startups_data_num[,c(-2,-3)])
rsquare_vals <- rbind(rsquare_vals,rOn_RandD=summary(Profit_Model_on_RD)$r.squared)
adj_r_square_vals <- rbind(adj_r_square_vals,On_RandD=summary(Profit_Model_on_RD)$adj.r.squared)

r2 <- as.data.frame(rsquare_vals)
adju_r2 <- as.data.frame(adj_r_square_vals)

#Accuary of different Models
accuracy <- cbind(r2,adju_r2)
names(accuracy)[1] <- "R2"
names(accuracy)[2] <- "Adjusted R2"
accuracy


library(car)
#Final Selected Model 94.83%
Profit_Model_Exclu_Admin <- lm(Profit~.,data=startups_data_num[,-2])

# Diagnostic plots:
# Residual plots, QQ - Plots, Std. Residual vs Fitted
plot(Profit_Model_Exclu_Admin)

# Residuals vs Regressors
residualPlots(Profit_Model_Exclu_Admin)

# Added variables plots
avPlots(Profit_Model_Exclu_Admin)

# QQ plots for studentized residuals
qqPlot(Profit_Model_Exclu_Admin)

# Deletion Diagnostics
influenceIndexPlot(Profit_Model_Exclu_Admin) # Index plots of the influence measures:
influence(Profit_Model_Exclu_Admin)



