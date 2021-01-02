library(ggpubr)

Emp_Data <- read.csv("C:/Hymaa/Data Science/Simple Linear Regression/emp_data.csv")
head(Emp_Data)
str(Emp_Data)

#Understanding Variables Graphically

#Scatter/ggplot plot - To visualize linear relationship between predictor and response

scatter.smooth(x=Emp_Data$Salary_hike, y=Emp_Data$Churn_out_rate, main="Churn Out rate ~ Salary hike")  # scatterplot

ggplot(Emp_Data, aes(x = Salary_hike, y = Churn_out_rate)) + geom_point() +stat_smooth()

#Box plot - To spot outliers in data

par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(Emp_Data$Churn_out_rate, main="Churn Out rate", sub=paste("Outlier rows: ", boxplot.stats(Emp_Data$Churn_out_rate)$out))
boxplot(Emp_Data$Salary_hike, main="Salary hike", sub=paste("Outlier rows: ", boxplot.stats(Emp_Data$Salary_hike)$out))

#Density plot - To see distribution of the predictor variable - Check if the response variable is close to normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(Emp_Data$Churn_out_rate), main="Density Plot: Churn Out rate", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Emp_Data$Churn_out_rate), 2))) 
polygon(density(Emp_Data$Churn_out_rate), col="red")

plot(density(Emp_Data$Salary_hike), main="Density Plot: Soting Time", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Emp_Data$Salary_hike), 2))) 
polygon(density(Emp_Data$Salary_hike), col="red")

#Correlation Between and X and y
cor(Emp_Data$Salary_hike, Emp_Data$Churn_out_rate)

#Build Linear Model
linearMod <- lm(Churn_out_rate ~ Salary_hike, data=Emp_Data)
linearMod


modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Salary_hike", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["Salary_hike", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- modelSummary$fstatistic[1]  # fstatistic
f <- modelSummary$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
adjus_r2 <- modelSummary$adj.r.squared
r2 <- modelSummary$r.squared

#AIC and BIC
AIC(linearMod)
BIC(linearMod)

