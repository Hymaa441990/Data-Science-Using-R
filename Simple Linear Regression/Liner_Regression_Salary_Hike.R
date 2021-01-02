library(ggpubr)

Salary_Data <- read.csv("C:/Hymaa/Data Science/Simple Linear Regression/Salary_Data.csv")
head(Salary_Data)
str(Salary_Data)

#Understanding Variables Graphically

#Scatter/ggplot plot - To visualize linear relationship between predictor and response

scatter.smooth(x=Salary_Data$YearsExperience, y=Salary_Data$Salary, main="Salary ~ Years of Experience")  # scatterplot

ggplot(Salary_Data, aes(x = YearsExperience, y = Salary)) + geom_point() +stat_smooth()

#Box plot - To spot outliers in data

par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(Salary_Data$Salary, main="Salary", sub=paste("Outlier rows: ", boxplot.stats(Salary_Data$Salary)$out))
boxplot(Salary_Data$YearsExperience, main="Years of experience", sub=paste("Outlier rows: ", boxplot.stats(Salary_Data$YearsExperience)$out))

#Density plot - To see distribution of the predictor variable - Check if the response variable is close to normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(Salary_Data$Salary), main="Density Plot: Salary", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Salary_Data$Salary), 2))) 
polygon(density(Salary_Data$Salary), col="red")

plot(density(Salary_Data$YearsExperience), main="Density Plot: Years of experience", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Salary_Data$YearsExperience), 2))) 
polygon(density(Salary_Data$YearsExperience), col="red")

#Correlation Between and X and y
cor(Salary_Data$YearsExperience, Salary_Data$Salary)

#Build Linear Model
linearMod <- lm(Salary ~ YearsExperience, data=Salary_Data)
linearMod


modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["YearsExperience", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["YearsExperience", "Std. Error"]  # get std.error for speed
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

