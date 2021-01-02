library(ggpubr)

Calories_Data <- read.csv("C:/Hymaa/Data Science/Simple Linear Regression/calories_consumed.csv")
head(Calories_Data)
str(Calories_Data)

colnames(Calories_Data)[1] <- "Weight_Gained"
colnames(Calories_Data)[2] <- "Calories_Consumed"

names(Calories_Data)

#Understanding Variables Graphically

#Scatter/ggplot plot - To visualize linear relationship between predictor and response

scatter.smooth(x=Calories_Data$Calories_Consumed, y=Calories_Data$Weight_Gained, main="Weight ~ Calories")  # scatterplot

ggplot(Calories_Data, aes(x = Calories_Consumed, y = Weight_Gained)) + geom_point() +stat_smooth()

#Box plot - To spot outliers in data

par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(Calories_Data$Weight_Gained, main="Weight Gained", sub=paste("Outlier rows: ", boxplot.stats(Calories_Data$Weight_Gained)$out))
boxplot(Calories_Data$Calories_Consumed, main="Calories Consumed", sub=paste("Outlier rows: ", boxplot.stats(Calories_Data$Calories_Consumed)$out))

#Density plot - To see distribution of the predictor variable - Check if the response variable is close to normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(Calories_Data$Weight_Gained), main="Density Plot: Weight Gained", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Calories_Data$Weight_Gained), 2))) 
polygon(density(Calories_Data$Weight_Gained), col="red")

plot(density(Calories_Data$Calories_Consumed), main="Density Plot: Calories Consumed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Calories_Data$Calories_Consumed), 2))) 
polygon(density(Calories_Data$Calories_Consumed), col="red")

#Correlation Between and X and y
cor(Calories_Data$Calories_Consumed, Calories_Data$Weight_Gained)

#Build Linear Model
linearMod <- lm(Weight_Gained ~ Calories_Consumed, data=Calories_Data)
linearMod

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Calories_Consumed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["Calories_Consumed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- modelSummary$fstatistic[1]  # fstatistic
f <- modelSummary$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
adjus_r2 <- modelSummary$adj.r.squared

#AIC and BIC
AIC(linearMod)
BIC(linearMod)

#Predicting Linear Models
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(Calories_Data), 0.8*nrow(Calories_Data))  # row indices for training data
trainingData <- Calories_Data[trainingRowIndex, ]  # model training data
testData  <- Calories_Data[-trainingRowIndex, ]   # test data

#Develop the model on the training data and use it to predict the distance on test data
# Build the model on training data -
lmMod <- lm(Weight_Gained ~ Calories_Consumed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)
AIC (lmMod)

#Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$Weight_Gained, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 64.46%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 45.47%, mean absolute percentage deviation

#k- Fold Cross validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=Calories_Data, form.lm=Weight_Gained ~ Calories_Consumed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error


attach(Calories_Data)

# ---------------Exponential Model
plot(Calories_Consumed, log(Weight_Gained))
cor(Calories_Consumed, log(Weight_Gained))
reg_exp <- lm(log(Weight_Gained) ~ Calories_Consumed)  #lm(log(Y) ~ X)
summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
caloriesConsumed <- exp(logat)

error = Calories_Consumed - caloriesConsumed
error

sqrt(sum(error^2)/nrow(Calories_Data))  #RMSE - 2033.1

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)
plot(Calories_Consumed, Weight_Gained)
plot(Calories_Consumed*Calories_Consumed, Weight_Gained)

cor(Calories_Consumed*Calories_Consumed, Weight_Gained)

plot(Calories_Consumed*Calories_Consumed, log(Weight_Gained))

cor(Calories_Consumed, log(Weight_Gained))
cor(Calories_Consumed*Calories_Consumed, log(Weight_Gained))

reg2degree <- lm(log(Weight_Gained) ~ Calories_Consumed + I(Calories_Consumed*Calories_Consumed))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = Calories_Data$Weight_Gained - expy

sqrt(sum(err^2)/nrow(Calories_Data))  #RMSE - 117.4145

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = Calories_Data, aes(x = Calories_Consumed + I(Calories_Consumed^2), y = log(Weight_Gained))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Calories_Data, aes(x=Calories_Consumed+I(Calories_Consumed^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Weight_Gained)~Calories_Consumed + I(Calories_Consumed*Calories_Consumed) + I(Calories_Consumed*Calories_Consumed*as.numeric(Calories_Consumed)))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = Calories_Data, aes(x = Calories_Consumed + I(Calories_Consumed^2) + I(Calories_Consumed^3), y = Weight_Gained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Calories_Data, aes(x=Calories_Consumed+I(Calories_Consumed^2)+I(Calories_Consumed^3), y=expy3))

