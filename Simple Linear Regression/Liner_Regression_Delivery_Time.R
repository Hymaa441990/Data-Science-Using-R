library(ggpubr)

Delivery_Data <- read.csv("C:/Hymaa/Data Science/Simple Linear Regression/delivery_time.csv")
head(Delivery_Data)
str(Delivery_Data)

colnames(Delivery_Data)[1] <- "Delivery_Time"
colnames(Delivery_Data)[2] <- "Sorting_Time"

names(Delivery_Data)

#Understanding Variables Graphically

#Scatter/ggplot plot - To visualize linear relationship between predictor and response

scatter.smooth(x=Delivery_Data$Sorting_Time, y=Delivery_Data$Delivery_Time, main="Delievry Time ~ Sorting Time")  # scatterplot

ggplot(Delivery_Data, aes(x = Sorting_Time, y = Delivery_Time)) + geom_point() +stat_smooth()

#Box plot - To spot outliers in data

par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(Delivery_Data$Delivery_Time, main="Delivery Time", sub=paste("Outlier rows: ", boxplot.stats(Delivery_Data$Delivery_Time)$out))
boxplot(Delivery_Data$Sorting_Time, main="Sorting Time", sub=paste("Outlier rows: ", boxplot.stats(Delivery_Data$Sorting_Time)$out))

#Density plot - To see distribution of the predictor variable - Check if the response variable is close to normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(Delivery_Data$Delivery_Time), main="Density Plot: Delivery Time", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Delivery_Data$Delivery_Time), 2))) 
polygon(density(Delivery_Data$Delivery_Time), col="red")

plot(density(Delivery_Data$Sorting_Time), main="Density Plot: Soting Time", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Delivery_Data$Sorting_Time), 2))) 
polygon(density(Delivery_Data$Sorting_Time), col="red")

#Correlation Between and X and y
cor(Delivery_Data$Sorting_Time, Delivery_Data$Delivery_Time)

#Build Linear Model
linearMod <- lm(Delivery_Time ~ Sorting_Time, data=Delivery_Data)
linearMod


modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Sorting_Time", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["Sorting_Time", "Std. Error"]  # get std.error for speed
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
trainingRowIndex <- sample(1:nrow(Delivery_Data), 0.8*nrow(Delivery_Data))  # row indices for training data
trainingData <- Delivery_Data[trainingRowIndex, ]  # model training data
testData  <- Delivery_Data[-trainingRowIndex, ]   # test data

#Develop the model on the training data and use it to predict the distance on test data
# Build the model on training data -
lmMod <- lm(Delivery_Time ~ Sorting_Time, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)
AIC (lmMod)

#Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$Delivery_Time, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 89.54%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 12.17%, mean absolute percentage deviation

#k- Fold Cross validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(data=Delivery_Data, form.lm=Delivery_Time ~ Sorting_Time, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 10.76101 mean squared error

attach(Delivery_Data)

# ---------------Exponential Model
plot(Sorting_Time, log(Delivery_Time))
cor(Sorting_Time, log(Delivery_Time))
reg_exp <- lm(log(Delivery_Time) ~ Sorting_Time)  #lm(log(Y) ~ X)
summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
caloriesConsumed <- exp(logat)

error = Sorting_Time - caloriesConsumed
error

sqrt(sum(error^2)/nrow(Delivery_Data))  #RMSE - 10.57475

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)
plot(Sorting_Time, Delivery_Time)
plot(Sorting_Time*Sorting_Time, Delivery_Time)

cor(Sorting_Time*Sorting_Time, Delivery_Time)

plot(Sorting_Time*Sorting_Time, log(Delivery_Time))

cor(Sorting_Time, log(Delivery_Time))
cor(Sorting_Time*Sorting_Time, log(Delivery_Time))

reg2degree <- lm(log(Delivery_Time) ~ Sorting_Time + I(Sorting_Time*Sorting_Time))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = Delivery_Data$Delivery_Time - expy

sqrt(sum(err^2)/nrow(Delivery_Data))  #RMSE - 2.799042

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = Delivery_Data, aes(x = Sorting_Time + I(Sorting_Time^2), y = log(Delivery_Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Delivery_Data, aes(x=Sorting_Time+I(Sorting_Time^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Delivery_Time)~Sorting_Time + I(Sorting_Time*Sorting_Time) + I(Sorting_Time*Sorting_Time*as.numeric(Sorting_Time)))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = Delivery_Data, aes(x = Sorting_Time + I(Sorting_Time^2) + I(Sorting_Time^3), y = Delivery_Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Delivery_Data, aes(x=Sorting_Time+I(Sorting_Time^2)+I(Sorting_Time^3), y=expy3))

