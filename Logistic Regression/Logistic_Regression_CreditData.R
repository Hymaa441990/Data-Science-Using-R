setwd("C:/Hymaa/Data Science/Logistic Regression/")
Credit_Data <- read.csv("creditcard.csv")
Credit_Data <- Credit_Data[,-1]

head(Credit_Data)
str(Credit_Data)

nrow(Credit_Data)


nrow(Credit_Data[which(Credit_Data$card=="yes"),])
nrow(Credit_Data[which(Credit_Data$card=="no"),])

#Checking Class Bias of Dependent Variable
table(Credit_Data$card)

# Create Training Data
input_ones <- Credit_Data[which(Credit_Data$card == "yes"), ]  # all 1's
input_zeros <- Credit_Data[which(Credit_Data$card == "no"), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's

#Compute Information Values
#install.packages("InformationValue")
#install.packages("smbinning")
#install.packages("Information")

library(smbinning)
library(InformationValue)
library(Information)

# segregate continuous and factor variables
factor_vars <- c ("owner", "selfemp")
continuous_vars <- c("reports", "age","income", "share", "expenditure", "dependents","months","majorcards","active")

vars_all <- names(trainingData[,-1])


#Calculate the Information Values
trainingData$card <- as.numeric(ifelse(trainingData$card=="yes",1,0))

final <- create_infotables(data=trainingData, y="card", bins=10, parallel=F)$Summary
IV <- create_infotables(data=trainingData, y="card", bins=10, parallel=F)$Summary$IV

n <- data.frame(ifelse(IV < 0.03,"Not Predictive",ifelse(IV < 0.1,"Somewhat Predictive","Highly Predictive")))
names(n) <- "Label"

final_IV <- cbind(final,n)
final_IV


#Build Logit Models and Predict
formula_nn <- paste("card",paste(final_IV[which(final_IV$Label=="Highly Predictive"),1],collapse ="+"),sep="~")

summary(trainingData$expenditure)
k <- log(trainingData$expenditure)
trainingData$expenditure <- ifelse(k == "-Inf",0,k)

summary(trainingData$active)
l <- log(trainingData$active)
trainingData$active <- ifelse(l == "-Inf",0,l)

summary(trainingData$reports)
m <- log(trainingData$reports)
trainingData$reports <- ifelse(m == "-Inf",0,m)

logitMod <- glm(formula_nn, data=trainingData, family = binomial("logit"), maxit = 100)
summary(logitMod)


predicted <- plogis(predict(logitMod, testData[,-1]))  # predicted scores
head(predicted)
# or
predicted <- predict(logitMod, testData[,-1], type="response")  # predicted scores
head(predicted)

# Model Accuracy
accuracy <- confusionMatrix(testData$card,predicted)
accuracy_percetage <- (sum(accuracy[[1,1]],accuracy[[2,2]])/nrow(testData))*100
accuracy_percetage

## ROC Curve
#Extract from the fitted model object the vector of fitted probabilities:

library(pROC)
roccurve <- roc(testData$card ~ predicted)
windows()
plot(roccurve)

auc <- auc(testData$card ~ predicted)
auc
