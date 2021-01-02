setwd("C:/Hymaa/Data Science/Logistic Regression/")
Bank_Data <- read.csv2("bank-full.csv")
head(Bank_Data)
str(Bank_Data)

nrow(Bank_Data)


nrow(Bank_Data[which(Bank_Data$y=="yes"),])
nrow(Bank_Data[which(Bank_Data$y=="no"),])

#Checking Class Bias of Dependent Variable
table(Bank_Data$y)

# Create Training Data
input_ones <- Bank_Data[which(Bank_Data$y == "yes"), ]  # all 1's
input_zeros <- Bank_Data[which(Bank_Data$y == "no"), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's

#ind <- sample(2,nrow(Bank_Data),replace=T,prob = c(0.8,0.2))
#trainingData <- Bank_Data[ind==1,]
#testData <- Bank_Data[ind==2,]

#Compute Information Values
#install.packages("InformationValue")
#install.packages("smbinning")
#install.packages("Information")

library(smbinning)
library(InformationValue)
library(Information)

# segregate continuous and factor variables
factor_vars <- c ("job", "marital", "education", "default", "housing", "loan", "contact", "month","poutcome")
continuous_vars <- c("age", "balance","day", "duration", "campaign", "pdays","previous")

vars_all <- names(trainingData[,-17])


#Calculate the Information Values
trainingData$y <- as.numeric(ifelse(trainingData$y=="yes",1,0))

final <- create_infotables(data=trainingData, y="y", bins=10, parallel=F)$Summary
IV <- create_infotables(data=trainingData, y="y", bins=10, parallel=F)$Summary$IV

n <- data.frame(ifelse(IV < 0.03,"Not Predictive",ifelse(IV < 0.1,"Somewhat Predictive","Highly Predictive")))
names(n) <- "Label"

final_IV <- cbind(final,n)



#Build Logit Models and Predict
formula_nn <- paste("y",paste(final_IV[which(final_IV$Label=="Highly Predictive"),1],collapse ="+"),sep="~")

logitMod <- glm(formula_nn, data=trainingData, family="binomial")
summary(logitMod)

View(testData)
predicted <- plogis(predict(logitMod, testData[,-17]))  # predicted scores
head(predicted)
# or
predicted <- predict(logitMod, testData[,-17], type="response")  # predicted scores
head(predicted)

# Model Accuracy
accuracy <- confusionMatrix(testData$y,predicted)
accuracy_percetage <- (sum(accuracy[[1,1]],accuracy[[2,2]])/nrow(testData))*100
accuracy_percetage

## ROC Curve
#Extract from the fitted model object the vector of fitted probabilities:

library(pROC)
roccurve <- roc(testData$y ~ predicted)
windows()
plot(roccurve)

auc <- auc(testData$y ~ predicted)
auc
