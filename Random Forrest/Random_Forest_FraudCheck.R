#install.packages("caret", dependencies = TRUE)
#install.packages("randomForest")

library(caret)
library(randomForest)

#Set Path
setwd("C:/Hymaa/Data Science/Random Forrest/")

#Read Data
Fraud_check <- read.csv("Fraud_check.csv")

#See Features
head(Fraud_check)

#Convert target variable Sales into categorical variable
Fraud_check$Taxable_Income <- ifelse(Fraud_check$Taxable.Income <= 30000, 0,1)
Fraud_check <- Fraud_check[,-3]
head(Fraud_check)

#Picking the best Features
table(Fraud_check[,c("Taxable_Income","Undergrad")]) #Keep
table(Fraud_check[,c("Taxable_Income","Marital.Status")]) #Keep
table(Fraud_check[,c("Taxable_Income","Urban")]) #Keep

#Checking distribution of each continuous variable
#install.packages("fields")
library(fields)

bplot.xy(Fraud_check$Taxable_Income, Fraud_check$City.Population) #Keep
summary(Fraud_check$City.Population)

bplot.xy(Fraud_check$Taxable_Income, Fraud_check$Work.Experience) #Keep
summary(Fraud_check$Work.Experience)


# Converting 'High Sales' to a factor
Fraud_check$Taxable_Income <- factor(Fraud_check$Taxable_Income)

# Set a random seed
set.seed(12345)

#Creating Training and Test data sets
inTrainingLocal <- createDataPartition(Fraud_check$Taxable_Income,p=.75,list = F) # 75% data is training dataset
Fraud_check_train <- Fraud_check[inTrainingLocal,]
Fraud_check_test <- Fraud_check[-inTrainingLocal,]

# Training using 'random forest' algorithm
model <- train(Taxable_Income ~ .,
               data = Fraud_check_train,
               method = 'rf',
               trControl = trainControl(method = 'cv',number = 5))
model

set.seed(51)

model1 <- randomForest(as.factor(Fraud_check_train$Taxable_Income)~.,data=Fraud_check_train,ntree=1000)
model1


#Check if any missing values in test data before predection
summary(Fraud_check_test)

#Importance of variable - Lower Gini
print(importance(model1))

head(Fraud_check_test)

#Prediction - With Model1
pred <- predict(model, newdata = Fraud_check_test[,-6])
confusionMatrix(Fraud_check_test$Taxable_Income,pred)

#Prediction - With Model2
pred1 <- predict(model1, newdata = Fraud_check_test[,-6])
confusionMatrix(Fraud_check_test$Taxable_Income,pred1)

