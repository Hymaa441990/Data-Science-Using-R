#install.packages("caret", dependencies = TRUE)
#install.packages("randomForest")

library(caret)
library(randomForest)

#Set Path
setwd("C:/Hymaa/Data Science/Random Forrest/")

#Read Data
company_data <- read.csv("Company_Data.csv")

#See Features
head(company_data)

#Convert target variable Sales into categorical variable
company_data$High_Sales <- ifelse(company_data$Sales < 8, 0,1)
company_data <- company_data[,-1]
head(company_data)

#Picking the best Features
table(company_data[,c("High_Sales","Urban")]) #Keep
table(company_data[,c("High_Sales","US")]) #Keep
table(company_data[,c("High_Sales","ShelveLoc")]) #Keep

#Checking distribution of each continuous variable
#install.packages("fields")
library(fields)

bplot.xy(company_data$High_Sales, company_data$CompPrice) #Keep
summary(company_data$CompPrice)

bplot.xy(company_data$High_Sales, company_data$Income) #Keep
summary(company_data$Income)

bplot.xy(company_data$High_Sales, company_data$Advertising) #Keep
summary(company_data$Advertising)

bplot.xy(company_data$High_Sales, company_data$Population)
summary(company_data$Population)

bplot.xy(company_data$High_Sales, company_data$Price) #Keep
summary(company_data$Price)

bplot.xy(company_data$High_Sales, company_data$Age) #Keep
summary(company_data$Age)

bplot.xy(company_data$High_Sales, company_data$Education) #Keep
summary(company_data$Education)

# Converting 'High Sales' to a factor
company_data$High_Sales <- factor(company_data$High_Sales)

# Set a random seed
set.seed(12345)

#Creating Training and Test data sets
inTrainingLocal <- createDataPartition(company_data$High_Sales,p=.75,list = F) # 75% data is training dataset
company_data_train <- company_data[inTrainingLocal,]
company_data_test <- company_data[-inTrainingLocal,]

# Training using 'random forest' algorithm
model <- train(High_Sales ~ Urban + US + ShelveLoc +Advertising+Price+Age+Education,
               data = company_data_train,
               method = 'rf',
               trControl = trainControl(method = 'cv',number = 5))
model

set.seed(12345)

model1 <- randomForest(as.factor(company_data_train$High_Sales)~.,data=company_data_train,ntree=1000)
model1

set.seed(51)
model2 <- randomForest(as.factor(company_data_train$High_Sales)~CompPrice+ Income + ShelveLoc +Advertising+Price+Age+Education+Population,data=company_data_train,ntree=1000)
model2



#Check if any missing values in test data before predection
summary(company_data_test)

#Code to fill if any NA found with mean of that column data
#test$Fare <- ifelse(is.na(test$Fare), mean(test$Fare, na.rm = TRUE), test$Fare)

#Importance of variable - Lower Gini
print(importance(model1))
print(importance(model2))

head(company_data_test)

#Prediction - With Model1
pred <- predict(model, newdata = company_data_test[,-11])
confusionMatrix(company_data_test$High_Sales,pred)

#Prediction - With Model2
pred1 <- predict(model1, newdata = company_data_test[,-11])
confusionMatrix(company_data_test$High_Sales,pred1)

#Prediction - With Model3
pred2 <- predict(model2, newdata = company_data_test[,-11])
confusionMatrix(company_data_test$High_Sales,pred2)
