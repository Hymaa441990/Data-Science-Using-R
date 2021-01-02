library(caret)
library(C50)
library(ISLR)
library(dplyr)

require(tree)

company_data <- read.csv("C:/Hymaa/Data Science/Decision Trees/Company_Data.csv")

#Sales_cat <- cut(carseats$Sales,breaks = c(0,2,4,6,8,10,20),labels = c("<2","2-4","4-6","6-8","8-10",">10"),right=FALSE)
#carseats$Sales_Cat<- Sales_cat

names(company_data)
hist(company_data$Sales)

High_Sales <- ifelse(company_data$Sales < 8, "No", "Yes")
company_data <- data.frame(company_data, High_Sales)

company_data <- select(company_data,-c("Sales"))

#Picking the best Features
table(company_data[,c("High_Sales","Urban")]) #Keep
table(company_data[,c("High_Sales","US")]) #Keep
table(company_data[,c("High_Sales","ShelveLoc")]) #Keep


#Checking distribution of each continuous variable
#install.packages("fields")
library(fields)

bplot.xy(as.numeric(company_data$High_Sales), company_data$CompPrice) #Keep
summary(company_data$CompPrice)

bplot.xy(as.numeric(company_data$High_Sales), company_data$Income) #Keep
summary(company_data$Income)

bplot.xy(as.numeric(company_data$High_Sales), company_data$Advertising) #Keep
summary(company_data$Advertising)

bplot.xy(as.numeric(company_data$High_Sales), company_data$Population)
summary(company_data$Population)

bplot.xy(as.numeric(company_data$High_Sales), company_data$Price) #Keep
summary(company_data$Price)

bplot.xy(as.numeric(company_data$High_Sales), company_data$Age) #Keep
summary(company_data$Age)

bplot.xy(as.numeric(company_data$High_Sales), company_data$Education) #Keep
summary(company_data$Education)

# Converting 'High Sales' to a factor
company_data$High_Sales <- factor(company_data$High_Sales)



set.seed(9)

# splittinf data file into 2 part for training and testing
inTrainingLocal <- createDataPartition(company_data$High_Sales,p=.75,list = F) # 70% data is training dataset
training <- company_data[inTrainingLocal,]
testing <- company_data[-inTrainingLocal,]

#Model Building
model <- C5.0(training$High_Sales ~ ., data=training,trials=1) #Trails - Boosting Parameter

#Generate the model summary
summary(model)

#Predict for test dataset
pred <- predict.C5.0(model,select(testing,-c("High_Sales")))
a <- table(testing$High_Sales,pred)

#Accuracy of Model - 79 %
sum(diag(a))/sum(a)

confusionMatrix(testing$High_Sales,pred)

plot(model)
