setwd("C:/Hymaa/Data Science/SVM")
SalaryData_Train<-read.csv("SalaryData_Train.csv")
SalaryData_Test<-read.csv("SalaryData_Test.csv")
str(SalaryData_Train)
str(SalaryData_Test)

#Keep only useful columns
cols <- c("age","educationno","capitalgain","capitalloss","hoursperweek","Salary")
SalaryData_Train<- SalaryData_Train[,cols]
SalaryData_Test<- SalaryData_Test[,cols]
str(SalaryData_Train)
str(SalaryData_Test)

table(SalaryData_Train$Salary)
table(SalaryData_Test$Salary)


# Handling Biased Training Data
input_ones <- SalaryData_Train[which(SalaryData_Train$Salary == " <=50K"), ]  # all 1's
input_zeros <- SalaryData_Train[which(SalaryData_Train$Salary == " >50K"), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

table(trainingData$Salary)

# Handling Biased Test Data
input_ones <- SalaryData_Test[which(SalaryData_Test$Salary == " <=50K"), ]  # all 1's
input_zeros <- SalaryData_Test[which(SalaryData_Test$Salary == " >50K"), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_test_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  # 1's for training
input_zeros_test_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's

test_ones <- input_ones[input_ones_test_rows, ]  
test_zeros <- input_zeros[input_zeros_test_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

table(testData$Salary)


# Feature Scaling 
trainingData[-6] <- scale(trainingData[-6]) 
testData[-6] <- scale(testData[-6]) 

# Fitting SVM to the Training set 
library(e1071) 

library(kernlab)
library(caret)


#Create Model
kernals <- c("rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", "besseldot", "anovadot")
types <- c("C-svc","nu-svc","C-bsvc","spoc-svc","kbb-svc","one-svc","eps-svr","nu-svr","eps-bsvr")

name <- c()
accuracy <- c()


for(i in kernals)
{
  for(j in types)
  {
    name <- rbind(name,data.frame(paste("Kernal = ",i," - Type = ",j)))
    classifier<-ksvm(Salary ~.,data = trainingData,kernel = i,type=j)
    classifier
    
    y_pred <- predict(classifier, newdata <- testData[-6]) 
    y_pred
    
    acc <- mean(y_pred==testData$Salary)*100
    
    accuracy <- rbind(accuracy,data.frame(acc))                  
    
  }
}


#Accuracy percetage on different k Values
names(name) <- c("Name")
names(accuracy) <- c("Accuracy Percentage")
cbind(n,accuracy)

#Kernal = laplacedot & Type = C-bsvc - Model has highest accuracy ~ 75.98456

classifier<-ksvm(Salary ~.,data = trainingData,kernel = "laplacedot",type="C-bsvc")
classifier

y_pred <- predict(classifier, newdata <- testData[-6]) 
y_pred

acc <- mean(y_pred==testData$Salary)*100
acc
