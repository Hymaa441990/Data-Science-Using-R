library(e1071)
setwd("C:/Hymaa/Data Science/Naive Bayes")
SalaryData_Train <- read.csv("SalaryData_Train.csv")
SalaryData_Test <- read.csv("SalaryData_Test.csv")

str(SalaryData_Train)
str(SalaryData_Test)

#Checking NA values in both data sets
dim(SalaryData_Train)
na_check_train<-table(is.na(SalaryData_Train))
na_check_train
str(SalaryData_Train)
summary(SalaryData_Train)

dim(SalaryData_Test)
str(SalaryData_Test)
na_check_test<-table(is.na(SalaryData_Test))
na_check_test
summary(SalaryData_Test)

colnames(SalaryData_Train)
head(SalaryData_Train)

#Training a model on the data
Classifier<-naiveBayes(SalaryData_Train,SalaryData_Train$Salary)
Pred<-predict(Classifier,SalaryData_Test)

#Evaluating model performance
acc <- CrossTable(Pred,SalaryData_Test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

fre <- as.data.frame(acc$prop.tbl)[[3]]
accuracy <- (fre[1] + fre[4])*100
round(accuracy,2)

#Accuracy ~ 97.02

#Improving model performance
accuray <- c()
laplace_value <- c()

for(i in 1:5)
{
  #Training a model on the data
  Classifier<-naiveBayes(SalaryData_Train,SalaryData_Train$Salary,laplace = i)
  Pred<-predict(Classifier,SalaryData_Test)
  
  #Evaluating model performance
  tab <- CrossTable(Pred,SalaryData_Test$Salary,
             prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
             dnn = c('predicted', 'actual'))
  
  fre <- as.data.frame(tab$prop.tbl)[[3]]
  acc <- (fre[1] + fre[4])*100

  laplace_value <- rbind(laplace_value,data.frame(i))
  accuray <- rbind(accuray,data.frame(round(acc,2)))
  
}

names(laplace_value) <- c("Laplace Value")
names(accuray) <- c("Accuracy Percentage")
cbind(laplace_value,accuray)

#Classifier with laplace 1 model highest accuracy ~ 98.20 among all other models
