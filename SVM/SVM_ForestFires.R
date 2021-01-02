setwd("C:/Hymaa/Data Science/SVM")
Forestfires_Data<-read.csv("forestfires.csv")

#Keep only useful columns
Forestfires_Data<- Forestfires_Data[,c(3:10,31)]
str(Forestfires_Data)

# Splitting the dataset into the Training set and Test set 
library(caTools) 

set.seed(123) 
split <- sample.split(Forestfires_Data$size_category, SplitRatio <- 0.75) 

training_set <- subset(Forestfires_Data, split == TRUE) 
test_set <- subset(Forestfires_Data, split == FALSE) 
View(training_set)
View(test_set)

# Feature Scaling 
training_set[-9] <- scale(training_set[-9]) 
test_set[-9] <- scale(test_set[-9]) 
nrow(training_set)

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
    classifier<-ksvm(size_category ~.,data = training_set,kernel = i,type=j)
    classifier
    
    y_pred <- predict(classifier, newdata <- test_set[-9]) 
    y_pred
    
    acc <- mean(y_pred==test_set$size_category)*100
    
    accuracy <- rbind(accuracy,data.frame(acc))                  
    
  }
}


#Accuracy percetage on different k Values
names(name) <- c("Name")
names(accuracy) <- c("Accuracy Percentage")
cbind(name,accuracy)

#Kernal = rbfdot & Type = spoc-svc - Model has highest accuracy ~ 75.19380

