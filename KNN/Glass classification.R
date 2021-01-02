library(caret)
library(class)
library(gmodels)

#Reading data set
glass_data <- read.csv("C:/Hymaa/Data Science/KNN/glass.csv")
glass_data <- glass_data[,c(-1)]

#Data set proportions
table(glass_data$Type)
round(prop.table(table(glass_data$Type))*100,digits = 1)

#Normalization function
normalize <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

#Apply Normalization
Nor_glass_data <- as.data.frame(lapply(glass_data[1:9],normalize))
Nor_glass_data_L <- cbind(Nor_glass_data,type=glass_data$Type)

#Partitioning 2 data sets
InTRaining_Local <- createDataPartition(Nor_glass_data_L$Type,times=1,p=.75,list = FALSE)
glass_data_train <- Nor_glass_data[InTRaining_Local,]
glass_data_test <- Nor_glass_data[-InTRaining_Local,]

#Assigning Labels
glass_data_train_labs <- Nor_glass_data_L[InTRaining_Local,10]
glass_data_test_labs <- Nor_glass_data_L[-InTRaining_Local,10]

View(glass_data_train)
View(glass_data_test)

#KNN Model for Prediction
Classify_glass_model <- knn(train=glass_data_train,test = glass_data_test,cl=glass_data_train_labs,k=sqrt(nrow(glass_data)))

#Prection table
CrossTable(x=glass_data_test_labs,y=Classify_glass_model,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)

#Finiding the model accuracy
n <- as.matrix(table(Actual=glass_data_test_labs,Predicted=Classify_glass_model))
accuray_per <- sum(diag(n)/length(glass_data_test_labs))
accuray_per
