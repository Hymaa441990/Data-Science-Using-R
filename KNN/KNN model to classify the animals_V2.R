library(caret)
library(class)
library(gmodels)

#Reading data set
animal_data <- read.csv("C:/Hymaa/Data Science/KNN/Zoo.csv")
animal_data_num <- animal_data[,c(-1)]

#Data set proportions
table(animal_data$type)
round(prop.table(table(animal_data$type))*100,digits = 1)

#Normalization function
normalize <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}


#Apply Normalization
Nor_animal_data <- as.data.frame(lapply(animal_data_num[1:16],normalize))
Nor_animal_data_L <- cbind(Nor_animal_data,type=animal_data_num$type)

#Partitioning 2 data sets
InTRaining_Local <- createDataPartition(Nor_animal_data_L$type,times=1,p=.75,list = FALSE)
animal_data_train <- Nor_animal_data[InTRaining_Local,]
animal_data_test <- Nor_animal_data[-InTRaining_Local,]

#Assigning Labels
animal_data_train_labs <- Nor_animal_data_L[InTRaining_Local,17]
animal_data_test_labs <- Nor_animal_data_L[-InTRaining_Local,17]


k_vals <- c(3,5,10,15,20,25)
kvalues<-c()
accuray<-c()

for(i in k_vals)
{
  #KNN Model for Prediction
  recog_Animal_model <- knn(train=animal_data_train,test = animal_data_test,cl=animal_data_train_labs,k=i)
  
  #Prection table
  CrossTable(x=animal_data_test_labs,y=recog_Animal_model,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
  
  #Finiding the model accuracy
  n <- as.matrix(table(Actual=animal_data_test_labs,Predicted=recog_Animal_model))
  accuray_per <- sum(diag(n)/length(animal_data_test_labs))
  
  kvalues <- rbind(kvalues,data.frame(i))
  accuray <- rbind(accuray,data.frame(accuray_per))
  
}

#Accuracy percetage on different k Values
names(kvalues) <- c("K-Value")
names(accuray) <- c("Accuracy Percentage")
cbind(kvalues,accuray)

