library(caret)
Fireforests <- read.csv("C:/Hymaa/Data Science/Neural Networks/fireforests.csv")

cols_exclud <- c(1,2,12:30)
Fireforests <- Fireforests[,-cols_exclud]


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

#Apply normalization to entire data frame
Fireforests_norm<-as.data.frame(lapply(Fireforests,normalize))

View(Fireforests_norm)
set.seed(12345)
#Creating Training and Test data sets
inTrainingLocal <- createDataPartition(Fireforests_norm$area,p=.80,list = F) # 70% data is training dataset
Fireforests_train <- Fireforests_norm[inTrainingLocal,]
Fireforests_test <- Fireforests_norm[-inTrainingLocal,]

# Using multilayered feed forward nueral network
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("area",paste(colnames(Fireforests[-9]),collapse ="+"),sep="~")

#Deciding no.of layers based on performance
Hid_layers <- c(5,10,15)

Hid_Layers_Vals<-c()
Cor_Vals<-c()

for(i in Hid_layers)
{
  
  #Building model
  Fireforests_model <- neuralnet(formula = formula_nn,data = Fireforests_train,hidden=i)
  
  #Visualize network typology
  plot(Fireforests_model)
  
  # Evaluating model performance
  # compute function to generate ouput for the model prepared
  model_results <- compute(Fireforests_model,Fireforests_test[1:8])
  
  #Predicted results
  predicted_area <- model_results$net.result
  
  #Examin predicted vs. actual
  #Storing Hidden layers values to check performance based on changing the layers count
  
  Hid_Layers_Vals <- rbind(Hid_Layers_Vals,data.frame(i))
  Cor_Vals <- rbind(Cor_Vals,data.frame(cor(predicted_area,Fireforests_test$area)[1]))
  
  plot(predicted_area,Fireforests_test$area)
  
}

#Providing labels
names(Hid_Layers_Vals) <- c("No. of Hidden Layers")
names(Cor_Vals) <- c("Correlation Value")

#Performance matrix table
cbind(Hid_Layers_Vals,Cor_Vals)

#Final Model
Fireforests_model <- neuralnet(formula = formula_nn,data = Fireforests_train,hidden = 10)
plot(Fireforests_model)

# ************Predicting the area**********.

{
  FFMC <- as.numeric(readline(prompt="Enter FFMC :"))
  DMC <- as.numeric(readline(prompt="Enter DMC:"))
  DC <- as.numeric(readline(prompt="Enter DC: "))
  ISI <- as.numeric(readline(prompt="Enter ISI: "))
  temp <- as.numeric(readline(prompt="Enter temp: "))
  RH <- as.numeric(readline(prompt="Enter RH: "))
  wind <- as.numeric(readline(prompt="Enter wind: "))
  rain <- as.numeric(readline(prompt="Enter rain: "))
  
  input_data <- data.frame(FFMC,DMC,DC,ISI,temp,RH,wind,rain)
  
  Predicted_area <- predict(Fireforests_model,newdata = input_data)
  
  #Apply Denormalization to predicted value
  Predicted_area_1<-as.data.frame(Predicted_area*(max(Fireforests[,9])-min(Fireforests[,9]))+min(Fireforests[,9]))
  
  #Show predicted area figure  
  print(paste("Predicted area figure will be : ",round(Predicted_area_1,2)))
}
