Startups <- read.csv("C:/Hymaa/Data Science/Neural Networks/50_Startups.csv")
View(Startups)
str(Startups)

Startups <- Startups[,-4]
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

#Apply normalization to entire data frame
Startups_norm<-as.data.frame(lapply(Startups,normalize))
View(Startups_norm)

#Creating Training and Test data sets
Startups_train<-Startups_norm[1:35,]
Startups_test<-Startups_norm[36:50,]

# Using multilayered feed forward nueral network
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("Profit",paste(colnames(Startups[-4]),collapse ="+"),sep="~")

#Deciding no.of layers based on performance
Hid_layers <- c(1,3,5,7,10)

Hid_Layers_Vals<-c()
Cor_Vals<-c()

for(i in Hid_layers)
{
  
  set.seed(12345)
  #Building model
  Startups_model <- neuralnet(formula = formula_nn,data = Startups_train,hidden = c(i,3))
  
  #Visualize network typology
  plot(Startups_model)
  
  # Evaluating model performance
  # compute function to generate ouput for the model prepared
  model_results <- compute(Startups_model,Startups_test[1:3])
  
  #Predicted results
  predicted_Profit <- model_results$net.result
  
  #Examin predicted vs. actual
  #Storing Hidden layers values to check performance based on changing the layers count
  
  Hid_Layers_Vals <- rbind(Hid_Layers_Vals,data.frame(i))
  Cor_Vals <- rbind(Cor_Vals,data.frame(cor(predicted_Profit,Startups_test$Profit)[1]))
  
  plot(predicted_Profit,Startups_test$Profit)
  
}

#Providing labels
names(Hid_Layers_Vals) <- c("No. of Hidden Layers")
names(Cor_Vals) <- c("Correlation Value")

#Performance matrix table
cbind(Hid_Layers_Vals,Cor_Vals)

#Final Model
Startups_model <- neuralnet(formula = formula_nn,data = Startups_train,hidden = c(5,3))
#plot(Startups_model)
model_results <- compute(Startups_model,Startups_test[1:4])

#Predicted results
predicted_Profit <- model_results$net.result

# ************Predicting the Profit**********.

{
  R.D.Spend <- readline(prompt="Enter R.D. Spend cost :")
  Administration <- readline(prompt="Enter Administration cost:")
  Marketing.Spend <- readline(prompt="Enter Marketing Spend cost: ")
  
  input_data <- data.frame(as.numeric(R.D.Spend),as.numeric(Administration),as.numeric(Marketing.Spend))
  
  Predicted_Profit <- predict(Startups_model,newdata = input_data)
  
  #Apply Denormalization to predicted value
  Predicted_Profit_1<-as.data.frame(Predicted_Profit*(max(Startups[,4])-min(Startups[,4]))+min(Startups[,4]))
  
  #Show predicted profit figure  
  print(paste("Predicted Profit figure will be : ",round(Predicted_Profit_1,2)))
}
