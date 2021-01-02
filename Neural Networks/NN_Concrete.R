concrete <- read.csv("C:/Hymaa/Data Science/Neural Networks/concrete.csv")
View(concrete)
str(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

#Apply normalization to entire data frame
concrete_norm<-as.data.frame(lapply(concrete,normalize))

#Creating Training and Test data sets
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

# Using multilayered feed forward nueral network
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("strength",paste(colnames(concrete[-9]),collapse ="+"),sep="~")

Hid_layers <- c(1,5,10)

Hid_Layers_Vals<-c()
Cor_Vals<-c()

for(i in Hid_layers)
{
  
  
  #Building model
  concrete_model <- neuralnet(formula = formula_nn,data = concrete_train,hidden = i)
 
  #Visualize network typology
  plot(concrete_model)
  
  # Evaluating model performance
  # compute function to generate ouput for the model prepared
  model_results <- compute(concrete_model,concrete_test[1:8])
  
  #Predicted results
  predicted_strength <- model_results$net.result
  
  #Examin predicted vs. actual
  #Storing Hidden layers values to check performance based on changing the layers count
  
  Hid_Layers_Vals <- rbind(Hid_Layers_Vals,data.frame(i))
  Cor_Vals <- rbind(Cor_Vals,data.frame(cor(predicted_strength,concrete_test$strength)[1]))
  
  plot(predicted_strength,concrete_test$strength)
  
}

#Providing labels
names(Hid_Layers_Vals) <- c("No. of Hidden Layers")
names(Cor_Vals) <- c("Correlation Value")

#Performance matrix table
cbind(Hid_Layers_Vals,Cor_Vals)

