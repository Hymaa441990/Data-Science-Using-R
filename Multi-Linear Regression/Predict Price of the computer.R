library(lattice)

computer_data <- read.csv("C:/Hymaa/Data Science/Multi-Linear Regression/Computer_Data.csv")
str(computer_data)
computer_data <- computer_data[,-1]


computer_data$cd <- ifelse(computer_data$cd=="yes", 1,0)
computer_data$multi <- ifelse(computer_data$multi=="yes", 1,0)
computer_data$premium <- ifelse(computer_data$premium=="yes", 1,0)

computer_data <- data.frame(scale(computer_data))
computer_data_num <- computer_data[,-c(6,7,8)]
str(computer_data_num)

#Scatter Plot Matrix :
pairs(computer_data)
pairs(computer_data_num)

#Correlation Matrix :
cor(computer_data_num)

#Regression Model and Its Summary
rsquare_vals1<-c()
adj_r_square_vals1<-c()

price_Model_all <- lm(price~.,data=computer_data)
names(summary(price_Model_all))

rsquare_vals1 <- cbind(rsquare_vals1,rall=summary(price_Model_all)$r.squared)
adj_r_square_vals1 <- cbind(adj_r_square_vals1,all=summary(price_Model_all)$adj.r.squared)

price_Model_Exclu_cd <- lm(price~.,data=computer_data[,-6])
rsquare_vals <- cbind(rsquare_vals1,rExclu_State=summary(price_Model_Exclu_cd)$r.squared )
adj_r_square_vals1 <- cbind(adj_r_square_vals1,Exclu_State=summary(price_Model_Exclu_cd)$adj.r.squared)

price_Model_Exclu_multi <- lm(price~.,data=computer_data[,-7])
rsquare_vals1 <- cbind(rsquare_vals,rExclu_Admin=summary(price_Model_Exclu_multi)$r.squared)
adj_r_square_vals1 <- cbind(adj_r_square_vals1,Exclu_Admin=summary(price_Model_Exclu_multi)$adj.r.squared)

price_Model_on_premium <- lm(price~.,data=computer_data[,c(-8)])
rsquare_vals1 <- cbind(rsquare_vals1,rOn_RandD=summary(price_Model_on_premium)$r.squared)
adj_r_square_vals1 <- cbind(adj_r_square_vals1,On_RandD=summary(price_Model_on_premium)$adj.r.squared)

data.frame(c(rsquare_vals1))
data.frame(c(adj_r_square_vals1))

library(car)


computer_data_num_Exclu_Outliers <- computer_data

outlier_array <- c()
rsquare_vals <- c()
adj_r_square_vals <- c()
outlier_array_pre <- c()



repeat
{
  price_Model <- lm(price~.,data=computer_data_num_Exclu_Outliers)
  rsquare_vals <- cbind(rsquare_vals,summary(price_Model)$r.squared)
  adj_r_square_vals <- cbind(adj_r_square_vals,summary(price_Model)$adj.r.squared)
  
  # Diagnostic plots:
  # Residual plots, QQ - Plots, Std. Residual vs Fitted
  plot(price_Model)
  
  # Residuals vs Regressors
  residualPlots(price_Model)
  
  # Added variables plots
  windows()
  avPlots(price_Model)
  
  # QQ plots for studentized residuals
  n <- as.data.frame(qqPlot(price_Model))
  
  # Deletion Diagnostics
  influenceIndexPlot(price_Model) # Index plots of the influence measures:
  influence(price_Model)
  
  outlier_array <- as.numeric(rownames(n))
  
  if(is.null(outlier_array_pre)) 
    outlier_array_pre <- outlier_array
  else
    if (outlier_array == outlier_array_pre) break
  
  outlier_array_pre <- outlier_array
  
  
  if(length(outlier_array)>0)
    {
      
      computer_data_num_Exclu_Outliers<-computer_data_num_Exclu_Outliers[-c(outlier_array),]
      nrow(computer_data_num_Exclu_Outliers)
    }
  
  
  if(length(outlier_array) == 0) break
  
}



#R2 and Adjusted R2 Values
data.frame(R2=c(rsquare_vals))
data.frame(Adj_R2=c(adj_r_square_vals))


# ************Predicting the price**********.

{
computer.speed <- readline(prompt="Enter Computer speed: ")
computer.hd <- readline(prompt="Enter Computer hd: ")
computer.ram <- readline(prompt="Enter Computer ram: ")
computer.screen <- readline(prompt="Enter Computer Screen: ")
computer.cd <- readline(prompt="Enter Computer cd: ")
computer.multi <- readline(prompt="Enter Computer Multi: ")
computer.premium <- readline(prompt="Enter Computer Premium: ")
computer.ads <- readline(prompt="Enter Computer Ads: ")
computer.trend <- readline(prompt="Enter Computer Trend: ")

Computer_price <- predict(price_Model,newdata = data.frame(speed=as.numeric(computer.speed),hd=as.numeric(computer.hd),ram=as.numeric(computer.ram),screen=as.numeric(computer.screen), cd=as.numeric(computer.cd),multi=as.numeric(computer.multi),premium=as.numeric(computer.premium),ads=as.numeric(computer.ads),trend=as.numeric(computer.trend)))
print(paste("Computer Price will be : ",round(Computer_price,0)))

}
