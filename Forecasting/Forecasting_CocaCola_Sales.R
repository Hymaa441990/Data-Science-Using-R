library(readr)
setwd("C:/Hymaa/Data Science/Forecasting")
CocaCola_Data <- readxl::read_xlsx("CocaCola_Sales_Rawdata.xlsx",sheet = "Sheet1")
View(CocaCola_Data) # Seasonality 4 quarters 
windows()
plot(CocaCola_Data$Sales,type="o")
# So creating 4 dummy variables 



quar_names <- c('Quarter1','Quarter2','Quarter3','Quarter4')

X<- data.frame(outer(rep(quar_names,length = 42), quar_names,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-quar_names # Assigning month names 
View(X)
trakdata<-cbind(CocaCola_Data,X)
View(trakdata)
trakdata["t"]<- 1:42
View(trakdata)
trakdata["log_rider"]<-log(trakdata["Sales"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)

train<-trakdata[1:30,]

test<-trakdata[31:42,]

View(train)
########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 714.0144


######################### Exponential #################################

expo_model<-lm(log_rider~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 552.2821

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 646.2715

######################### Additive Seasonality #########################
sea_add_model<-lm(Sales~Quarter1+Quarter2+Quarter3,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1778.007

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Quarter1+Quarter2+Quarter3,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 637.9405

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Quarter1+Quarter2+Quarter3,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 586.0533

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_rider~Quarter1+Quarter2+Quarter3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1828.924

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_rider~t+Quarter1+Quarter2+Quarter3,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 410.2497

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_rider~t+Quarter1+Quarter2+Quarter3,data = trakdata)


resid <- residuals(new_model)
resid[1:20]
windows()
acf(resid,lag.max =15)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
test_data<-test
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)

#Actual Price
exp(pred_new)
