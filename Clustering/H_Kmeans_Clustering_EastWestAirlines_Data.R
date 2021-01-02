library(caret)
#Set path
setwd("C:/Hymaa/Data Science/Clustering/")

#Read data
EastWestAirlines_data <- readxl::read_xlsx("EastWestAirlines.xlsx",sheet = "data")

#Change column names
names(EastWestAirlines_data)[1] <- "UniqueID"
names(EastWestAirlines_data)[12] <- "Award"

#Excluding Uniue ID and Award
EastWestAirlines_data_n <- EastWestAirlines_data[,-c(1,4,5,6,12)]

plot()

#Table of diagnosis
table(EastWestAirlines_data$Award)

# Table or proportions with more informative labels
round(prop.table(table(EastWestAirlines_data$Award))*100,digits = 1)

# create normalization function
normalize <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

#Apply Normalization
EastWestAirlines_data_n <- as.data.frame(lapply(EastWestAirlines_data_n,normalize))
EastWestAirlines_data_n_Award <- cbind(EastWestAirlines_data_n,Award=EastWestAirlines_data$Award)

View(EastWestAirlines_data_n)

ggplot(EastWestAirlines_data_n, aes(x = age, y = spend)) +
  geom_point()

#HClustering
d <- dist(EastWestAirlines_data_n[,-11], method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")

plot(fit) # display dendrogram
plot(fit, hang=-1)

rect.hclust(fit, k=10, border="red")
groups <- cutree(fit, k=10) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers

final <- data.frame(EastWestAirlines_data$UniqueID, membership)

write.csv(final, file="EastWestAirlines_With_Clusters_HClust.csv",row.names = F)

aggregate(EastWestAirlines_data[,-1],by=list(final$membership),mean)

table(EastWestAirlines_data_n[,-11],groups)


#Create training and test data

###########################################
#KNN_data_train <- createDataPartition(EastWestAirlines_data_n,p=0.75,list = FALSE)
#Award_yes <- data.frame(which(EastWestAirlines_data$Award==1))
#Award_No <- data.frame(which(EastWestAirlines_data$Award==0))

#KNN_data_train_Yes <- EastWestAirlines_data_n[3501:3999,]
#KNN_data_train_No <- createDataPartition(Award_No,p=0.75,list = FALSE)

#KNN_data_train <- EastWestAirlines_data_n[c(Award_yes[1:1110,],Award_No[1:1500,]),]
#KNN_data_test <- EastWestAirlines_data_n[c(Award_yes[1111:nrow(Award_yes),],Award_No[1501:nrow(Award_No),]),]

#Create labels for training and test data
#KNN_data_train_Labls <- EastWestAirlines_data_n_Award[c(Award_yes[1:1110,],Award_No[1:1500,]),11]
#KNN_data_test_Labls <- EastWestAirlines_data_n_Award[c(Award_yes[1111:nrow(Award_yes),],Award_No[1501:nrow(Award_No),]),11]
################################################


KNN_data_train <- EastWestAirlines_data_n[1:3500,]
KNN_data_test <- EastWestAirlines_data_n[3501:3999,]

#Create labels for training and test data
KNN_data_train_Labls <- EastWestAirlines_data_n_Award[1:3500,11]
KNN_data_test_Labls <- EastWestAirlines_data_n_Award[3501:3999,11]



#--------Training a model on the data-------------#
library(class)
KNN_data_test_pred <- knn(train = KNN_data_train,test = KNN_data_test,cl=KNN_data_train_Labls,k=10)

#--------Evaluating model preformance-------------#
library(gmodels)

#Create the crosstabulation of predicted vs.actual
CrossTable(x=KNN_data_test_Labls,y=KNN_data_test_pred,prop.chisq = FALSE,prop.c = FALSE,r=FALSE)
  