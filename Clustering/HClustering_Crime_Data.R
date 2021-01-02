#Setting the path
setwd("C:/Hymaa/Data Science/Clustering/")

#Reading & storing the data
crime_data <- read.csv("crime_data.csv")

# create normalization function
normalize <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

#Apply Normalization
crime_data_n <- as.data.frame(lapply(crime_data[2:5],normalize))

# distance matrix
d <- dist(crime_data_n, method = "euclidean") 
fit <- hclust(d, method="complete")

#Plot dendogram
plot(fit, hang=-1,labels=NULL,xlab="States",main = "Crime Data by State")

#Identifying the clusters
rect.hclust(fit, k=4, border="red")
groups <- cutree(fit, k=4) # cut tree into 5 clusters

# groups or cluster numbers
membership<-as.matrix(groups) 

#Assigning cluster no. to state
final <- data.frame(crime_data$X, membership)

#Storing cluster information in file
write.csv(final, file="Criminal_Data_With_Clusters.csv",row.names = T)

#Crime rate for each cluster
aggregate(crime_data[,-1],by=list(final$membership),mean)

