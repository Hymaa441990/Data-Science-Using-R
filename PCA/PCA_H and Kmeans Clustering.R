library(caret)

#Read data
c <- read.csv("C:/Hymaa/Data Science/PCA/Wine.csv")
str(Wine_data)

#Excluding Classification Varibale
Wine_data1 <- Wine_data[,-1]

# create normalization function
normalize <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

#Apply Normalization
Wine_data1 <- as.data.frame(lapply(Wine_data1,normalize))
Wine_data2 <- cbind(Wine_data1,Type=Wine_data$Type)

View(Wine_data2)

#------------------Clustering without PCA--------------------------------------------
#------------------HClustering--------------------------------
d <- dist(Wine_data2[,-14], method = "euclidean") # distance matrix

fit1 <- hclust(d, method="ward.D")

plot(fit1,labels = FALSE, main = "Ward", xlab = "") # display dendrogram
plot(fit1, hang=-1)
rect.hclust(fit1, k=3, border="red")

groups <- cutree(fit1, k=3) # cut tree into 3 clusters
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Wine_data$Type, membership)

write.csv(final, file="Wines_HClust.csv",row.names = F)


#------------------------KMEANS-------------------------------------------
wss<-c()
for(i in 1:10)wss[i]<-sum(kmeans(Wine_data2[,-14],centers = i)$withinss)
plot(1:10,wss,type="b",xlab="no of clusters", ylab="avg distance")

# Compute k-means with k = 4
set.seed(123)
km<-kmeans(Wine_data2,3)

# Print the results
print(km)

aggregate(Wine_data, by=list(cluster=km$cluster), mean)

group_kmean<-km$cluster
membership_kmean<-as.matrix(group_kmean) # groups or cluster numbers
final_kmean <- data.frame(Wine_data$Type, membership_kmean)

write.csv(final_kmean, file="Wines_kmean1.csv",row.names = F)

#------------------Clustering with PCA--------------------------------------------

pca_data<-princomp(Wine_data2[,-14],cor=TRUE,scores = TRUE,covmat = NULL)
plot(pca_data)
summary(pca_data)
loadings(pca_data)
str(pca_data)

# verify by plotting variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(Wine_data2[,-14], var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(Wine_data2[,-14], var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)

# Verify variance is uniform
plot(sapply(Wines_data2, var))

# Proceed with principal components
pc <- princomp(Wines_data2[,-14])
plot(pc)
plot(pc, type='l')
summary(pc) # 3 components is both 'elbow' and explains >85% variance

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(Wines_data2[,-14])
plot(pc)

# First 3 principal components
comp <- data.frame(pc$x[,1:3])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

#------------------HClustering--------------------------------
d <- dist(comp, method = "euclidean") # distance matrix

fit1 <- hclust(d, method="ward.D")

plot(fit1,labels = FALSE, main = "Ward", xlab = "") # display dendrogram
plot(fit1, hang=-1)
rect.hclust(fit1, k=3, border="red")

groups <- cutree(fit1, k=3) # cut tree into 3 clusters
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(Wine_data$Type, membership)

write.csv(final, file="Wines_HClust_PCA.csv",row.names = F)

#------------------------KMEANS-------------------------------------------
wss<-c()
for(i in 1:10)wss[i]<-sum(kmeans(comp,centers = i)$withinss)
plot(1:10,wss,type="b",xlab="no of clusters", ylab="avg distance")

# Compute k-means with k = 4
set.seed(123)
km<-kmeans(comp,3)

# Print the results
print(km)

aggregate(comp, by=list(cluster=km$cluster), mean)

group_kmean<-km$cluster
membership_kmean<-as.matrix(group_kmean) # groups or cluster numbers
final_kmean <- data.frame(Wine_data$Type, membership_kmean)

write.csv(final_kmean, file="Wines_Kmean_PCA.csv",row.names = F)

#--------------------------------------------------------------------
# In all ways we have obtained same no. of optimum number of clusters
#--------------------------------------------------------------------
