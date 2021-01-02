library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)

#Read data
book_ratings <- read.csv("C:/Hymaa/Data Science/RecommendationSystem/book.csv")
summary(book_ratings)
str(book_ratings)

#Distribution of the ratings
barplot(table(book_ratings$Book.Rating))

object.size(book_ratings)

#Only a subset of the data will be used to build the recommender systems
book_ratings$User.ID <- as.factor(book_ratings$User.ID)
book_ratings$Book.Title <- as.factor(book_ratings$Book.Title)
bmatrix <- as(book_ratings, "realRatingMatrix")
dim(bmatrix@data)

#Similarity Matrix
sim <- similarity(bmatrix[1:10, ], method = "cosine", which = "users")
windows()
image(as.matrix(sim), main = "User Similarity")

sim2 <- similarity(bmatrix[ ,1:10], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")

# users who rated at least 100 books and books rated at least 100 times
bmatrix1 <- bmatrix[rowCounts(bmatrix) > 0, colCounts(bmatrix) > 10]
bmatrix1

#How are the ratings disributed?
tbl_ratings <- as.data.frame(table(as.vector(bmatrix1@data)))
tbl_ratings

tbl_ratings <- tbl_ratings[-1,] #0 means missing values so remove missing values
ggplot(tbl_ratings, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle("Distribution of Book Ratings")

#Most rated books
rated_count <- colCounts(bmatrix)
read_book <- data.frame(
  Book.Title = names(rated_count),
  read = rated_count
)

top_books <- read_book  %>%
  arrange(desc(read_book$read))%>% 
  head(10)

#Average book ratings
windows()
avg_book_ratings <- data.frame("avg_rating" = colMeans(bmatrix1)) %>% 
  ggplot(aes(x = avg_rating)) + 
  geom_histogram(color = "black", fill = "lightgreen") + 
  ggtitle("Distribution of Average Ratings for Books")
avg_book_ratings


min_readers <- quantile(rowCounts(bmatrix1), 0.99)
min_books <- quantile(colCounts(bmatrix1), 0.99)


#Training and Testing Datasets
train <- sample(x = c(T, F), size = nrow(bmatrix1), replace = T, prob = c(0.8, 0.2)) 
books_train <- bmatrix1[train, ] 
books_test <- bmatrix1[-train, ]

#Popularity based 
POPULAR <- Recommender(books_train, method="POPULAR")
POPULAR

#Predictions for two users 
recommended_items1 <- predict(POPULAR, books_test[413:414], n=5)
as(recommended_items1, "list")

#User Based Collaborative Filtering
UBCF <- Recommender(books_train, method="UBCF")
UBCF

#Predictions for two users 
recommended_items2 <- predict(UBCF, books_test[413:414], n=5)
as(recommended_items2, "list")

#Item-Item Collaborative Filtering
IBCF <- Recommender(books_train, method="IBCF")
IBCF

#Predictions for two users 
recommended_items3 <- predict(Item_Based, books_test[402:414], n=5)
as(recommended_items3, "list")

