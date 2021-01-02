library(mlbench)
library(tm)
library(tidyverse)
library(SnowballC)
library(wordcloud)
library(gmodels)

#Get Data
setwd("C:/Hymaa/Data Science/Naive Bayes")
spam_ham_data <- read.csv("sms_raw_NB.csv")
str(spam_ham_data)

#Change Variable type
spam_ham_data$text <- as.character(spam_ham_data$text)
str(spam_ham_data)

#Cleaning and standardizing text data
sms_corpus <- VCorpus(VectorSource(spam_ham_data$text))
typeof(sms_corpus)
print(sms_corpus)

#Inspect Documnets in corpus
length(sms_corpus) %>%
  sample(replace = FALSE) %>%
  sort.list(decreasing = FALSE) %>%
  head(2) %>%
  sms_corpus[.] %>%
  tm::inspect()

# Stemming
sms_corpus_clean <- sms_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords()) %>%
  tm_map(removePunctuation) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace)

#Look at the change before processing and after processing
cat("The text document prior processing:", "\n")
for(i in 1:3){
  print(as.character(sms_corpus[[i]]))
}
cat("\n")
cat("The text document after processing:", "\n")
for(i in 1:3){
  print(as.character(sms_corpus_clean[[i]]))
}

#Splitting text documents into words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_dtm_no_prep <- DocumentTermMatrix(
  sms_corpus,
  control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    removePunctuation = TRUE,
    stemming = TRUE
  )
)

#Compare the 2 matrix

cat("Our matrix with preprocessing:", "\n")
sms_dtm
cat("\n")
cat("Our matrix without preprocessing:", "\n")
sms_dtm_no_prep

#Creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- spam_ham_data[1:4169, ]$type
sms_test_labels <- spam_ham_data[4170:5559, ]$type

cat("Our training data")
sms_train_labels %>%
  table %>%
  prop.table
cat("\n")
cat("Our testing data")
sms_test_labels %>%
  table %>%
  prop.table

#Creating a Word Cloud visualization
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

par(mfcol = c(1, 2))
spam <- spam_ham_data %>%
  subset(type == "spam")
spamCloud <- wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
ham <- spam_ham_data %>%
  subset(type == "ham")
hamCloud <- wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


#Creating indicator features for frequent words

sms_dtm_freq_train <- sms_dtm_train %>%
  findFreqTerms(5) %>%
  sms_dtm_train[ , .]
sms_dtm_freq_test <- sms_dtm_test %>%
  findFreqTerms(5) %>%
  sms_dtm_test[ , .]

#DT matrices from numeric to categorical "yes/no" matrices
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- sms_dtm_freq_train %>%
  apply(MARGIN = 2, convert_counts)
sms_test <- sms_dtm_freq_test %>%
  apply(MARGIN = 2, convert_counts)

#Step 3 - training a model on the data
sms_classifier <- e1071::naiveBayes(sms_train, sms_train_labels)
sms_pred <- predict(sms_classifier, sms_test)

#Step 4 - evaluating model performance
CrossTable(sms_pred, sms_test_labels, prop.chisq = FALSE, chisq = FALSE, 
           prop.t = FALSE,
           dnn = c("Predicted", "Actual"))
#ACCURACY ~ 97.4%

#Step 5 - Improving model performance
sms_classifier2 <- e1071::naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_pred2, sms_test_labels, prop.chisq = FALSE, chisq = FALSE, 
           prop.t = FALSE,
           dnn = c("Predicted", "Actual"))
#ACCURACY ~ 97.5%
