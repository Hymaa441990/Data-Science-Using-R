api_key <- "pF8QVeAoe20OwQ1EGY8foaTFq"
api_secret <- "2RnWzrPhpqc0Q4EVnv1XAs5hRCFMOmF9G74PeiOnnec8nBbkxh"
access_token <- "918468922529103872-bqyeRczSuBhgFYHsmzrgMDvrR5MW0Yh"
access_token_secret <- "h9fWTtzWAPn1lLT9ajXRUwpTyBa2PSooAYH08RhGFmTA3"

#Load Library
library("twitteR")
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Getting tweets
tweets <- searchTwitter('@narendramodi',n=1000,lang='en')
tweets

tweetsdf <- twListToDF(tweets)
write.csv(tweetsdf, file="C:/Hymaa/Data Science/Text Mining/NarendraModi.csv",row.names = F)

twitter_data <- read.csv("C:/Hymaa/Data Science/Text Mining/NarendraModi.csv")
head(twitter_data)

#Trend Location
trend <- availableTrendLocations()
head(trend)
trend

#Getting Trends
world <- getTrends(1)
head(world)

Hyderabad <- getTrends(2295414)
head(Hyderabad)

#UserTimeline
t <- getUser('captriturathee')
t

userTimeline(t,n=2)

#Read File
twitter_data <- read.csv("C:/Hymaa/Data Science/Text Mining/NarendraModi.csv",header = T)
str(twitter_data)

#Build Corpus
library(tm)

corpus <- iconv(twitter_data$text,to="latin1")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean Text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

#stopwords1 <- read.delim("C:/Hymaa/Data Science/Text Mining/stop.txt")
#stopwords1 <- c(stopwords1)

cleanset <- tm_map(corpus,removeWords,stopwords('English'))
inspect(cleanset[1:5])

removeUrL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset,content_transformer(removeUrL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

removename <- function(x) gsub('narendramodi','',x)
cleanset <- tm_map(cleanset,content_transformer(removename))
inspect(cleanset[1:5])


cleanset <- tm_map(cleanset,removeWords,c('narendramodi','bjpindia','india','govt','china','pmoindia'))
cleanset <- tm_map(cleanset,gsub,pattern='covid',replacement='Covid19')

#Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:20]

#Bar Plot
w <- rowSums(tdm)
w <- subset(w,w>=25)

windows()
barplot(w,las=2,col=rainbow(50))

#WordCloud

library(wordcloud)

w <- sort(rowSums(tdm),decreasing = TRUE)

set.seed(222)
wordcloud(words=names(w),freq = w,max.words = 150,
          random.order = FALSE,min.freq = 5,colors=brewer.pal(8,'Dark2'),
          scale = c(5,0.3),rot.per=0.8)

library(wordcloud2)
w1 <- data.frame(names(w),w)
colnames(w1) <- c('word','freq')
w1

#Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library("httpuv")
library(httr)

tweets <- iconv(twitter_data$text,to="latin1")
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]
get_nrc_sentiment('accident')

#Bar Plot
barplot(colSums(s),las=2,col=rainbow(10),ylab = 'Count',main='Sentiment Scores for Naredramodi Tweets')
