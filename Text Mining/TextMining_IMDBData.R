library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# IMDBReviews #############################
#aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
aurl <- "https://www.imdb.com/title/tt8110330/reviews?ref_=tt_urv"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

setwd("C:/Hymaa/Data Science/Text Mining")
write.table(IMDB_reviews,"DilBechara.txt",row.names = F)


movie <- read.delim('DilBechara.txt')
str(movie)

View(movie)

# Build Corpus and DTM/TDM
library(tm)
corpus <- movie[-1,]
head(corpus)
class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
inspect(cleanset[1:5])

# Since the word laptop and can were used, this can be removed as we are 
# mining the reviews for this film.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,gsub,pattern='sushanth',replacement='sushant')
cleanset <- tm_map(cleanset,gsub,pattern='ssr',replacement='sushant')
cleanset <- tm_map(cleanset,gsub,pattern='singh',replacement='sushant')
cleanset <- tm_map(cleanset,gsub,pattern='rajput',replacement='sushant')
cleanset <- tm_map(cleanset,gsub,pattern='watched',replacement='watch')
cleanset <- tm_map(cleanset,gsub,pattern='watching',replacement='watch')

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 5227 words and 163478 documents(# of tweets) in this TDM
# Sparsity is 97% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 
w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
windows()
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
windows()
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# lettercloud 
letterCloud(w,word = 'A',frequency(5), size=1)


# Read File 
IMDB_reviews <- read.delim('DilBechara.txt')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]

get_nrc_sentiment('smile')

#Smile has 1 joy, 1 surprise, 1 trust and 1 positive

get_nrc_sentiment('no words') #1 Anger and 1 Negative

# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Dil Bechara')
