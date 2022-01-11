### Twitter core analysis -Laboratory 2 FER
#--------------------------------------------------------------------------------

# install.packages("tm")
# install.packages("twitteR")

#--------------------------------------------------------------------------------

install.packages(textdata)
library(openssl)
library(httpuv)
library(twitteR)
library(tidyverse)
library(tm)
install.packages("base64inc") 
library("openssl")
library(base64enc)

#--------------------------------------------------------------------------------

rm(list=ls())
getwd()
setwd("/Users/svenskender/Downloads/lab2")

setup_twitter_oauth(consumer_key = '',
                    consumer_secret = '',
                    access_token ='',
                    access_secret ='')


user <- getUser("@a_schallenberg")
tweets <- userTimeline(user, n = 100)
#--------------------------------------------------------------------------------

tweets_data <- twListToDF(tweets)
tweets_data$text <- sapply(tweets_data$text,function(row) iconv(row, "latin1", "ASCII", sub="")) # ty FER2.net

#build a corpus and specify the source to be character of vectors
#a corpus is a collection of written texts
myCorpus <- Corpus(VectorSource(tweets_data$text))
myCorpus <- tm_map(myCorpus, function(x) iconv(enc2utf8(x), sub = "byte"))

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)

removeURL2 <- function(x) gsub("https[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL2)

#convert myCorpus into lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
#remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
  gsub("^ ", "", x) # Remove blank spaces at the beginning
  gsub(" $", "", x) # Remove blank spaces at the end
}

myCorpus <- tm_map(myCorpus,Textprocessing)

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# remove stop words
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus <- tm_map(myCorpus, removeWords, stopwords("german"))
#--------------------------------------------------------------------------------
library(wordcloud)

wordcloud(myCorpus,min.freq = 5,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#--------------------------------------------------------------------------------

# generiranje matrice
tdm <- TermDocumentMatrix(myCorpus)

termDocMatrix <- as.matrix(tdm)
termDocMatrix_sentiment <- termDocMatrix
termDocMatrix [termDocMatrix >=1] <- 1
termMatrix <- termDocMatrix %*% t(termDocMatrix)

termMatrixDF <- as.data.frame(termMatrix)
write.csv2(termMatrixDF, "termMatrix.csv", quote=FALSE)

#--------------------------------------------------------------------------------

# An Sentiment analiza
library(tidytext)

str(termDocMatrix_sentiment)
freq <- rowSums(as.matrix(termDocMatrix_sentiment))
freq <- as.data.frame(freq)
freq$word <- rownames(freq)
freq <- freq %>% arrange(desc(freq))

nrc_sentiment <- get_sentiments("nrc")
sentiment_val <- freq %>% inner_join(nrc_sentiment, by =c('word'))
#--------------------------------------------------------------------------------

# general wording sentiment
sent1 <- sentiment_val %>% count(sentiment, sort = TRUE)

# frequency wording sentiment
sent2 <- sentiment_val %>% group_by(sentiment) %>%
  summarise(total_freq = sum(freq))%>% arrange(desc(total_freq))

#--------------------------------------------------------------------------------

ggplot(sent1, aes(x = reorder(sentiment, -n, sum), y = n, fill = sentiment))  + geom_col() + theme_bw() +
  labs( x = 'Sentiment category', y = 'Value', title = 'Sentiment analysis')

ggplot(sent2, aes(x = reorder(sentiment, -total_freq, sum), y = total_freq, fill = sentiment))  + geom_col() + theme_bw() +
  labs( x = 'Sentiment category', y = 'Value', title = 'Sentiment analysis')

#--------------------------------------------------------------------------------
