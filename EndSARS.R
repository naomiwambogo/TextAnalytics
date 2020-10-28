# You will need the following packages.
if(!require(tm)) install.packages('tm')
if(!require(wordcloud)) install.packages('wordcloud')
if(!require(ROAuth)) install.packages("ROAuth")
if(!require(RCurl)) install.packages("RCurl")
if(!require(twitteR)) install.packages("twitteR") # for Search API
if(!require(streamR)) install.packages("streamR") # for Streaming API

library(tm)
library(wordcloud)
library(ROAuth)
library(RCurl)
library(twitteR)
library(streamR)

# You need to create a twitter app with your twitter account.  Please follow the steps below
# http://www.r-bloggers.com/setting-up-the-twitter-r-package-for-text-analytics/

apiKey <- "8ZrKV46oomfJneUfgIyaFBzCJ"       # Insert <Your consumer key>
apiSecret<- "e4JnCPr80L3Jpl4dF6EywTuB0Yn1523SpJHyKIPHHOZ2Kq0gvp"     # Insert <Your consumer secret>
token <- "298191686-3iMxX0F7fIEmvTlK1zq91erDYMXykI0XffX7mzaA"        # Insert <Your access token>
tokenSecret <- "DOTlTKaf5rWenJHKsjk9EtOIihNFQFJbcSIrOiunZiB5P"  # Insert <Your access secret>

# The following establishes the "handshake" between the R session and Twitter
# Simply select 1 for Yes when prompted.
setup_twitter_oauth(apiKey,apiSecret,token,tokenSecret)

# Search API is backward looking. Obtain 1000 tweets from recent history
# This may take a while
EndSARSTweets <- searchTwitter('#EndSARS', n=1000, lang='en')

# A nice function to create a data frame
EndSARSDF<-twListToDF(EndSARSTweets)
# Each tweet becomes a document in the corpus.
EndSARSCorpus <- Corpus(VectorSource(EndSARSDF$text))

# Functions for preprocessing texts
StripString=content_transformer(function(x,pattern) gsub(pattern,'',x))
latin2Ascii=content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=" "))

# Text preprocessing including removing URL
EndSARSCorpus=tm_map(EndSARSCorpus, latin2Ascii)
EndSARSCorpus=tm_map(EndSARSCorpus,StripString,'http[[:alnum:]]*')
EndSARSCorpus=tm_map(EndSARSCorpus,StripString,'[\r\n]') #hard returns
EndSARSCorpus=tm_map(EndSARSCorpus,StripString,'[\t]') # tabs
EndSARSCorpus=tm_map(EndSARSCorpus,StripString,'&amp') # ampersands
EndSARSCorpus=tm_map(EndSARSCorpus,content_transformer(tolower))
EndSARSCorpus=tm_map(EndSARSCorpus, stemDocument)
EndSARSCorpus=tm_map(EndSARSCorpus, removePunctuation)
EndSARSCorpus=tm_map(EndSARSCorpus, removeNumbers)
EndSARSCorpus=tm_map(EndSARSCorpus, stripWhitespace)


wordcloud(EndSARSCorpus, max.words = 200)

# Building a Term-Document Matrix
EndSARSTDM <- TermDocumentMatrix(EndSARSCorpus)

# Which words are associated  
findAssocs(EndSARSTDM, "kill", 0.20)
findAssocs(EndSARSTDM, "police", 0.30)
findAssocs(EndSARSTDM, "lekki", 0.30)
findAssocs(EndSARSTDM, "curfew", 0.30)

findAssocs(EndSARSTDM, "mazinnamdikanu", 0.30)

findAssocs(EndSARSTDM, "buhari", 0.30)


findAssocs(EndSARSTDM, "covid", 0.30)

findAssocs(EndSARSTDM, "renoomokri", 0.30)


### Clustering
# Remove sparse terms
EndSARSTDM2<- removeSparseTerms(EndSARSTDM,sparse = 0.97)
m2<- as.matrix(EndSARSTDM2)

#### Cluster terms
# cosine (dis)similarity may work better with text clustering.
# proxy package is required to use cosine similarity.
if (!require(proxy)) install.packages("proxy")
library(proxy)
distMatrix <-dist(scale(m2), method="cosine")
ward.out <-hclust(distMatrix, method="ward.D")
plot(ward.out, hang=-1)
rect.hclust(ward.out, k=10)

# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(6201)
k<-10
kmeans.out <- kmeans(m3, k)
# cluster centers
round(kmeans.out$centers, digits=3)

# print top 5 words in each cluster
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", spe=""))
  s<-sort(kmeans.out$centers[i,], decreasing=TRUE)
  cat(names(s)[1:5], "\n")
}
