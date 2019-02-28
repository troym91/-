# This script is from "Text Mining with R - Twitter Data Analysis" by Yanchang Zhao
# http://www.rdatamining.com/docs/text-mining-with-r

# check data directory and files
getwd()
dir()

# install libraries if you need
# install.packages("twitteR")
# install.packages("tm")
# install.packages("ggplot2")
# install.packages("graph")
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# install.packages("wordcloud")
# install.packages("topicmodels")

# load libraries
library(twitteR)
library(tm)
library(ggplot2)
library(graph)
library(Rgraphviz)
library(wordcloud)
library(topicmodels)

# load and convert data
load("RDataMining-Tweets-20160203.rdata")  # pre-downloaded twitter data
tweets.df <- twListToDF(tweets)
tweets.df[1,]

###########################################################################
# Build vector space model
###########################################################################

# build a corpus, and specify the source to be character vectors
# a corpus is a set of documents, here one tweet is one document
myCorpus <- Corpus(VectorSource(tweets.df$text))
length(myCorpus) # number of documents
myCorpus$content[1:3] # actual tweet text of document 1~3

# text cleaning
# tm_map(corpus,function); applying function to the corpus

# covert to lower cases
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus$content[1:3]

# remove all urls
# gsub is a fucntion that substitutes matched patterns (1st arg) to the 2nd argument
# removeURL finds any words start with "http" and substitutes with ""(empty text)
# [:space:] indicates all characters similar to a white space (e.g. tab)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus$content[1:3]

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus$content[1:3]

# stopword: words which are filtered out before or after processing of natural language data (text)
stopwords('english')
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
# removeWords() is a built-in function of tm library
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus$content[1:3]

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus$content[1:3]

###########################################################################
# term frequency
##########################################################################

tdm <- TermDocumentMatrix(myCorpus,control=list(wordLengths=c(1,Inf)))

# sparse matrix form
cbind(tdm$i,tdm$j,tdm$v)[1:10,] # i, j, tf
tdm$nrow # number of rows = number of terms
tdm$ncol # number of columns = number of documents
tdm$dimnames$Term # actual terms
tdm$dimnames$Docs # document names
max(tdm$v) # what does it mean?

# inspect
inspect(tdm[1:10,1:10])

# term frequency
freq.terms = findFreqTerms(tdm,lowfreq=20,highfreq=Inf)
tf <- rowSums(as.matrix(tdm))
tf.data <- data.frame(term=names(tf),freq=tf)
df <- tf.data[tf.data$freq>=20,]
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +  xlab("Terms") + ylab("Count") + coord_flip()

# find association
# the association means Person's correlation of weight vectors of two terms
findAssocs(tdm,"r",0.2) # find all terms of which association >= 0.2
v1 = as.vector(tdm[tdm$dimnames$Term=="r",])
v2 = as.vector(tdm[tdm$dimnames$Term=="users",])
cor(v1,v2)

# term network
freq.terms = findFreqTerms(tdm,lowfreq=20,highfreq=Inf)
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)


###########################################################################
# word cloud
##########################################################################

# calculate the frequency of words and sort it by frequency
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = TRUE)

# word cloud
wordcloud(words=names(word.freq),freq=word.freq)
wordcloud(words=names(word.freq),freq=word.freq,random.order=FALSE)
wordcloud(words=names(word.freq),freq=word.freq,random.order=FALSE,min.freq=5)
wordcloud(words=names(word.freq),freq=word.freq,random.order=FALSE,min.freq=5,scale=c(4,1))
wordcloud(words=names(word.freq),freq=sqrt(word.freq),random.order=FALSE,min.freq=sqrt(5))
wordcloud(words=names(word.freq),freq=sqrt(word.freq),random.order=FALSE,min.freq=sqrt(5),colors=c("black","blue"))

# color
pal <- brewer.pal(9, "BuGn") # 9 colors from white to dark green
pal <- pal[-(1:4)] # only takes colors not too much white 
wordcloud(words=names(word.freq),freq=sqrt(word.freq),random.order=FALSE,min.freq=sqrt(5),colors=pal)

# make your own wordcloud


###########################################################################
# word clustering
##########################################################################

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(1-cor(t(m2)))
fit <- hclust(distMatrix, method = "complete")
plot(fit)
rect.hclust(fit, k = 6)


###########################################################################
# topic modeling
##########################################################################

dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 6) # first 6 terms of every topic
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
topic <- topics(lda, 1)

topics <- data.frame(date=as.Date(tweets.df$created), topic)
ggplot(topics,aes(x=date,y=..count..)) + geom_density(aes(fill=term[topic]),position="stack")

