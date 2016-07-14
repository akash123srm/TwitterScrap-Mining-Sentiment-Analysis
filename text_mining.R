

library("twitteR")
library("ROAuth")

#all this info is obtained for the Twitter developer account:

key <- "NIAfLMyCaJytEMQmKm1tuzaK6"

secret <- "jisoQf0XGTnVPiBEIKWic113vXe4NFSIbno5inR35R70Iot1gB"

#set a working directory for the whole process - 
#you need to download a few files and R needs to know where to look for that stuff

setwd("C:/Users/User/Documents/R/Twitter_Sentiment_Analysis")

#this is a crucial step - at least for windows users
#if you are on Linux or Mac you might skip this step, Win needs the certificate collection
#Cacert.pem is a collection of certificates

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/User/Documents/R/Twitter_Sentiment_Analysis/cacert.pem",
              method="auto")

#we are entering the whole Twitter API info and call the whole object authenticate

authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL='https://api.twitter.com/oauth/request_token',
                                 accessURL='https://api.twitter.com/oauth/access_token',
                                 authURL='https://api.twitter.com/oauth/authorize')


#this will get us to a Twitter Site - obtain the PIN
#the whole process is meant to provide the signature for your Twitter usage

authenticate$handshake(cainfo="C:/Users/User/Documents/R/Twitter_Sentiment_Analysis/cacert.pem")

#insert the PIN from Twitter

4497470

save(authenticate, file="twitter authentication.Rdata")

registerTwitterOAuth(authenticate)

# Alternative Authentication

access_token_secret <- "sbpw4MQj2sH79o4wMrg8nSdqh0G5nSOgV9Slf7EH9ACqS"

access_token <- "2431900465-sDi7vsC4vXIgqw6cajvZlmIEdwiduOwZCGhdUTI"


library("httr")


?setup_twitter_oauth

#This function wraps the OAuth authentication handshake functions 
#from the httr package for a twitteR session.Sets up the OAuth credentials for a twitteR session

#Note we nead to have proper internet connection to run this function

setup_twitter_oauth(key, secret, access_token, access_token_secret)

#Get the 1000 scraps with the keyword Udemy

tweets <- searchTwitter("#Udemy", n=1000)

# Lets check the recent tweets of Udemy


userTimeline("Udemy")
  

length(tweets)

head(tweets)

library("tm")


tweet_list <- sapply(tweets, function(x) x$getText())

# Create a corpus for the tweets

corpus_tweets <- Corpus(VectorSource(tweet_list))

# Convert the text into the lowercase

corpus_tweets <- tm_map(corpus_tweets, tolower)

# Remove punctuation

corpus_tweets <- tm_map(corpus_tweets, removePunctuation)

# Remove stopwords

corpus_tweets <- tm_map(corpus_tweets, function(x)removeWords(x, stopwords()))

# Converting text into plain text document

corpus_tweets <- tm_map(corpus_tweets, PlainTextDocument)

library("wordcloud")

? wordcloud

wordcloud(corpus_tweets, min.freq=5, scale=c(5,1), colors=brewer.pal(9,"Dark2"),
          random.color=F, max.word=50, random.order=F)

#changing to a tdm

corpus_tweets <- TermDocumentMatrix(corpus_tweets)

#A DocumentTermMatrix is a very useful tool when it comes to text mining.
#It structures the text in a matrix where each term is organized in a column. 
#Each row is a document and the number represents the counts of that term.


?findFreqTerms

findFreqTerms(corpus_tweets,lowfreq=15)

findAssocs(corpus_tweets, c("android","webdeveloper"), 0.6)


#Let’s get a dendrogram to see related terms
#-remove sparse (infrequently used) terms from the term-document matrix

corpus_tweets <-removeSparseTerms(corpus_tweets, sparse=0.9)

#Let’s scale the data

corpus_tweets <- scale(corpus_tweets)

#-distance matrix

corpus_tweets <- dist(corpus_tweets, method = "euclidean")

#-hierarchical clustering

corpus_tweets <- hclust(corpus_tweets)

#-Visualize the result

plot(corpus_tweets)

#-to calculate a certain number of groups

cutree(corpus_tweets, k=6)

#-we can even color the 6 groups and plot them

rect.hclust(corpus_tweets, k=6, border="red")




