library("twitteR")
library("ROAuth")


#all this info is obtained for the Twitter developer account:

key <- "NIAfLMyCaJytEMQmKm1tuzaK6"

secret <- "jisoQf0XGTnVPiBEIKWic113vXe4NFSIbno5inR35R70Iot1gB"


access_token_secret <- "sbpw4MQj2sH79o4wMrg8nSdqh0G5nSOgV9Slf7EH9ACqS"

access_token <- "2431900465-sDi7vsC4vXIgqw6cajvZlmIEdwiduOwZCGhdUTI"


library("httr")

#This function wraps the OAuth authentication handshake functions 
#from the httr package for a twitteR session.Sets up the OAuth credentials for a twitteR session

#Note we nead to have proper internet connection to run this function

setup_twitter_oauth(key, secret, access_token, access_token_secret)

#Get the 1000 scraps with the keyword Udemy

tweets_sachin <- searchTwitter("sachintendulkar", n=1000, lang="en")


library("tm")


tweet_list <- sapply(tweets_sachin, function(x) x$getText())

# Create a corpus for the tweets

corpus_tweets <- Corpus(VectorSource(tweet_list))

# Convert the text into the lowercase

corpus_tweets <- tm_map(corpus_tweets, tolower)

# Remove punctuation

corpus_tweets <- tm_map(corpus_tweets, removePunctuation)

# Remove white space

corpus_tweets <- tm_map(corpus_tweets, stripWhitespace)

# Remove numbers

corpus_tweets <- tm_map(corpus_tweets, removeNumbers)

# Remove stopwords

corpus_tweets <- tm_map(corpus_tweets, function(x)removeWords(x, stopwords()))


# Converting text into plain text document

corpus_tweets <- tm_map(corpus_tweets, PlainTextDocument)

library("wordcloud")

library("RColorBrewer")

? wordcloud

w <- wordcloud(corpus_tweets, min.freq=5, scale=c(4,1), colors=brewer.pal(9,"Dark2"),
          rot.per=0.5, random.color=F, max.word=50, random.order=F)

# Saving the workcloud

print(w)

dev.copy(png, filename='word_cloud_sachin.png')

dev.off()

#changing to a tdm

corpus_tweets_dtm <- TermDocumentMatrix(corpus_tweets)

findFreqTerms(corpus_tweets_dtm, lowfreq=10) # experiment with the lowfreq

tdm <-removeSparseTerms(corpus_tweets_dtm, sparse=0.94) # experimet with sparse

tdmscale <- scale(tdm)

dist <- dist(tdmscale, method = "euclidean")

fit <- hclust(dist)

plot(fit)

#-to calculate a certain number of groups

cutree(fit, k=6)

#-we can even color the 4 groups and plot them


dendogram <- rect.hclust(fit, k=6, border="red")

# Saving the debdogram in the working directory






