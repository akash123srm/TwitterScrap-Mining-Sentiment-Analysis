
setwd("C:/Users/User/Documents/R/Twitter_Sentiment_Analysis")

#Estimating Sentiment

#Sentiment analysis is an active area of research involving complicated algorithms and subtleties. For the purposes of this tutorial, we err on the side of simplicity and estimate a tweet’s sentiment by counting the number of occurrences of “positive” and “negative” words.

#To assign a numeric score to each tweet, we’ll simply subtract the number of occurrences of negative words from the number of positive. Larger negative scores will correspond to more negative expressions of sentiment, neutral (or balanced) tweets should net to zero, and very positive tweets should score larger, positive numbers.



#Sentiment Lexicon: a list of words which you are using to compare your scraped txt with

#Hu Liu Lexicon got the standard of sentiment analysis lately list of pos and negative words - manually created - approx. 6800


positives <- readLines("Positive-Words.txt")

negatives <-  readLines("Negative-Words.txt")

#Implementing our sentiment scoring algorithm

#To score each tweet, our score.sentiment() function uses laply() to iterate through the input text. It strips punctuation and control characters from each line using R’s regular expression-powered substitution function, gsub(), and uses match() against each word list to find matches:

library("stringr")

library("plyr")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  #Parameters
  #-sentences: vector of text to score
  #-pos.words: vector of words of postive sentiment
  #-neg.words: vector of words of negative sentiment
  #-.progress: passed to laply() to control of progress bar
  
  #-create simple array of scores with laply
  
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   #-remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   #-remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   #-remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   #-define error handling function when trying tolower
                   
                   tryTolower = function(x)
                   {
                     #-create missing value
                     y = NA
                     #-tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     #-if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     #-result
                     return(y)
                   }
                   #-use tryTolower with sapply
                   
                   sentence = sapply(sentence, tryTolower)
                   
                   #-split sentence into words with str_split (stringr package)
                   
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
  
                   #compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   #-get the position of the matched term or NA
                   #-we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   #-final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  #-data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
  
  
# Now we will fetch tweets related to different countries

library("twitteR")


tweets_usa <- searchTwitter("usa", n=900,lang="en")

tweets_germany <- searchTwitter("germany", n=900,lang="en")

tweets_india <- searchTwitter("india", n=900,lang="en")

tweets_russia <- searchTwitter("russia", n=900,lang="en")

#get text from the tweets

usa_txt = sapply(tweets_usa, function(x) x$getText())

germany_txt = sapply(tweets_germany, function(x) x$getText())

india_txt = sapply(tweets_india, function(x) x$getText())

russia_txt = sapply(tweets_russia, function(x) x$getText())


#-how many tweets of each country

nd = c(length(usa_txt), length(germany_txt),
       length(india_txt), length(russia_txt))

#-comine the texts

country = c(usa_txt, germany_txt, india_txt, russia_txt)

#-apply function score.sentiment

scores = score.sentiment(country, positives, negatives, .progress='text')

#add variables to data frame

scores$country = factor(rep(c("usa", "germany", "india", "russia"), nd))

scores$very.pos = as.numeric(scores$score >= 2)

scores$very.neg = as.numeric(scores$score <= -2)

head(scores)

b <- boxplot(score~country, data=scores,main="Distribution of sentiment scores assigned to 4 countries",
             xlab="Country",ylab="Score",col=c("red","yellow","blue","green"))

# Saving the boxplot to the working directory

dev.copy(png,'boxplot_countries.png')

dev.off()
  
library("lattice")


h <- histogram(data=scores, ~score|country, main="Sentiment Analysis of 4 Countries", xlab="", sub="Sentiment Score")
  
# Saving the histogram to the working directory

print(h)

dev.copy(png, filename='histogram_countries.png')

dev.off()




