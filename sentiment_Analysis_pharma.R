
setwd("C:/Users/User/Documents/R/Twitter_Sentiment_Analysis")

#Estimating Sentiment

#Sentiment analysis is an active area of research involving complicated algorithms and subtleties. For the purposes of this tutorial, we err on the side of simplicity and estimate a tweet’s sentiment by counting the number of occurrences of “positive” and “negative” words.

#To assign a numeric score to each tweet, we’ll simply subtract the number of occurrences of negative words from the number of positive. Larger negative scores will correspond to more negative expressions of sentiment, neutral (or balanced) tweets should net to zero, and very positive tweets should score larger, positive numbers.



#Sentiment Lexicon: a list of words which you are using to compare your scraped txt with

#Hu Liu Lexicon got the standard of sentiment analysis lately list of pos and negative words - manually created - approx. 6800


positives <- readLines("Positive-Words.txt")

negatives <-  readLines("Negative-Words.txt")



library("twitteR")

#-tweets for companies - may not get the full 900

bayertweets = searchTwitter("#bayer", n=200, lang="en")
pfizertweets = searchTwitter("#pfizer", n=200, lang="en")
rochetweets = searchTwitter("#roche", n=200, lang="en")
novartistweets = searchTwitter("#novartis", n=200, lang="en")

#get text from the tweets


bayer_txt = sapply(bayertweets, function(x) x$getText())
pfizer_txt = sapply(pfizertweets, function(x) x$getText())
roche_txt = sapply(rochetweets, function(x) x$getText())
novartis_txt = sapply(novartistweets, function(x) x$getText())

#how many tweets

nd = c(length(bayer_txt), length(pfizer_txt), length(roche_txt), length(novartis_txt))

#-comine the texts

pharma = c(bayer_txt, pfizer_txt, roche_txt, novartis_txt)

#-apply function score.sentiment

scores = score.sentiment(pharma, positives, negatives, .progress='text')

#add variables to data frame

scores$company = factor(rep(c("bayer", "pfizer", "roche", "novartis"), nd))

head(scores)

b <- boxplot(score~company, data=scores,main="Distribution of sentiment scores assigned to 4 pharma companies",
             xlab="Company",ylab="Score",col=c("red","yellow","blue","green"))

# Saving the boxplot to the working directory

dev.copy(png,'boxplot_pharma.png')

dev.off()

library("lattice")


h <- histogram(data=scores, ~score|company, main="Sentiment Analysis of 4 pharma companies", xlab="", sub="Sentiment Score")

# Saving the histogram to the working directory

print(h)

dev.copy(png, filename='histogram_countries.png')

dev.off()




