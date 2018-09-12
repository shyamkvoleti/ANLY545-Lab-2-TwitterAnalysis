rm(list = ls())

#Install the required packages

if(!require(twitteR)) {install.packages("twitteR")}
if(!require(wordcloud)) {install.packages("wordcloud")}
if(!require(tm)) {install.packages("tm")}

library("twitteR")
library("wordcloud")
library("tm")
library("plyr")

#Read Tokens into R

consumer_key <-  'a6Zeno192ZGt49vQfBT4qI9hH'
consumer_secret <- 'kLy79PNFO1GbR9Y8HjhEdFBFyolX4noCoqDEqG1bEcBDZy9uWu'
access_token <- '1037519686525702144-icE9a7h6xmgRNMSJP5TVG0kLIwjRAx'
access_secret <- 'A1dFFEO5lJGtyoFn0F0vwKSPW0hmZ9X1BoeQdKxyMrEIR'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

#Read data from Twitter into R

num_tweets <- 10000
r_stats <- searchTwitter("#HarrisburgU", n=num_tweets)

#Modify the tweets

#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())

#create corpus - Constructs a text document collection (corpus).
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#Modify the tweets

r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)


myDtm <- TermDocumentMatrix(r_stats_text_corpus, control = list(minWordLength = 2))
findFreqTerms(myDtm, lowfreq=3)    # Find the data with given frequency


#GET USER INFO

user <- getUser("HarrisburgU")
friends <- user$getFriends() # who HU follows
friends_df <- twListToDF(friends)
save(friends_df, file = "hu_friends.RData")

#Followers information
followersIds <- lookupUsers(user$getFollowerIDs())
length(followersIds)

#Extra informations from Tweets
trend <- availableTrendLocations()
head(trend)