if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# install downloadtweets from github
devtools::install_github("kevincoakley/rehydratoR", ref = "0.5.0")

library(rehydratoR)

saved_tweets <- "tweets.Rda"
tweet_ids_file <- "ids.txt" # Example from https://archive.org/details/gaza-tweets

# Set the working directory
setwd('/home/rstudio/')

# If saved_tweets exist, load the tweets from disk, otherwise download the tweets from Twitter
if(file.exists(saved_tweets)){
  load(saved_tweets)
} else {
  # Get Twitter api keys from https://apps.twitter.com
  consumerKey <- ''
  consumerSecret <- ''
  accessToken <- ''
  accessTokenSecret <- ''

  # Read tweet ids
  tweet_ids <- data.frame(read.table(tweet_ids_file, numerals = 'no.loss'))

  # Download tweets
  tweets <- rehydratoR(consumerKey, consumerSecret, accessToken, accessTokenSecret, tweet_ids)

  # Save tweets to disk
  save(tweets, file=saved_tweets)
}

show(tweets)
