if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# install downloadtweets from github
devtools::install_github("kevincoakley/rehydratoR", ref = "0.5.0")

library(rehydratoR)
library(jsonlite)

tweet_ids_file <- "ids.txt" # Example from https://archive.org/details/gaza-tweets

# Set the working directory
setwd('/home/rstudio/')

# Get Twitter api keys from https://apps.twitter.com
consumerKey <- ''
consumerSecret <- ''
accessToken <- ''
accessTokenSecret <- ''

# Read tweet ids
tweet_ids <- data.frame(read.table(tweet_ids_file, numerals = 'no.loss'))

# Download tweets
rehydratoR(consumerKey, consumerSecret, accessToken, accessTokenSecret, tweet_ids, 'example')

tweets <- fromJSON('example_001.json')
show(tweets)
