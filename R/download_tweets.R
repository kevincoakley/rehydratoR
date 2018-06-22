#' Get tweets for given statuses (tweet IDs).
#'
#' @param consumer_key Consumer Key (API Key) from https://apps.twitter.com/
#' @param consumer_secret Consumer Secret (API Secret) from https://apps.twitter.com/
#' @param access_token Access Token from the https://apps.twitter.com/
#' @param access_secret Access Token Secret from https://apps.twitter.com/
#' @param statuses data frame of tweet IDs
#'
#' @return A tibble of tweets data.
#'
#' TODO: Work around twitter rate limits https://github.com/mkearney/rtweet/blob/master/R/statuses.R#L3

download_tweets <- function(consumer_key, consumer_secret, access_token, access_secret, statuses) {
  # Use the rtweet library to access the Twitter API (https://github.com/mkearney/rtweet)
  library(rtweet)

  create_token(
    app = "Download Tweets",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

  # Make the data frame atomic
  statuses <- as.character(unlist(statuses))

  tweets <- tryCatch(
    {
      lookup_statuses(statuses)
    },
    error=function(cond) {
      message("Error: ")
      message(cond)
    },
    warning=function(cond) {
      message("Warning: ")
      message(cond)
    }
  )

  return(tweets)
}
