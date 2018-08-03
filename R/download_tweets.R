#' Get tweets for given statuses (tweet IDs).
#'
#' @param consumer_key Consumer Key (API Key) from https://apps.twitter.com/
#' @param consumer_secret Consumer Secret (API Secret) from https://apps.twitter.com/
#' @param access_token Access Token from the https://apps.twitter.com/
#' @param access_secret Access Token Secret from https://apps.twitter.com/
#' @param status_ids data frame of tweet IDs
#'
#' @return A tibble of tweets data.
#'
#' @export
#' @importFrom rtweet create_token lookup_statuses
#' @import tibble
#' @importFrom dplyr bind_rows count

download_tweets <- function(consumer_key, consumer_secret, access_token, access_secret, status_ids) {
  create_token(
    app = "Download Tweets",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

  # Make the data frame atomic
  status_ids <- as.character(unlist(status_ids))

  message(sprintf("%s - Total number of Tweet ids: %s", Sys.time(), length(status_ids)))

  # Split the status ids in groups of 89,990 in order to advoid the 90,000/15 min api call limit
  rate_limited_status_ids <- split(status_ids, ceiling(seq_along(status_ids)/89990))

  # Create an empty tibble to store the downloaded tweets
  tweets <- tibble()

  counter <- 1

  for (statuses in rate_limited_status_ids) {
    # Sleep for 15.25 mintues (915 seconds) if this isn't the first time through the loop
    # to avoid the api call limit
    if (counter > 1) {
      message(sprintf("%s - Sleeping for 15 minutes", Sys.time()))
      Sys.sleep(915)
      message(sprintf("%s - Resuming Tweet download", Sys.time()))
    }

    counter <- counter + 1

    # Download the tweets
    for_tweets <- tryCatch({
      lookup_statuses(statuses)
    },
    error = function(cond) {
      message(sprintf("Error: %s", cond))
    },
    warning = function(cond) {
      message(sprintf("Warning: %s", cond))
    })

    tweets <- bind_rows(tweets, for_tweets)

    message(sprintf("%s - Tweets downloaded: %s", Sys.time(), count(tweets)))
  }
  return(tweets)
}
