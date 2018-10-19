#' Get tweets for given statuses (tweet IDs).
#'
#' @param consumer_key Consumer Key (API Key) from https://apps.twitter.com/
#' @param consumer_secret Consumer Secret (API Secret) from https://apps.twitter.com/
#' @param access_token Access Token from the https://apps.twitter.com/
#' @param access_secret Access Token Secret from https://apps.twitter.com/
#' @param status_ids data frame of tweet IDs
#' @param base_filename optional. The base filename to use to save the tweets. If set, the fuction will
#'   write the tweets to files instead of returning the tweets as a variable.
#'
#' @return A tibble of tweets data if base_filename is not defined. Nothing is returned if base_filename is defined
#'   but the tweets are saved to a file for about every 90,0000 tweets.
#'
#' @export
#' @importFrom rtweet create_token lookup_statuses
#' @import tibble
#' @importFrom dplyr bind_rows count
#' @importFrom jsonlite toJSON validate

rehydratoR <- function(consumer_key, consumer_secret, access_token, access_secret, status_ids, base_filename = NULL) {
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

  message(sprintf("%s - Splitting tweets into %s groups", Sys.time(), length(rate_limited_status_ids)))

  counter <- 1
  total_tweets <- 0

  for (statuses in rate_limited_status_ids) {
    total_loop_time <- 0
    loop_start_time <- Sys.time()

    message(sprintf("\n%s - Start group: %s", Sys.time(), counter))

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

    # If base_filename is not defined then save the results to the var tweets to be returned to the
    # user at the end of the function. If base_filename is defined then save the tweets to a file
    # named base_filename_###.json every time though the loop.
    if (is.null(base_filename)) {
      tweets <- bind_rows(tweets, for_tweets)
    } else {
      json <- toJSON(for_tweets)
      validate(json)
      write(json, file = sprintf("%s_%03d.json", base_filename, counter))
    }

    # Calculate and print the total number of tweets downloaded so far
    total_tweets <- total_tweets + count(for_tweets)
    message(sprintf("%s - Total tweets downloaded: %s", Sys.time(), total_tweets))

    loop_end_time <- Sys.time()

    # Calculate the time it took to download the tweets
    total_loop_time = as.integer(difftime(loop_end_time, loop_start_time, units = "secs"))

    # If there are multiple loops, then each loop must take at 15.25 mintues (915 seconds)
    # to avoid the api call limit
    if (total_loop_time < 915 && counter < length(rate_limited_status_ids)) {
      sleep_time <- 915 - total_loop_time
      message(sprintf("%s - Sleeping for %s seconds", Sys.time(), sleep_time))
      Sys.sleep(sleep_time)
      message(sprintf("%s - Resuming Tweet download", Sys.time()))
    }

    counter <- counter + 1
  }
  return(tweets)
}
