# setwd("/Users/masahiro/Documents/Brown Courses/Term 7/PHP2560/Japan")

fLatLngTweets <- function (tweets) {
  return(tweets[-which(is.na(tweets$place_lat)),])
}