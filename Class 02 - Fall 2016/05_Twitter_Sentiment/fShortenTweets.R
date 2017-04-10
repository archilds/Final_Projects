# setwd("/Users/masahiro/Documents/Brown Courses/Term 7/PHP2560/Japan")

fShortenTweets <- function (tweets) {
  return(tweets[c("text","created_at","place_lat","place_lon","url")])
}