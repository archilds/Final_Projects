# setwd("/Users/masahiro/Documents/Brown Courses/Term 7/PHP2560/Japan")

fGetTweets <- function (keyword, timeout) {

  library(streamR)
  library(RCurl)
  library(RJSONIO)
  library(stringr)
  library(streamR)
  
  if(file.exists("tweets.json")){
    file.remove("tweets.json") 
  }
  
  load("my_oauth.Rdata")
  filterStream(file.name = "tweets.json",
               track = keyword,
               language = "en",
               timeout = timeout,
               oauth = my_oauth)
  
  tweets.df <- parseTweets("tweets.json", simplify=FALSE)
  return(tweets.df)
   
}