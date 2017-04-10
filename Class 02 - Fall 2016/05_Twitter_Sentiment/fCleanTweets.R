fCleanTweets <- function(tweets.df) {
  
  yay <- scan('positive-words.txt',what='character',comment.char=';')
  boo <- scan('negative-words.txt',what='character',comment.char=';')
  combinedLexicon <- read.csv("lexicon.csv",header = FALSE) 
  pos_indx = combinedLexicon[,2] == "positive"
  neg_indx = combinedLexicon[,2] == "negative"
  pos_lex = combinedLexicon[pos_indx,1]
  neg_lex = combinedLexicon[neg_indx,1]
  positive_words  <- c(yay ,pos_lex)
  negative_words <- c(boo ,neg_lex)
  positive_words <- unique(positive_words)
  negative_words <- unique(negative_words)
  
  library(plyr)
  combinedEmotion <- read.csv("sentiment.csv",header = FALSE)
  source("score_sentiment.R")
  source("sentence_length.R")
  dataFrame <- score.sentiment3(tweets.df$text, positive_words, negative_words,combinedEmotion, .progress='none')
  dataFrame$lat<- tweets.df$place_lat
  dataFrame$lng <- tweets.df$place_lon
  dataFrame$url <- tweets.df$url
  dataFrame$radius <- .5+(dataFrame$score_adj)^2*100
  dataFrame$radius[dataFrame$radius==0] <- .5
  dataFrame$color[dataFrame$sentiment=="happy"] <- "red"
  dataFrame$color[dataFrame$sentiment=="neutral"] <- "green"
  dataFrame$color[dataFrame$sentiment=="sad"] <- "blue"
  return(dataFrame)
  
}