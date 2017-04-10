setwd("/Users/masahiro/Documents/Brown Courses/Term 7/PHP2560/Project8/Archive 2")

source("fGetTweets.R")
source("fShortenTweets.R")
source("fLatLngTweets.R")
a <- fGetTweets("trump",600)
b <- fShortenTweets(a)
tweets.df <- fLatLngTweets(b)

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
# dataFrame$url[which(is.na(tweets.df$url))] <- "NA"
popup.url <- as.character(1:nrow(dataFrame))
popup.url <-
  paste0("<strong>text: </strong> <a href='",dataFrame$url,"'> ", dataFrame$text,"</a>")
popup.url[which(is.na(dataFrame$url))] <- 
  paste0("<strong>text: </strong>", dataFrame$text)
# popup.url[-which(is.na(dataFrame$url))] <-
#   paste0("<strong>text: </strong> <a href='",dataFrame$url[which(is.na(dataFrame$url))],"'> ", dataFrame$text[which(is.na(dataFrame$url))],"</a>")

dataFrame$radius <- .5+(dataFrame$score_adj)^2*100
dataFrame$radius[dataFrame$radius==0] <- .5
dataFrame$color[dataFrame$sentiment=="happy"] <- "red"
dataFrame$color[dataFrame$sentiment=="neutral"] <- "green"
dataFrame$color[dataFrame$sentiment=="sad"] <- "blue"

# popup <- paste(sep="", "<b><a href='", c$url, "'>", c$url, "</a></b>")

c <- dataFrame
popup <- paste0(popup.url,
                "<br/><strong>score: </strong>",
                paste0(round(c$score_adj*100,3),"%"),
                "<br/><strong>anger: </strong>",
                paste0(round(c$anger_adj*100,3),"%"),
                "<br/><strong>anticipation: </strong>",
                paste0(round(c$anticipation_adj*100,3),"%"),
                "<br/><strong>disgust: </strong>",
                paste0(round(c$disgust_adj*100,3),"%"),
                "<br/><strong>fear: </strong>",
                paste0(round(c$fear_adj*100,3),"%"),
                "<br/><strong>joy: </strong>",
                paste0(round(c$joy_adj*100,3),"%"),
                "<br/><strong>sadness: </strong>",
                paste0(round(c$sadness_adj*100,3),"%"),
                "<br/><strong>surprise: </strong>",
                paste0(round( c$surprise_adj*100,3),"%"),
                "<br/><strong>trust: </strong>",
                paste0(round(c$trust_adj*100,3),"%")
                )

# Adding Markers
my_map <- 
  leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(
    lat=dataFrame$lat,
    lng=dataFrame$lng,
    radius=dataFrame$radius*7.5,
    color=dataFrame$color,
    weight=1,
    popup=popup) %>%
    addLegend(labels = c("Happy","Neutral","Sad"), colors = c("red","green","blue"))
my_map