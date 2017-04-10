score.sentiment = function(sentences, pos.words, neg.words, emotions.df, .progress='none')
  
{
  require(plyr)
  require(stringr)
  sentiment.df = llply(sentences, function(sentence, pos.words, neg.words,emotions.df) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('[0-9]+', '', sentence)
    # split into words
    word.list = str_split(sentence, '[\ \t\r\n\f]+')
    words = unlist(word.list)
    #make a list of the emotions from the emotionsDf
    anger.words <- as.character(emotions.df$V1[emotions.df$V2==1])
    anticipation.words<- as.character(emotions.df$V1[emotions.df$V3==1])
    digust.words<- as.character(emotions.df$V1[emotions.df$V4==1])
    fear.words<- as.character(emotions.df$V1[emotions.df$V5==1])
    joy.words<- as.character(emotions.df$V1[emotions.df$V6==1])
    sadness.words<- as.character(emotions.df$V1[emotions.df$V7==1])
    surprise.words<- as.character(emotions.df$V1[emotions.df$V8==1])
    trust.words<- as.character(emotions.df$V1[emotions.df$V9==1])
    
    #Create Arrays for the scores of each tweet
    pos.matches = rep(0,1,length(word.list))
    neg.matches = rep(0,1,length(word.list))
    
    anger.matches = rep(0,1,length(word.list))
    anticipation.matches = rep(0,1,length(word.list))
    digust.matches = rep(0,1,length(word.list))
    fear.matches = rep(0,1,length(word.list))
    joy.matches = rep(0,1,length(word.list))
    sadness.matches = rep(0,1,length(word.list))
    surprise.matches = rep(0,1,length(word.list))
    trust.matches = rep(0,1,length(word.list))
    
    # compare our words to the dictionaries of positive & negative terms and computes a score
    # by conversion to boolean type
    for (i in 1:length(word.list)) {
      pos.matches[i] = sum(!is.na(match(word.list[[i]], pos.words)))
      neg.matches[i] = sum(!is.na(match(word.list[[i]], neg.words)))
      
      anger.matches[i] = sum(!is.na(match(word.list[[i]], anger.words)))
      anticipation.matches[i] = sum(!is.na(match(word.list[[i]], anticipation.words)))
      digust.matches[i] = sum(!is.na(match(word.list[[i]], digust.words)))
      fear.matches[i] = sum(!is.na(match(word.list[[i]], fear.words)))
      joy.matches[i] = sum(!is.na(match(word.list[[i]], joy.words)))
      sadness.matches[i] = sum(!is.na(match(word.list[[i]], sadness.words)))
      surprise.matches[i] = sum(!is.na(match(word.list[[i]], surprise.words)))
      trust.matches[i] = sum(!is.na(match(word.list[[i]], trust.words)))
    }
    
    # Calls the length function and divides each score by length
    sentence_len <- dget("sentence_length.R")
    sentence_length<-sentence_len(word.list)
    scores = (pos.matches - neg.matches)/sentence_length
    
    score_anger = anger.matches/sentence_length
    score_anticipation = anticipation.matches/sentence_length
    score_digust = digust.matches/sentence_length
    score_fear = fear.matches/sentence_length
    score_joy = joy.matches/sentence_length
    score_sadness = sadness.matches/sentence_length
    score_surprise = surprise.matches/sentence_length
    score_trust = trust.matches/sentence_length
    
    #Classifies scores based on polarity
    sentiment = scores
    sentiment[sentiment > 0] = 'happy'
    sentiment[sentiment < 0] = 'sad'
    sentiment[sentiment == 0] = 'neutral'
    
    #Creates a data frame with the raw score values
    sentiment_Raw.df = data.frame(text=sentences, score=(pos.matches - neg.matches), sentiment = sentiment,
                              anger = anger.matches, anticipation = anticipation.matches, disgust = digust.matches,
                              fear = fear.matches, joy = joy.matches, sadness = sadness.matches,
                              surprise = surprise.matches, trust = trust.matches)
    
    #Creates a data frame with the length adjusted score values
    sentiment_lenAdj.df = data.frame(text=sentences, score=scores, sentiment = sentiment,
                                     anger = score_anger, anticipation = score_anticipation, disgust = score_digust,
                                     fear = score_fear, joy = score_joy, sadness = score_sadness,
                                     surprise = score_surprise, trust = score_trust)
    
    sentiment.df_list<-list(sentiment_Raw.df,sentiment_lenAdj.df)
      
    return(sentiment.df_list) 
    
    
  }, pos.words, neg.words, emotions.df, .progress=.progress )
  #
  sentiment.df<-as.data.frame(sentiment.df[[1]])
  drops <- c("text.1","sentiment.1")
  sentiment.df =sentiment.df[ , !(names(sentiment.df) %in% drops)]
  names(sentiment.df)<-sub(".[0-9]", "_adj", names(sentiment.df))
  return(sentiment.df)
}


score.sentiment2 = function(sentences, pos.words, neg.words, emotions.df, .progress='none')
  
{
  require(plyr)
  require(stringr)
  sentiment.df = llply(sentences, function(sentence, pos.words, neg.words,emotions.df) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('[0-9]+', '', sentence)
    # split into words
    word.list = str_split(sentence, '[\ \t\r\n\f]+')
    words = unlist(word.list)
    #make a list of the emotions from the emotionsDf
    anger.words <- as.character(emotions.df$V1[emotions.df$V2==1])
    anticipation.words<- as.character(emotions.df$V1[emotions.df$V3==1])
    digust.words<- as.character(emotions.df$V1[emotions.df$V4==1])
    fear.words<- as.character(emotions.df$V1[emotions.df$V5==1])
    joy.words<- as.character(emotions.df$V1[emotions.df$V6==1])
    sadness.words<- as.character(emotions.df$V1[emotions.df$V7==1])
    surprise.words<- as.character(emotions.df$V1[emotions.df$V8==1])
    trust.words<- as.character(emotions.df$V1[emotions.df$V9==1])

    
    # compare our words to the dictionaries of positive & negative terms and computes a score
    # by conversion to boolean type

    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    anger.matches = match(words, anger.words)
    anticipation.matches = match(words, anticipation.words)
    digust.matches = match(words, digust.words)
    fear.matches = match(words, fear.words)
    joy.matches = match(words, joy.words)
    sadness.matches = match(words, sadness.words)
    surprise.matches = match(words, surprise.words)
    trust.matches = match(words, trust.words)
    
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    anger.matches = !is.na(anger.matches)
    anticipation.matches = !is.na(anticipation.matches)
    digust.matches = !is.na(digust.matches)
    fear.matches = !is.na(fear.matches)
    joy.matches = !is.na(joy.matches )
    sadness.matches = !is.na(sadness.matches)
    surprise.matches = !is.na(surprise.matches)
    trust.matches = !is.na(trust.matches)
    
    
    pos.matches = sum(pos.matches)
    neg.matches = sum(neg.matches)
    
    anger.matches = sum(anger.matches)
    anticipation.matches = sum(anticipation.matches)
    digust.matches = sum(digust.matches)
    fear.matches = sum(fear.matches)
    joy.matches = sum(joy.matches )
    sadness.matches = sum(sadness.matches)
    surprise.matches = sum(surprise.matches)
    trust.matches = sum(trust.matches)
    # Calls the length function and divides each score by length
    sentence_length<-length(words)
    scores = (pos.matches - neg.matches)/sentence_length
    
    score_anger = anger.matches/sentence_length
    score_anticipation = anticipation.matches/sentence_length
    score_digust = digust.matches/sentence_length
    score_fear = fear.matches/sentence_length
    score_joy = joy.matches/sentence_length
    score_sadness = sadness.matches/sentence_length
    score_surprise = surprise.matches/sentence_length
    score_trust = trust.matches/sentence_length
    
    #Classifies scores based on polarity
    sentiment = scores
    sentiment[sentiment > 0] = 'happy'
    sentiment[sentiment < 0] = 'sad'
    sentiment[sentiment == 0] = 'neutral'
    
    #Creates a data frame with the raw score values
    sentiment_Raw.df = data.frame(text=sentences, score=(pos.matches - neg.matches), sentiment = sentiment,
                                  anger = anger.matches, anticipation = anticipation.matches, disgust = digust.matches,
                                  fear = fear.matches, joy = joy.matches, sadness = sadness.matches,
                                  surprise = surprise.matches, trust = trust.matches)
    
    #Creates a data frame with the length adjusted score values
    sentiment_lenAdj.df = data.frame(text=sentences, score=scores, sentiment = sentiment,
                                     anger = score_anger, anticipation = score_anticipation, disgust = score_digust,
                                     fear = score_fear, joy = score_joy, sadness = score_sadness,
                                     surprise = score_surprise, trust = score_trust)
    
    sentiment.df_list<-list(sentiment_Raw.df,sentiment_lenAdj.df)
    
    return(sentiment.df_list) 
    
    
  }, pos.words, neg.words, emotions.df, .progress=.progress )
  #
  sentiment.df<-as.data.frame(sentiment.df[[1]])
  drops <- c("text.1","sentiment.1")
  sentiment.df =sentiment.df[ , !(names(sentiment.df) %in% drops)]
  names(sentiment.df)<-sub(".[0-9]", "_adj", names(sentiment.df))
  return(sentiment.df)
}

score.sentiment3 = function(sentences, pos.words, neg.words, emotions.df, .progress='none')
{  

  require(plyr)
  require(stringr)

    # clean up sentences with R's regex-driven global substitute, gsub():
    sentences = gsub('[[:punct:]]', '', sentences)
    sentences = gsub('[[:cntrl:]]', '', sentences)
    sentences = gsub('[0-9]+', '', sentences)
    # split into words
    word.list = str_split(sentences, '[\ \t\r\n\f]+')
    words = unlist(word.list)
    #make a list of the emotions from the emotionsDf
    anger.words <- as.character(emotions.df$V1[emotions.df$V2==1])
    anticipation.words<- as.character(emotions.df$V1[emotions.df$V3==1])
    digust.words<- as.character(emotions.df$V1[emotions.df$V4==1])
    fear.words<- as.character(emotions.df$V1[emotions.df$V5==1])
    joy.words<- as.character(emotions.df$V1[emotions.df$V6==1])
    sadness.words<- as.character(emotions.df$V1[emotions.df$V7==1])
    surprise.words<- as.character(emotions.df$V1[emotions.df$V8==1])
    trust.words<- as.character(emotions.df$V1[emotions.df$V9==1])
    
    #Create Arrays for the scores of each tweet
    pos.matches = rep(0,1,length(word.list))
    neg.matches = rep(0,1,length(word.list))
    
    anger.matches = rep(0,1,length(word.list))
    anticipation.matches = rep(0,1,length(word.list))
    digust.matches = rep(0,1,length(word.list))
    fear.matches = rep(0,1,length(word.list))
    joy.matches = rep(0,1,length(word.list))
    sadness.matches = rep(0,1,length(word.list))
    surprise.matches = rep(0,1,length(word.list))
    trust.matches = rep(0,1,length(word.list))
    
    scores = rep(0,1,length(word.list))
    
    score_anger = rep(0,1,length(word.list))
    score_anticipation = rep(0,1,length(word.list))
    score_digust = rep(0,1,length(word.list))
    score_fear = rep(0,1,length(word.list))
    score_joy = rep(0,1,length(word.list))
    score_sadness = rep(0,1,length(word.list))
    score_surprise = rep(0,1,length(word.list))
    score_trust = rep(0,1,length(word.list))
    
    # compare our words to the dictionaries of positive & negative terms and computes a score
    # by conversion to boolean type
    # Calls the length function and divides each score by length
    sentence_len <- dget("sentence_length.R")
    sentence_length<-sentence_len(word.list)
    for (i in 1:length(word.list)) {
      pos.matches[i] = sum(!is.na(match(word.list[[i]], pos.words)))
      neg.matches[i] = sum(!is.na(match(word.list[[i]], neg.words)))
      
      anger.matches[i] = sum(!is.na(match(word.list[[i]], anger.words)))
      anticipation.matches[i] = sum(!is.na(match(word.list[[i]], anticipation.words)))
      digust.matches[i] = sum(!is.na(match(word.list[[i]], digust.words)))
      fear.matches[i] = sum(!is.na(match(word.list[[i]], fear.words)))
      joy.matches[i] = sum(!is.na(match(word.list[[i]], joy.words)))
      sadness.matches[i] = sum(!is.na(match(word.list[[i]], sadness.words)))
      surprise.matches[i] = sum(!is.na(match(word.list[[i]], surprise.words)))
      trust.matches[i] = sum(!is.na(match(word.list[[i]], trust.words)))
      scores[i] = (pos.matches[i] - neg.matches[i])/sentence_length[i]
      
      score_anger[i] = anger.matches[i]/sentence_length[i]
      score_anticipation[i] = anticipation.matches[i]/sentence_length[i]
      score_digust[i] = digust.matches[i]/sentence_length[i]
      score_fear[i] = fear.matches[i]/sentence_length[i]
      score_joy[i] = joy.matches[i]/sentence_length[i]
      score_sadness[i] = sadness.matches[i]/sentence_length[i]
      score_surprise[i] = surprise.matches[i]/sentence_length[i]
      score_trust[i] = trust.matches[i]/sentence_length[i]
    }

    # scores = (pos.matches - neg.matches)/sentence_length
    # 
    # score_anger = anger.matches/sentence_length
    # score_anticipation = anticipation.matches/sentence_length
    # score_digust = digust.matches/sentence_length
    # score_fear = fear.matches/sentence_length
    # score_joy = joy.matches/sentence_length
    # score_sadness = sadness.matches/sentence_length
    # score_surprise = surprise.matches/sentence_length
    # score_trust = trust.matches/sentence_length
    
    #Classifies scores based on polarity
    sentiment = scores
    sentiment[sentiment > 0] = 'happy'
    sentiment[sentiment < 0] = 'sad'
    sentiment[sentiment == 0] = 'neutral'
    
    #Creates a data frame with the raw score values
    sentiment_Raw.df = data.frame(text=sentences, score=scores*sentence_length, sentiment = sentiment,
                                  anger = anger.matches, anticipation = anticipation.matches, disgust = digust.matches,
                                  fear = fear.matches, joy = joy.matches, sadness = sadness.matches,
                                  surprise = surprise.matches, trust = trust.matches)
    
    #Creates a data frame with the length adjusted score values
    sentiment_lenAdj.df = data.frame(text=sentences, score=scores, sentiment = sentiment,
                                     anger = score_anger, anticipation = score_anticipation, disgust = score_digust,
                                     fear = score_fear, joy = score_joy, sadness = score_sadness,
                                     surprise = score_surprise, trust = score_trust)
    
    sentiment.df = data.frame(text=sentences, score=scores*sentence_length, score_adj=scores, sentiment = sentiment,
                                     anger = anger.matches,anger_adj = score_anger,
                                     anticipation = anticipation.matches, anticipation_adj = score_anticipation,
                                     disgust = digust.matches, disgust_adj = score_digust,
                                     fear = fear.matches, fear_adj = score_fear,
                                     joy = joy.matches, joy_adj = score_joy,
                                     sadness = sadness.matches,sadness_adj = score_sadness,
                                     surprise = surprise.matches,surprise_adj = score_surprise,
                                     trust = trust.matches,trust_adj = score_trust)
     
     
      
     trust = trust.matches
    
  #   sentiment.df_list<-list(sentiment_Raw.df,sentiment_lenAdj.df)
  # 
  # #
  # sentiment.df<-as.data.frame(sentiment.df_list[[1]])
  # drops <- c("text.1","sentiment.1")
  # sentiment.df =sentiment.df[ , !(names(sentiment.df) %in% drops)]
  # names(sentiment.df)<-sub(".[0-9]", "_adj", names(sentiment.df))
  return(sentiment.df)
}
