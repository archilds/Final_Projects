function(input, output, session) {

  views <- reactiveValues(lng = -93.85, lat = 37.45, zoom = 4)
  bins <- reactiveValues(numbins=5)
  
  dataset <- reactive({
    inFile<-input$file1
    default.Data<-input$defaultDataButton
    if(!is.null(inFile)) {
      read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    } else if(as.numeric(default.Data)>0){
      read.csv("State Data.csv", header=input$header, sep=input$sep, quote=input$quote)
    } else{
      return(NULL)
    }
  })
  
  #Updates the select input box based on the dataset that was uploaded
  observe({
    dt = dataset()
    ## Decide later what to do with the data, here we just fill
    if(!is.null(dt)){
      # dt <-subset(dt, select=-c(Region))
      dt <- dt[,-1]
      updateSelectInput(session, "variable", choices=names(dt))
    }
  })
  
  # dataInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(sentimentData[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   
  #   subset(sentimentData,
  #          latitude >= latRng[1] & latitude <= latRng[2] &
  #            longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
  plotInputTweets <- eventReactive(input$button,{
    # source("getTweets.R")
    keyword <- input$keyword
    keyword <-unlist(strsplit(keyword, ","))
    time <-input$timeout
    
    source("fGetTweets.R")
    source("fShortenTweets.R")
    source("fLatLngTweets.R")
    source("fCleanTweets.R")
    a <- suppressWarnings(fGetTweets(keyword,time))
    b <- fShortenTweets(a)
    tweets.df <- fLatLngTweets(b)
    tweets.short <- fCleanTweets(tweets.df)
    
    # views$lat <- (max(tweets.short$lat,na.rm=T)+min(tweets.short$lat,na.rm=T))/2
    # views$lng <- (max(tweets.short$lng,na.rm=T)+min(tweets.short$lng,na.rm=T))/2
    # views$zoom <-4
    
    # leafletProxy("mymap", data = zipdata) %>%
    #   clearShapes() %>%
    #   addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
    #              stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #   addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #             layerId="colorLegend")
    return(tweets.short)
  })
  
  observeEvent(input$button,{
    c <- plotInputTweets() # data frame
    popup.url <- as.character(1:nrow(c))
    popup.url <-
      paste0("<strong>text: </strong> <a href='",c$url,"'> ", c$text,"</a>")
    popup.url[which(is.na(c$url))] <- 
      paste0("<strong>text: </strong>", c$text)
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
    
    # Updates the map by adding markers after tweets are called
    updated_map <- 
      leafletProxy("mymap") %>% 
      addTiles() %>%
      addCircleMarkers(
        lat=c$lat,
        lng=c$lng,
        radius=c$radius*7.5,
        color=c$color,
        weight=1,
        opacity=1,
        popup=popup) %>%
      addLegend(labels = c("Happy","Neutral","Sad"), colors = c("red","green","blue"))
    updated_map
  })

  observeEvent(input$variable,{
    dt = dataset()
    bins$numbins <- input$obs
    #pal <- colorQuantile("Greens", NULL, n=5)
    pal <- colorQuantile("YlGn", NULL, n = bins$numbins)
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    if(!is.null(dt)) {
      dt <-dt[,-1]
      leafletProxy("mymap", data = mapStates) %>%
        addTiles()%>%
        addPolygons(fillColor = ~pal(as.integer((dt[,input$variable]))), color = "#BDBDC3", weight = 1,stroke = FALSE)
    }
  })  
  
  # first map
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = views$lng, lat = views$lat, zoom = views$zoom)
    
    
    #%>%addPolygons(fillColor = ~pal(test$Population), color = "#BDBDC3", weight = 1,stroke = FALSE)
    
    # mapStates = map("state", fill = TRUE, plot = FALSE)
    # leaflet(data = mapStates) %>% addTiles() 
    #%>%addPolygons(fillColor = ~pal(test$Population), color = "#BDBDC3", weight = 1,stroke = FALSE)
  })
  output$usmap2 <- renderLeaflet({
    print(plotInputTweets())
  })
  
  output$tweetsTable <- renderDataTable({
    plotInputTweets()
  })
  
  output$dataTable <- renderDataTable({
    dataset()
  })
  
  # observeEvent(c(input$variable,input$button), {
  #   dt = dataset() # dataset from the manual input
  #   c <- plotInputTweets() # dataset from the tweets
  #   if(!is.null(dt)){
  #     dt <- dt[,-1] # to check if there is data or not (if it does, then proceed)
  #     source("find_append.R")
  #     stateTweetFreq <- appendStateToDf(c)
  #     stateData <- dataset()
  #     Statesdf <- addColToData(stateTweetFreq, stateData, input$variable) # call the addColToData function
  # 
  #     plot(Statesdf[input$variable], Statesdf[stateTweetFreq], xlab="State Population", ylab="Tweet Counts", main="Scatterplot of State Pop and Tweet Counts about Tesla", col="red")
  #     fit <- lm(Statesdf[stateTweetFreq] ~ Statesdf[input$variable])
  #     summary(fit)
  #   }
  # 
  # 
  #   # Statesdf <- addCol(Statesdf,"State Data.csv")
  #   #
  #   # plot(Statesdf$Population, Statesdf$tweetFreq, xlab="State Population", ylab="Tweet Counts", main="Scatterplot of State Pop and Tweet Counts about Tesla", col="red")
  #   # fit <- lm(Statesdf$tweetFreq ~ Statesdf$Population)
  #   # summary(fit)
  # 
  # })
  
  output$newplot <- renderPlot({
      dt = dataset() # dataset from the manual input
      c <- plotInputTweets() # dataset from the tweets
      if(!is.null(dt)){
        dt <- dt[,-1] # to check if there is data or not (if it does, then proceed)
        source("find_append.R")
        stateTweetFreq <- appendStateToDf(c)
        stateData <- dataset()
        Statesdf <- addColToData(stateTweetFreq, stateData, input$variable) # call the addColToData function
        
        plot(Statesdf[input$variable], Statesdf[stateTweetFreq], xlab="State Population", ylab="Tweet Counts", main="Scatterplot of State Pop and Tweet Counts about Tesla", col="red")
      }
  })
  
  
  
  output$Phist <- renderPlot({
    hist(rpois(1000,1))
  })
  
  output$Scatter <- renderPlot({
    x <- runif(100)
    y <- 2*rnorm(100)+1
    plot(x,y)
    abline(x,y,col="red")
  })
    
  output$downloadData <- downloadHandler(
    filename = function() {
      name <- "data_map"
      paste(name, '.png', sep='')
    },
    content = function(data_map) {
      ggsave(data_map, plot=plotInput(), device="png")
    }
  )
  session$onSessionEnded(function() {
    if(file.exists("tweets.json")){
      file.remove("tweets.json")
    }
    stopApp()
  })
}
