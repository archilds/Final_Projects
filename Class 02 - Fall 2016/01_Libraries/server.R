source("check_packages.R")
check_packages(c("shiny","DT","broom","dplyr", "RColorBrewer","gridExtra","ggplot2")) 

StateNameList <- data.frame(abb = c(state.abb, "AS", "DC", "GU", "MP", "PR", "VI"), 
                            name = c(state.name, "American Samoa", "District of Columbia", 
                                   "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands"))

RegressionVariableList <- data.frame(abb=c("POPU_LSA", "TOTSTAFF", "TOTINCM", "BKVOL", "EBOOK", "AUDIO_DL", "VIDEO_DL", "VISITS", "REGBOR", "TOTCIR"),
                                     name=c("Legal Service Area Population","Total Paid Employees","Total Operating Income","Print Materials","Electronic Books","Downloadable Audio Titles","Downloadable Video Titles","Total Library Visits","Total Card Holders","Total Circulation Transactions"))


# SERVER START

shinyServer(function(input, output, session) {
  
  load("subdata.rda")

  observe({
    input$state
    # update list of library based on the state
    updateSelectInput(session, "lib", "LIBRARY", 
                      choices = c(paste(pupld[(pupld$year == input$year & pupld$STABR==input$state), 'LIBNAME'])) )
  })
  
  # SUMMARY OUTPUT REACTIVE HEADERS
  output$state <- renderText({ 
    paste(StateNameList[StateNameList$abb == input$state, c('name')])
  })
  output$lib <- renderText({ 
    input$lib
  })
  
  # extract summarized information of the interested state
  stateFact <- reactive(pusum[(pusum$year == input$year & pusum$STABR == input$state), ])
  # extract detailed information of all the libraries in the interested state
  stateLibs <- reactive(pupld[(pupld$year == input$year & pupld$STABR== input$state), ])
  # extract detailed information of the chosen libarary 
  libFact <- reactive(pupld[(pupld$year == input$year & pupld$STABR== input$state & pupld$LIBNAME == input$lib), ])
  
  
  
  output$pieState_material <- renderPlot({
    
    stateFact <- stateFact()
    
    number <- stateFact[, c("EBOOK", "VIDEO_PH", "VIDEO_DL", "AUDIO_PH", "AUDIO_DL")]
    data.pie <- data.frame(Categories = c("E-book", 
                                          "Video (physical)", "Video (downloadable)",
                                          "Audio (physical)", "Audio (downloadable)"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set3")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, pt.cex=0.75, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8, pt.cex=0.75,fill = piecols)
  })
  
  output$pieState_expense <- renderPlot({
    stateFact <- stateFact()
    number <- stateFact[, c("STAFFEXP", "PRMATEXP", "ELMATEXP", "OTHMATEX", "OTHOPEXP")]
    data.pie <- data.frame(Categories = c("Staff Expenditures", 
                                          "Print Materials",
                                          "Electronic Materials",
                                          "All Other Materials",
                                          "Other Expenses"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set1")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8, pt.cex=0.75,fill = piecols)
  })
  
  
  output$tableState <- renderTable({
    stateFact <- stateFact()
    stateLibs <- stateLibs()
    
    Item <- c("Number of Libraries", "Population of the Legal Service Area",
              "Operating Revenue from State Government", "Percentage of Urban Libraries",
              "Cardholders")
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    Value <- c(as.integer(nrow(stateLibs)), 
               as.integer(stateFact$POPU_LSA), 
               as.integer(stateFact$STGVT), 
               percent(mean(stateLibs$LOCALE %in% c(11,12,13))),
               as.integer(stateFact$REGBOR))
    data.frame(Item, Value)
  })
  
  output$tableLib <- renderTable({
    libFact <- libFact()
    
    Item <- c("Name of Library", "Address of Library",
              "Zip Code", 
              "Population of the Legal Service Area",
              "Annual Library Visits",
              "Cardholders", "Number of Internet Computers",
              "Geographic Location"
    )
    Value <- c(paste(libFact$LIBNAME), paste0(libFact$ADDRESS, ", ", libFact$CITY), 
               libFact$ZIP, libFact$POPU_LSA,
               libFact$VISITS, libFact$REGBOR,
               libFact$GPTERMS, 
               if (libFact$LOCALE %in% c(11,12,13)){
                 "City"
               } else if (libFact$LOCALE %in% c(21,22,23)){
                 "Suburb"
               } else if (libFact$LOCALE %in% c(31,32,33)){
                 "Town"
               } else {
                 "Rural"
               })
    data.frame(Item, Value)
    
  })
  
  output$pieLib_material <- renderPlot({
    
    libFact <- libFact()
    
    number <- libFact[, c("EBOOK", "VIDEO_PH", "VIDEO_DL", "AUDIO_PH", "AUDIO_DL")]
    data.pie <- data.frame(Categories = c("E-book", 
                                          "Video (physical)", "Video (downloadable)",
                                          "Audio (physical)", "Audio (downloadable)"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set3")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8,pt.cex=0.75,fill = piecols)
  })
  
  output$pieLib_expense <- renderPlot({
    
    libFact <- libFact()
    
    number <- libFact[, c("STAFFEXP", "PRMATEXP", "ELMATEXP", "OTHMATEX", "OTHOPEXP")]
    data.pie <- data.frame(Categories =  c("Staff Expenditures", 
                                           "Print Materials",
                                           "Electronic Materials",
                                           "All Other Materials",
                                           "Other Expenses"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set1")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8,fill = piecols)
  })
  
  
  
  # REGRESSION INPUT
  
  predictors <- reactive({input$predictor_vars})
  
  outcome <- reactive({input$outcome_var})
  
  pupld$Region <- factor(pupld$OBEREG,
                         levels = c(1,2,3,4,5,6,7,8,9), labels = c("New England",
                                                                   "Mid-Atlantic", "Great Lakes", "Plains", "Southeast", "Southwest",
                                                                   "Rocky Mountains", "Far West", "Territories"))
  
  
  output$scatterplot <- renderPlot( {

    outcome <- outcome()
    predictors <- predictors()

    if (length(predictors)==1){
      ggplot(data=pupld, aes(x = get(predictors),y = get(outcome))) + geom_point(aes(colour = Region)) +
        geom_smooth(method = lm, se = T) + labs(x=predictors, y=outcome)
    }

    else if (length(predictors)==2){
      plots <- list()
      for (i in 1:length(predictors)){
        plots[[i]] <- ggplot(data=pupld, aes(x = get(predictors[i]),y = get(outcome))) + geom_point(aes(colour = Region)) +
          geom_smooth(method = lm, se = T) + labs(x=predictors[i], y=outcome)
      }
      grid.arrange(plots[[1]],plots[[2]], nrow = 2)

    }

    else if (length(predictors)==3){
      plots <- list()
      for (i in 1:length(predictors)){
        plots[[i]] <- ggplot(data=pupld, aes(x = get(predictors[i]),y = get(outcome))) + geom_point(aes(colour = Region)) +
          geom_smooth(method = lm, se = T) + labs(x=predictors[i], y=outcome)
      }
      grid.arrange(plots[[1]],plots[[2]], plots[[3]], nrow = 3)
    }

    else if (length(predictors)==4){
      plots <- list()
      for (i in 1:length(predictors)){
        plots[[i]] <- ggplot(data=pupld, aes(x = get(predictors[i]),y = get(outcome))) + geom_point(aes(colour = Region)) +
          geom_smooth(method = lm, se = T) + labs(x=predictors[i], y=outcome)
        #plot(pupld[predictors][i],pupld[outcome],xlab=predictors[i],ylab=outcome,main=paste("Scatterplot of",outcome,"vs.",predictors[i]))
      }
      grid.arrange(plots[[1]],plots[[2]], plots[[3]],plots[[4]], nrow = 2)
    }

    else if (length(predictors)==5){
      par(mfrow=c(5,1))
      for (i in 1:length(predictors)){
        geom_point(data=pupld, aes(predictors[i],outcome))
      }
    }

    else if (length(predictors)==6){
      par(mfrow=c(6,1))
      for (i in 1:length(predictors)){
        geom_point(data=pupld, aes(predictors[i],outcome))
      }
    }

    else if (length(predictors)==7){
      par(mfrow=c(7,1))
      for (i in 1:length(predictors)){
        geom_point(data=pupld, aes(predictors[i],outcome))
      }
    }

    else if (length(predictors)==8){
      par(mfrow=c(8,1))
      for (i in 1:length(predictors)){
        geom_point(data=pupld, aes(predictors[i],outcome))
      }
    }

    else if (length(predictors)==9){
      par(mfrow=c(9,1))
      for (i in 1:length(predictors)){
        geom_point(data=pupld, aes(predictors[i],outcome))
      }
    }
  })
  
  
  
  # Regression summary
  
  output$regsummary <- renderDataTable({
    
    outcome <- outcome()
    predictors <- predictors()
    reg_formula <- as.formula(paste0(outcome, "~", paste(predictors,collapse="+")))
    linear_model <- lm(formula=reg_formula, data=pupld)
    tidy_model <- tidy(linear_model)
    intervals <- confint_tidy(linear_model) %>%
      mutate_each(funs(round(.,2)))
    regtable <- tidy_model %>%
      select(term, estimate, p.value) %>%
      mutate_each(funs(round(.,2)), -term)
    fulltable <- data.frame(regtable, intervals)
    fulltable
    
  })
  
  
})
  