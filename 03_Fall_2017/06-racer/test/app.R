#source(check_packages.R)

library(shiny)
library(tidyverse)
library(shinythemes)
library(racecar)
library(plotly)
###requires racecar package

ui <- fluidPage(theme = shinytheme("cyborg"),
                ####CSS CODE TO FORMAT THE TWO TAB NAVIGATION BAR
                tags$style(type = 'text/css', '.navbar { background-color:black;
                           font-family: Arial;
                           font-size: 13px;
                           color:black; }',
                           '.navbar-default .navbar-brand {
                           color:red;
                           }',
             
                           ####CSS CODE TO SUPRESS INITIAL WARNING ERRORS WHEN DATA IS NOT UPLOADED
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                
                navbarPage("raceR",
                           
                           ######### tab for looking at one set of data - for one driver or one lap #########       
                           tabPanel("Single", fluidRow(column(width = 4,
                                                              tags$img(src = "logo.jpg")), 
                                                       column(width = 6, 
                                                              tags$h1("raceR", style = "color:red;"))),
                                    ######## csv upload box for single data tab
                                    
                                    fluidRow(column(width = 12,
                                                    fileInput("upload1", label = h4("Upload .csv here")))),
                                    
                                    ######## graph choice drop down for single data tab           
                                    fluidRow(column(width = 12, selectInput("graphtype1", label = h5("Choose Graph"),
                                                                            choices = list("Braking Map" = "braking", "Throttle Position Map" = "throttle",
                                                                                           "Graph of Lap Speed" = "laps", "RPM by Gear" = "rpm_gear", 
                                                                                           "RPM by Speed" = "rpm_speed", "Map of Lap Speed" = "mapspeed", 
                                                                                           "Air to Fuel Ratio vs RPM" = "airfuel", 
                                                                                           "Oil Pressure Map" = "oilpressure"),
                                                                            selected = "mapspeed"))),
                                    
                                    ######## distance slider for single data tab       
                                    fluidRow(column(width = 12,
                                                    sliderInput("distrange1", label = h5("Select Distance"), 
                                                                min = 0,
                                                                max = 3.5, value = c(0, 3.5)))),
                                    ######## graphical output for single data tab         
                                    fluidRow(column(width = 12,
                                                    plotlyOutput("graph1"))), 
                                    verbatimTextOutput("event"),
                                    ######## summary statistics output for single data tab         
                                    fluidRow(column(width = 12,
                                                    tableOutput("table1")))), 
                           
                           ######### tab for looking at two sets of data - compare two drivers or compare two laps of the same driver #########    
                           tabPanel("Compare", 
                                    fluidRow(column(width = 6,
                                                    tags$img(src = "logo.jpg")), 
                                             column(width = 6, 
                                                    tags$h1("raceR", style = "color:red;"))),
                                    ######## left csv upload box for compare data tab      
                                    fluidRow(column(width = 6,
                                                    fileInput("upload2", label = h4("Upload .csv here"))),
                                             ######## right csv upload box for compare data tab  
                                             column(width = 6,
                                                    fileInput("upload3", label = h4("Upload .csv here")))),
                                    
                                    ######## left graph choice drop down for compare data tab
                                    fluidRow(column(width = 4,
                                                    selectInput("graphtype2", label = h5("Choose Graph"),
                                                                choices = list("Braking Map" = "braking", "Throttle Position Map" = "throttle",
                                                                               "Graph of Lap Speed" = "laps", "RPM by Gear" = "rpm_gear", 
                                                                               "RPM by Speed" = "rpm_speed", "Map of Lap Speed" = "mapspeed", 
                                                                               "Air to Fuel Ratio vs RPM" = "airfuel",
                                                                               "Oil Pressure Map" = "oilpressure"),
                                                                selected = "mapspeed")),
                                             ######## distance slider for compare data tab
                                             column(width = 4,
                                                    sliderInput("distrange2", label = h5("Select Distance"), 
                                                                min = 0,
                                                                max = 3.5, value = c(0, 3.5))),
                                             
                                             ######## right graph choice drop down for compare data tab       
                                             column(width = 4,
                                                    selectInput("graphtype3", label = h5("Choose Graph"),
                                                                choices = list("Braking Map" = "braking", "Throttle Position Map" = "throttle",
                                                                               "Graph of Lap Speed" = "laps", "RPM by Gear" = "RPM_gear", 
                                                                               "RPM by Speed" = "RPM_speed", "Map of Lap Speed" = "mapspeed", 
                                                                               "Air to Fuel Ratio vs RPM" = "airfuel",
                                                                               "Oil Pressure Map" = "oilpressure"),
                                                                selected = "mapspeed"))),
                                    ######## left graphical output for compare data tab 
                                    fluidRow(column(width = 6,
                                                    plotlyOutput("graph2")),
                                             ######## right graphical output for compare data tab           
                                             column(width = 6,
                                                    plotlyOutput("graph3"))),
                                    
                                             
                                    ######## left summary statistics output for compare data tab  
                                    fluidRow(column(width = 6,
                                                    tableOutput("table2")),
                                             
                                             ######## right summary statistics output for compare data tab  
                                             column(width = 6,
                                                    tableOutput("table3"))
                                    )
                           ))) 



server <- function(input, output) {
  
  options(shiny.maxRequestSize = 50*1024^2)
  
  ##create plot for single data tab based on user input from dropdown menu
  output$graph1 <- renderPlotly( {
    input_data <- cleanSingleLap(input$upload1$datapath, 1)
    if(input$graphtype1 %in% c("laps")) {
      lapspeed(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("mapspeed")) {
      mapspeed(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("RPM_speed")) {
      RPM_speed(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("RPM_gear")) {
      RPM_gear(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("throttle")) {
      throttle_position(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("braking")) {
      braking_pattern(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("airfuel")) {
      airfuel(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2])
      
    } else if (input$graphtype1 %in% c("oilpressure")) {
      oilpressure(input_data, 1, startdist = input$distrange1[1], enddist = input$distrange1[2]) 
      
    } 
  })
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  ##create left plot for compare data tab based on user input from dropdown menu
  output$graph2 <- renderPlotly({
    input_data <- cleanSingleLap(input$upload2$datapath, 1)
    if(input$graphtype2 %in% c("laps")) {
      lapspeed(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
    } else if (input$graphtype2 %in% c("mapspeed")) {
      mapspeed(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype2 %in% c("RPM_speed")) {
      RPM_speed(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype2 %in% c("RPM_gear")) {
      RPM_gear(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype2 %in% c("throttle")) {
      throttle_position(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype2 %in% c("braking")) {
      braking_pattern(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype2 %in% c("airfuel")) {
      airfuel(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype2 %in% c("oilpressure")) {
      oilpressure(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    }
    
  })
  ##create right plot for compare data tab based on user input from dropdown menu 
  output$graph3 <- renderPlotly({
    input_data <- cleanSingleLap(input$upload3$datapath, 1)
    if(input$graphtype3 %in% c("laps")) {
      lapspeed(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
    } else if (input$graphtype3 %in% c("mapspeed")) {
      mapspeed(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype3 %in% c("RPM_speed")) {
      RPM_speed(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype3 %in% c("RPM_gear")) {
      RPM_gear(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype3 %in% c("throttle")) {
      throttle_position(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype3 %in% c("braking")) {
      braking_pattern(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype3 %in% c("airfuel")) {
      airfuel(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    } else if (input$graphtype3 %in% c("oilpressure")) {
      oilpressure(input_data, 1, startdist = input$distrange2[1], enddist = input$distrange2[2])
      
    }
    
  })
  ##create summary statistics table for single data tab
  output$table1 <- renderTable( {
    input_data <- cleanSingleLap(input$upload1$datapath, 1)
    if(input$graphtype1 %in% c("laps")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        group_by(lap) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype1 %in% c("mapspeed")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype1 %in% c("RPM_speed")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
    } else if (input$graphtype1 %in% c("RPM_gear")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        group_by(Calculated_Gea) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
      
    } else if (input$graphtype1 %in% c("throttle")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        summarise(Ave_Throttle = mean(PE3_TPS), Variance = sd(PE3_TPS))
      
    } else if (input$graphtype1 %in% c("braking")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        summarise(Ave_Break = mean(BPS_Front), Variance = sd(BPS_Front))
      
      
    } else if (input$graphtype1 %in% c("airfuel")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        summarise(Ave_Ratio = mean(PE3_LAMBDA), Variance = sd(PE3_LAMBDA))
      
    } else if (input$graphtype1 %in% c("oilpressure")) {
      input_data %>%
        filter(Distance >= input$distrange1[1]) %>%
        filter(Distance <= input$distrange1[2]) %>%
        summarise(Ave_Lat_Accel = mean(GPS_LatAcc), Ave_Lon_Accel = mean(GPS_LonAcc))
      
    } 
  })
  
  ##create left summary statistics table for compare data tab
  output$table2 <- renderTable( {
    input_data <- cleanSingleLap(input$upload2$datapath, 1)
    if(input$graphtype2 %in% c("laps")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        group_by(lap) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype2 %in% c("mapspeed")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype2 %in% c("RPM_speed")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
    } else if (input$graphtype2 %in% c("RPM_gear")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        group_by(Calculated_Gea) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
      
    } else if (input$graphtype2 %in% c("throttle")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Throttle = mean(PE3_TPS), Variance = sd(PE3_TPS))
      
    } else if (input$graphtype2 %in% c("braking")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Break = mean(BPS_Front), Variance = sd(BPS_Front))
      
      
    } else if (input$graphtype2 %in% c("airfuel")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Ratio = mean(PE3_LAMBDA), Variance = sd(PE3_LAMBDA))
      
    } else if (input$graphtype2 %in% c("oilpressure")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Lat_Accel = mean(GPS_LatAcc), Ave_Lon_Accel = mean(GPS_LonAcc))
      
    }
  })
  ##create right summary statistics table for compare data tab 
  output$table3 <- renderTable( {
    input_data <- cleanSingleLap(input$upload3$datapath, 1)
    if(input$graphtype3 %in% c("laps")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        group_by(lap) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype3 %in% c("mapspeed")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Speed = mean(GPS_Speed), Variance = sd(GPS_Speed))
      
    } else if (input$graphtype3 %in% c("RPM_speed")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
    } else if (input$graphtype3 %in% c("RPM_gear")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        group_by(Calculated_Gea) %>%
        summarise(Ave_RPM = mean(PE3_RPM), Variance = sd(PE3_RPM))
      
      
    } else if (input$graphtype3 %in% c("throttle")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Throttle = mean(PE3_TPS), Variance = sd(PE3_TPS))
      
    } else if (input$graphtype3 %in% c("braking")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Break = mean(BPS_Front), Variance = sd(BPS_Front))
      
      
    } else if (input$graphtype3 %in% c("airfuel")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Ratio = mean(PE3_LAMBDA), Variance = sd(PE3_LAMBDA))
      
    }  else if (input$graphtype3 %in% c("oilpressure")) {
      input_data %>%
        filter(Distance >= input$distrange2[1]) %>%
        filter(Distance <= input$distrange2[2]) %>%
        summarise(Ave_Lat_Accel = mean(GPS_LatAcc), Ave_Lon_Accel = mean(GPS_LonAcc))
      
    }
  })
}


shinyApp(ui = ui, server = server)

