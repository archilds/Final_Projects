library(shiny)

# install necessary packages

source("R/install_packages.R")

# Import data
glb_temp <- get(load(file = "data/glb_temp.rda"))
country_temp <- get(load(file = "data/country_temp.rda"))
state_temp <- get(load(file = "data/state_temp.rda"))
annual_aqi <- get(load(file = "data/annual_aqi.rda"))
aqi_pollutant <- get(load(file = "data/aqi_pollutant.rda"))
AirQuality_Tracking <- get(load(file = "data/AirQuality_Tracking.rda"))

aqi <- annual_aqi%>%mutate(Per_CO = `Days CO`/sum(`Days CO`), Per_NO2 = `Days NO2`/sum(`Days NO2`),
                           Per_Ozone = `Days Ozone`/sum(`Days Ozone`), Per_SO2 = `Days SO2`/sum(`Days SO2`))

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))


ui <- navbarPage("Temperatures and AQI",
                 navbarMenu("Air Quality Index",
                            tabPanel("AQI Plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         shinyjs::useShinyjs(), # reset CBSA when you click the button 
                                         div(id = "side-panel",
                                             radioButtons("AnalysisType", "Analysis Selection",
                                                          choices = c("Boxplot of AQI","Health Concern By AQI"),
                                                          selected = "Boxplot of AQI"),
                                             
                                             sliderInput("YearAQI", "Year", min=2000, max=2017, value= c(2000,2017)),
                                             
                                             uiOutput("CBSAcontrols"), # If choose Boxplot, the CBSA input cannot be multiple
                                             
                                             selectizeInput("categoryInput", "Health Concern Level", 
                                                            c("Good","Moderate","UnhealthyForSensitiveGroup","Unhealthy",
                                                              "VeryUnhealthy","Hazardous"), 
                                                            selected = c("Good","Moderate"), multiple = T),
                                             
                                             radioButtons("PlotType", "Plot Selection",
                                                          choices = c("line","bar"),
                                                          selected = "line")
                                         ),
                                         tags$hr(),
                                         actionButton("resetCBSA", "Reset CBSA"),             # Reset Button
                                         actionButton("resetHealth", "Reset Health Concern"),
                                         actionButton("reset_input", "Reset All")
                                       ),
                                       
                                       mainPanel(wellPanel(
                                         conditionalPanel(
                                           condition = "input.AnalysisType == 'Boxplot of AQI'",
                                           plotOutput("BoxPlot")
                                         ),
                                         
                                         conditionalPanel(
                                           condition = "input.AnalysisType == 'Health Concern By AQI'",
                                           plotOutput("HealthConcern")
                                         )                 
                                       ),
                                       tags$hr(),
                                       tableOutput("AQI"))
                                     )),
                            tabPanel("Pollutant AQI Trend",
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         shinyjs::useShinyjs(), # reset CBSA when you click the button 
                                         div(id = "ResetTrend",
                                             selectizeInput("CityInput", "City", unique(aqi_pollutant$city), 
                                                            selected = c("New York"), multiple = T),
                                             
                                             sliderInput("YearP", "Year", min=2000, max=2016, value= c(2000,2016)),
                                             
                                             selectizeInput("Pollutant", "pollutant", c("AverageNo2","AverageO3", "AverageSo2", "AverageCo"), 
                                                            selected = c("AverageNo2"), multiple = T)
                                         ),
                                         tags$hr(),
                                         actionButton("resetCity", "Reset City"),
                                         actionButton("resetPollutant", "Reset Pollutant"),
                                         actionButton("resetAll", "Reset All")
                                       ),
                                       mainPanel(plotOutput("PollutantTrend"))
                                     )
                            ),
                            tabPanel("Cluster", fluidPage(
                              headerPanel('k-means clustering'),
                              tabsetPanel(
                                tabPanel(title="Pollution",
                                         selectInput('xcol', 'X Variable', names(aqi)[20:23]),
                                         selectInput('ycol', 'Y Variable', names(aqi)[20:23],
                                                     selected = names(aqi)[22]),
                                         numericInput('clusters', 'Cluster count', 3,
                                                      min = 1, max = 9),
                                         plotOutput('pollution')
                                ),
                                tabPanel(title="Health",
                                         selectInput('x',"X Variable",names(annual_aqi)[5:10]),
                                         selectInput('y',"Y Variable",names(annual_aqi)[5:10],
                                                     selected = names(annual_aqi)[6]),
                                         numericInput('clus', 'Cluster count', 3,
                                                      min = 1, max = 9),
                                         plotOutput('health')
                                )
                              )
                            )
                            )
                 ),
                 navbarMenu("Map",
                            tabPanel("Air Quality Map",
                              fluidPage(headerPanel("AirQuality"),
                                                          sidebarPanel(
                                                          numericInput("yearaqm", "Year", 2005, min = 1999, max = 2013), #Choosing year you want to observe
                                                          radioButtons("color","Color",choices = c('Blues','Reds','Purples'), 
                                                                         selected = 'Purples') #Choosing different state background color
                                                          ),
                                                          mainPanel(plotlyOutput("map"))
                              )
                            ),
                            tabPanel("Temperature Map",
                          sidebarLayout(
                            sidebarPanel (radioButtons("datasetMap", "Dataset Selection",
                                                       choices = c("GlobalLandTemperaturesByCountry","GlobalLandTemperaturesByState"),
                                                       selected = "GlobalLandTemperaturesByCountry"), # Choose analysis with which dataset
                                          
                                          radioButtons("tempVar", "Temperature Variation", # Select TRUE if you want to analyze temperature
                                                       choices = c("FALSE","TRUE"),        # variation between two year.
                                                       selected = "FALSE"),
                                          
                                          sliderInput("yearVar", "Year for Temperature Variation ", 
                                                      min = 1743, max = 2013,
                                                      value = c(2000, 2013)),             # Use this slider when analyze temperature variation
                                          
                                          numericInput("yearMap", "Year", 
                                                       min = 1743, max = 2013, 
                                                       value = 2000)),                    # Use this input year when you select FALSE in Temperature
                            # Variation button
                            mainPanel(wellPanel(           
                              conditionalPanel(
                                condition = "input.datasetMap == 'GlobalLandTemperaturesByState'",
                                plotOutput("plotMap")
                              ),
                              
                              conditionalPanel(
                                condition = "input.datasetMap == 'GlobalLandTemperaturesByCountry'",
                                plotlyOutput("plotMap2")
                              )                 
                            ))
                          )
                 )
                 ),
                 tabPanel("Calculator",
                          fluidPage(headerPanel(title = "Caculator"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        tabPanel(title="Summary Statistic",
                                                 selectInput("city","City",unique(annual_aqi$CBSA),
                                                             selected = "Providence-Warwick, RI-MA")
                                        ),
                                        tabPanel(title = "health",
                                                 radioButtons("smoke","Smoking",
                                                              choices = c("Yes","No"),
                                                              selected = "Yes"),
                                                 radioButtons("exercise","Exercise",
                                                              choices = c("Yes","No"),
                                                              selected = "Yes"),
                                                 radioButtons("gene","Gene",
                                                              choices = c("Yes","No"),
                                                              selected = "Yes"),
                                                 numericInput("yearcal", "Year", 2010, min = 2000, max = 2017)
                                        )
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Summary Statistic",tags$h3(tableOutput("stat"))),
                                          br(),
                                          br(),
                                          tabPanel("Health",tags$em(tags$strong(tags$h3(textOutput("health_text")))),
                                                   br(),
                                                   hr(),
                                                   br(),
                                                   br(),
                                                   htmlOutput("picture"))
                                        )
                                      )
                                    )
                          )
                 )
                   ,
                 
                 tabPanel("Temperature Trend",
                          sidebarLayout(
                            sidebarPanel(
                              shinyjs::useShinyjs(),
                              div(id = "ResetTemp",
                                  
                                  radioButtons("dataset", "Dataset Selection",
                                               choices = c("GlobalTemperature", "GlobalLandTemperaturesByCountry"),
                                               selected = "GlobalTemperature"), # Choose analysis with which dataset
                                  
                                  radioButtons("type", "Type",
                                               choices = c("Year", "Month", "Year and Month"),
                                               selected = "Year and Month"), # Choose year if you want to analyze
                                  # yearly average temperature.
                                  # Choose month if you want to analyze
                                  # monthly average temperature.
                                  radioButtons("ConfidenceInterval", "Confidence Interval",
                                               choices = c("TRUE", "FALSE"), # Select true if you want to display
                                               selected = "FALSE"),          # confidence interval around smooth
                                  
                                  sliderInput("year", "Year", min = 1743, max = 2013,
                                              value = c(2000, 2013)),
                                  
                                  sliderInput("month", "Month", min = 1, max = 12,
                                              value = c(1, 12)),
                                  
                                  selectizeInput("CountryInput", "Country", unique(country_temp$Country), 
                                                 selected = NULL, multiple = T)
                              ),
                              
                              tags$hr(),
                              actionButton("resetCountry", "Reset Country"),
                              actionButton("resetAll1", "Reset All")
                              
                            ),
                            mainPanel(plotOutput("trendplot"))
                          )
                 )
                 )
                 

server <- function(input, output) {
  source("R/avg_temp.R")
  source("R/map_temp.R")
  source("R/boxplot_aqi.R")
  source("R/aqi_healthconcern.R")
  # Show temperature trend
  
  output$trendplot <- renderPlot({
    
    if (input$dataset=="GlobalTemperature") {
      data <- glb_temp
      if (input$type=="Year") {
        avg_temp(data, year = c(input$year[1]:input$year[2]) , 
                 month = c(input$month[1]:input$month[2]),type=1, con=input$ConfidenceInterval)
        
      } else if (input$type=="Month"){
        avg_temp(data, year = c(input$year[1]:input$year[2]) , 
                 month = c(input$month[1]:input$month[2]),type=2, con=input$ConfidenceInterval)
        
      } else {
        avg_temp(data, year = c(input$year[1]:input$year[2]) , 
                 month = c(input$month[1]:input$month[2]),type=c(1,2), con=input$ConfidenceInterval) 
        
      }
    } else { # When GlobalLandTemperaturesByCountry is selected.
      data <- country_temp
      if (input$type=="Year"){
        avg_temp(data, year = c(input$year[1]:input$year[2]) , month = c(input$month[1]:input$month[2]) ,
                 type=1, country = input$CountryInput, con=input$ConfidenceInterval)
      } else if (input$type=="Month") {
        avg_temp(data, year = c(input$year[1]:input$year[2]) , month = c(input$month[1]:input$month[2]) ,
                 type=2, country = input$CountryInput, con=input$ConfidenceInterval)
      } else {
        avg_temp(data, year = c(input$year[1]:input$year[2]) , month = c(input$month[1]:input$month[2]) ,
                 type=c(1,2), country = input$CountryInput, con=input$ConfidenceInterval)
      }
    }
  }
  )
  
  observeEvent(
    input$resetCountry,{
      shinyjs::reset("CountryInput")
    })
  
  observeEvent(
    input$resetAll1,{
      shinyjs::reset("ResetTemp")
    })
  
  # Show temperature map
  output$plotMap2 <- renderPlotly({
    if (input$datasetMap=="GlobalLandTemperaturesByCountry"){
      data<-country_temp
      if (input$tempVar=="FALSE"){
        temp_country(data,input$yearMap)
      } else {
        temp_country(data, start=input$yearVar[1], end=input$yearVar[2], diff=input$tempVar)
      }
    }
  })
  
  output$plotMap <- renderPlot({
    if (input$datasetMap=="GlobalLandTemperaturesByState"){
      data<-state_temp
      temp_state(data,input$yearMap)
    }
  })
  
  # Show AQI plot
  
  output$CBSAcontrols<-renderUI({
    if (input$AnalysisType=="Boxplot of AQI"){
      selectizeInput("CBSA_aqi", "CBSA", unique(annual_aqi$CBSA), 
                     selected = c("Providence-Warwick, RI-MA"), multiple = F)
    } else {
      selectizeInput("CBSA_aqi", "CBSA", unique(annual_aqi$CBSA), 
                     selected = c("Providence-Warwick, RI-MA"), multiple = T)
    }
  })
  
  output$BoxPlot<-renderPlot(
    {
      if (input$AnalysisType=="Boxplot of AQI"){
        boxplot_aqi(annual_aqi,year=c(input$YearAQI[1]:input$YearAQI[2]), cbsa=input$CBSA_aqi)
      }
    }
  )
  
  output$HealthConcern <- renderPlot({
    if (input$AnalysisType=="Health Concern By AQI"){
      aqi_healthconcern(annual_aqi, cbsa=input$CBSA_aqi, category=input$categoryInput,
                        year=c(input$YearAQI[1]:input$YearAQI[2]),plot=input$PlotType)
    }
  })
  
  
  observeEvent(
    input$reset_input,{
      shinyjs::reset("side-panel")
    })
  
  observeEvent(
    input$resetCBSA,{
      shinyjs::reset("CBSA_aqi")
    })
  
  observeEvent(
    input$resetHealth,{
      shinyjs::reset("categoryInput")
    })
  
  # Show AQI category description
  
  AQI_category <- reactive({load(file = "data/AQI.rda")
    AQI[-7, ]})
  
  output$AQI <- renderTable({
    AQI_category()
  })
  
  # Show AQI Pollutant Trend
  output$PollutantTrend<-renderPlot({
    aqi_pollutant <- aqi_pollutant %>% 
      filter(city %in% input$CityInput)%>%
      filter(year >= input$YearP[1] & year <= input$YearP[2])%>% 
      select(dt,city,input$Pollutant)
    
    aqi_pollutant%>%
      tidyr::gather("id", "Pollutant", (ncol(aqi_pollutant)-length(input$Pollutant)+1):ncol(aqi_pollutant)) %>%
      ggplot(aes_string(x = "dt", y = "Pollutant", colour="city")) +
      geom_line()+
      facet_wrap(~id,scales="free_y")+
      theme_bw()+
      labs(title = "Average AQI By City",
           x="Time",
           y="Average AQI")
  })
  
  observeEvent(
    input$resetAll,{
      shinyjs::reset("ResetTrend")
    })
  
  observeEvent(
    input$resetCity,{
      shinyjs::reset("CityInput")
    })
  
  observeEvent(
    input$resetPollutant,{
      shinyjs::reset("Pollutant")
    })
  
  selectedData <- reactive({
    aqi[, c(input$xcol, input$ycol)]
  })
  Data <- reactive(
    annual_aqi[,c(input$x,input$y)]
  )
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  clus <- reactive({
    kmeans(Data(), input$clus)
  })
  output$pollution <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  output$health <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(Data(),
         col = clus()$cluster,
         pch = 20, cex = 3)
    points(clus()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  source("R/statemap.R")
  output$map <- renderPlotly(checkAirQuality(data = AirQuality_Tracking,year=input$yearaqm, 
                                             Color = input$color))
  
  source("R/stat.R")
  source("R/health_status.R")
  output$stat <- renderTable(stat_func(data = annual_aqi, cbsa = input$city))
  output$health_text <- renderText(health_status(data = annual_aqi, cbsa = input$city, year = input$yearcal,
                                            smoke = input$smoke, exercise = input$exercise,
                                            gene = input$gene))

  output$picture <-renderText({
    c(
      '<img src="',
      "http://drive.google.com/uc?export=view&id=0By6SOdXnt-LFaDhpMlg3b3FiTEU",
      '">'
    )
  })
}

shinyApp(ui = ui, server = server)