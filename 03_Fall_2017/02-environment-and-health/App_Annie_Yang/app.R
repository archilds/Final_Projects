library(shiny)

# install necessary packages

source("R/install_packages.R")

# Import data
glb_temp<-get(load(file = "data/glb_temp.rda"))
country_temp<-get(load(file = "data/country_temp.rda"))
state_temp<-get(load(file = "data/state_temp.rda"))
annual_aqi<-get(load(file = "data/annual_aqi.rda"))
aqi_pollutant<-get(load(file = "data/aqi_pollutant.rda"))

ui <- navbarPage("Temperatures and AQI",
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
                 ),
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
}

shinyApp(ui = ui, server = server)