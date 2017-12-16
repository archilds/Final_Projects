library(shiny)
library(plotly)
library(dplyr)
AirQuality_Tracking <- read.csv("data-raw/AirQuality_Tracking.csv")
ui <- fluidPage(headerPanel("AirQuality"),
                sidebarPanel(
                  numericInput("year", "Year", 2005, min = 1999, max = 2013), #Choosing year you want to observe
                  radioButtons("color","Color",choices = c('Blues','Reds','Purples'), 
                               selected = 'Purples') #Choosing different state background color
                ),
                mainPanel(plotlyOutput("map"))
                )
server <- function(input, output) {
  source("R/statemap.R")
  output$map <- renderPlotly(checkAirQuality(data = AirQuality_Tracking,year=input$year, 
                                             Color = input$color))
}
shinyApp(ui = ui, server = server)