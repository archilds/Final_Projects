library(shiny)

ui <- fluidPage(headerPanel(title = "Caculator"),
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
                          numericInput("year", "Year", 2010, min = 2000, max = 2017)
                 )
                  ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Summary Statistic",tableOutput("stat")),
                     tabPanel("Health",textOutput("health"),
                             htmlOutput("picture"))
                   )
                 )
                )
)
server <- function(input, output) {
  source("R/stat.R")
  source("R/health_status.R")
  output$stat <- renderTable(stat_func(data = annual_aqi, cbsa = input$city))
  output$health <- renderText(health_status(data = annual_aqi, cbsa = input$city, year = input$year,
                                            smoke = input$smoke, exercise = input$exercise,
                                            gene = input$gene))
  output$picture <-renderText({
      c(
        '<img src="',
        "http://drive.google.com/uc?export=view&id=0By6SOdXnt-LFaDhpMlg3b3FiTEU",
        '">'
      )
    })
} #Add a picture
shinyApp(ui = ui, server = server)

