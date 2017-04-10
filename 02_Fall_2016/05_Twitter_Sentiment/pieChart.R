library(shiny)

server <- function(input, output, session) {
  inFile = reactive({ return (read.csv(file.choose(), header = TRUE))})
  
  observe({
    updateSelectInput(session, "pieData", choices = names(inFile()))
    updateSelectInput(session, "pieGroups", choices = names(inFile()))
  })
  
  output$plot = renderPlot({
    piePlotData = aggregate(. ~ Gender, inFile, sum)
    pie(piePlotData[[input$pieData]], labels = piePlotData[[input$pieGroups]])
  })
}

ui <- pageWithSidebar(
  headerPanel("Grouped Pie Chart"),
  sidebarPanel(
    selectInput("pieData", "Columns in pie", "Update"),
    selectInput("pieGroups", "Groups for pie", "Update")
  ),
  mainPanel(
    plotOutput("plot")
  )
)

shinyApp(ui = ui, server = server)