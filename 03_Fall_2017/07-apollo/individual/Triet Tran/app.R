library(shiny)

ui <- fluidPage(
  titlePanel("Simple Z score conversion for two-tailed test"),
  sidebarLayout(
    sidebarPanel(
      numericInput("zInput", "Z-score", 0)
    ),
    mainPanel(
      textOutput("p_value")
    )
  )
)

server <- function(input, output) {
  output$p_value <- renderText({ 
    paste("The p-value is", 2*pnorm(-abs(input$zInput)))
  })
}

shinyApp(ui = ui, server = server)