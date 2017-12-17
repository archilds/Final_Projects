library(shiny)

ui <- fluidPage(
  titlePanel("P value Calculator" ),
  sidebarLayout(
    sidebarPanel(
      numericInput("ZInput", "Enter Z-Value: ", 0)
      ),
    mainPanel(
        div("P-Value:"),
        br(),
        textOutput("PValue"))
      )
)

server <- function(input, output) {
  output$PValue <- renderText({
    # for a 2 tailed
    2*pnorm(abs(input$ZInput),lower.tail=FALSE)
    
  })
  
}

shinyApp(ui, server)
