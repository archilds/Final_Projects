library(shiny)

ui <- fluidPage(
  headerPanel("z for p" ),

  numericInput("num", label = "enter z to find p value ", value = 0),
  div("p-value:"),
  br(),
  textOutput("pValue")


)

server <- function(input, output) {
  output$pValue <- renderText({
    # for a 2 tailed
    pVal <- 2*pnorm(abs(input$num),lower.tail=FALSE)
    return(pVal)

  })

}

shinyApp(ui, server)
