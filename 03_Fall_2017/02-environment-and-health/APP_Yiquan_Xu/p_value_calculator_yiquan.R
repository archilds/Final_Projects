library(shiny)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  headerPanel("P Value Calculator"),
  hr(),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      div(id="form",
          numericInput("value","Value",value = 5),
      numericInput("s","Standard Deviation", value = 2),
      numericInput("n","sample size", value =20),
      numericInput("xbar","mean", value = 7)),
      actionButton("reset","Reset")
    ),
    mainPanel(
      tags$strong(tags$h2(textOutput("p_value")))
    )
  )
)
server <- function(input, output) {
  observeEvent(input$reset,{reset("form")})
  output$p_value = renderText({
    input$show_results
   
      paste("P value:", 2*pnorm(-abs((input$value-input$xbar)/(input$s/sqrt(input$n)))),sep=" ") # Calculate P value
   
  }
)
}
shinyApp(ui = ui, server = server)