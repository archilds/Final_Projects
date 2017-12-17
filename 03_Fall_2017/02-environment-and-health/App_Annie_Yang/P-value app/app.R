library(shiny)

ui <- fluidPage(
  
  titlePanel("P-value calculator for normal standard distribution"),
  
  br(),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("z_value", "Z value", 
                   value = 1 ),
      br(),
      hr(),
      br(),
      
      actionButton("show_results", "An action button") # The app will calculate the P-value after you 
                                                        # click the action button
    ),
    
    mainPanel(
      h2(textOutput("Pvalue"))
    )
    
  )
)

server <- function(input, output) {
  
  output$Pvalue <- renderText({
    input$show_results
    isolate({
        paste("P value:", pnorm(-input$z_value),sep=" ") # Calculate P value
    })
    })
}

shinyApp(ui = ui, server = server)