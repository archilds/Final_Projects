
library(shiny)


ui <- fluidPage(
   
   # Application title
   titlePanel("Standard Normal P-Value Calculator"),
   
   # Input field for z score
   numericInput("num", label = h3("Enter Z Value"), value = 0),
   
   # Output window
   textOutput("pval")
    
   
)


server <- function(input, output) {
  
  output$pval = renderText({
    
    paste("The p value is ", 1 - pnorm(input$num))
    
  }
  )
     
}

# Run the application 
shinyApp(ui = ui, server = server)

