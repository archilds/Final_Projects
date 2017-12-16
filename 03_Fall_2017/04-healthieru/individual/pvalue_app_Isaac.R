library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Z Score to P Value Converter"),
   
   # Sidebar with a slider input for z score
   sidebarLayout(
      sidebarPanel(
         sliderInput("zscore","Z Score",min = -4,max = 4,value = 0, step=0.01)
      ),
      
      # Show resulting p value from input z score
      mainPanel(
         textOutput("pval"),
         tags$head(tags$style("#pval{color: green;
                                 font-size: 40px;
                                 }"
         ))
      )
   )
)


server <- function(input, output) {
   
   output$pval <- renderText({
       # Computes p value for upper tail and rounds to four decimal places
       paste("P Value (Upper Tail) is: ",round(pnorm(input$zscore,lower.tail=FALSE), 4))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
 
