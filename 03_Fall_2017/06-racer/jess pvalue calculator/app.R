#Shiny app to convert a z-score and return a right-tailed probability (one-sided p-value)
#Jess Kaminsky 
#PHP2560 due: 12/06/2017

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Convert Z-scores to p-values"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("zscore",
                     "Z-Score:",
                     value = ""),
         submitButton(text = "Calculate p-value")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h1(textOutput("pvalue"))
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$pvalue <- renderText({ x <- 1 - pnorm(as.numeric(input$zscore))
   paste("P(Z >= ", input$zscore, ") = ", x)
   })
  }

# Run the application 
shinyApp(ui = ui, server = server)

