#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("P-Value Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #Display a slider bar with starting value at 0 and increments of .01 in either direction. 
         sliderInput("z",
                     "Z-Score",
                     min = -4,
                     max = 4,
                     value = 0, 
                     step=0.01)
        
      ),
      
      # Show a text output of the P-Value
      mainPanel(
         textOutput("PValue")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 #Calculate the P-Value for the upper tail rounded to 3 sig figs.   
   output$PValue <- renderText({
      paste("P-Value of the Upper Tail for this Z-Score is:" ,signif(pnorm(input$z, lower.tail = FALSE),3))
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

