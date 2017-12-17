library(shiny)

ui <- fluidPage(h1("the probability of achieving z value or greater in a standard normal distribution"),
       #Using siderbar layout for data input         
                sidebarLayout(
                  sidebarPanel(
                    textInput("num",
                              "z-value:",
                              value = ""),
                    submitButton(text = "Calculate p-value")
                  ),
                  mainPanel(
                    textOutput("pvalue")
                    
                  )
                )
)



server <- function(input, output) {
  output$pvalue <- renderText({ cbind("p-value",
    #p-value calculation round up to 3
  round(pnorm(as.numeric(input$num), mean = 0, sd=1, lower.tail = FALSE),3))
  })
}

shinyApp(ui = ui, server = server)