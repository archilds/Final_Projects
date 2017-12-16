library(shiny)



# Define UI for application that draws a histogram
fluidPage(
  
  # title
  titlePanel("Z to P-Value  Converter"),
  
  # Sidebar layout - app wireframework
  sidebarLayout(
    # sider bar panel left layout
    sidebarPanel(
      helpText(
        strong(
          "This is a simple app that takes a standard normal statistic and displays the probability of achieving that value or greater in a standard normal distribution. In other words it takes a (z) value and gives a p-value."
        )
        
      ),
      
      
      # type of test
      radioButtons(
        "testType",
        label = h5("Type of Test:"),
        choices = list(
          "Right-Tailed" = "right",
          "Two-Tailed" = "both",
          "Left-Tailed" = "left"
          
        ),
        selected = "both"
      ),
      
      # sider bar panel right  layout
      numericInput("zValue", "Z value for your test statistics:", value = 0)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel(
        "Standard Normal Distribution",
        div(h4(textOutput("pValue")), style = "color:red"),
        plotOutput("stdDistPlot")
      ),
      tabPanel("About",
               div('@TODO'))
    ))
  ))
