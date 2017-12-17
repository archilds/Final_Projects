library(shiny)
library(ggplot2)

# define server function
server <- function(input, output) {
  output$header <- renderText({
    switch(input$dist_tabs,
           'Normal' = sprintf('Normal distribution with x = %.1f, mean = %.1f, sd = %.1f', 
                              input$x, input$norm_mean, input$norm_sd)
    )
  })
  
  
  output$pdf <- renderPlot({
    dat <- switch(input$dist_tabs,
                  data.frame(x=seq(input$norm_mean-5,input$norm_mean+5, length=200),
                             y=dnorm(seq(input$norm_mean-5,input$norm_mean+5, length = 200) , 
                                     input$norm_mean, input$norm_sd))
    )
    
    ggplot(dat, aes(x, y)) + 
      geom_line(size=1.2) + 
      theme_bw() + xlab('x') + ylab('') + 
      geom_vline(xintercept = input$x, colour = "#E69F00", size = 1.3) +
      ggtitle(paste('P = ', pnorm(input$x,input$norm_mean, input$norm_sd)) )
  })
  
}

# define ui function
ui <- fluidPage(
  titlePanel('Normal Probability Distributions'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type='tabs', id='dist_tabs',
                  tabPanel('Choose x, and then specify the mean and sd', 
                           br(),
                           p('pdf:'),
                           code('(2*pi*sig)^(-1/2) * exp(-((x-mu)^2) / (2*sig^2))'),
                           br(), br(),
                           sliderInput('x', 'x:',  min = -10, max = 10, 
                                       value=1, step=0.01),
                           sliderInput('norm_mean', 'mean:', min=-5, max=5,
                                       value=0, step=0.1),
                           sliderInput('norm_sd', 'Sd:', min=0.1, max=5,
                                       value=1, step=0.1)
                  )
      )
    ),
    
    mainPanel(
      textOutput('header'),
      plotOutput('pdf', width='500px', height='250px')
    )
  )
)

# run app
shinyApp(ui = ui, server = server)