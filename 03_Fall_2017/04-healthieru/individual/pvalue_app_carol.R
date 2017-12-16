#### check for shiny package ####
check_packages = function(names){
  for(name in names){
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org") #if package not installed, install the package
    
    library(name, character.only=TRUE)
  }
}
# Checks to see if required packages are already installed.
check_packages("shiny")  #check these packages

#### load shiny ####
library(shiny)

#### make shiny ####
ui <- fluidPage(
  titlePanel("Probability Under Normal Distribution"),
  sidebarLayout(
    sidebarPanel(
      numericInput("zscore", "Enter Z-Score Here:",  0, -4, 4, .01)), #make input box to enter z-score 
    mainPanel(
      plotOutput("coolplot"), #make plot based on z-score
      br()
    )
  )
)

server <- function(input, output) {
  
  output$coolplot <- renderPlot({
    req(input$zscore)  # check for valid variable 
    prob= 1-round(pnorm(input$zscore), 3) #give probability with the input zscore
    if (input$zscore<=4 && input$zscore>=-4){ #if score is between c(-4,4), fill right of the zscore
      ggplot(data.frame(x=c(-4,4)), aes(x)) +   #lim=c(-4,4)
        stat_function(fun=dnorm, args=list(0,1), color = "black", size = .5, geom="area", fill="grey", alpha = 0.4) +  #add grey background   
        scale_x_continuous(name="Z-Score") +            # Labels x axis "Z-Score"
        theme_classic() +                           # Makes the background white theme
        stat_function(fun=dnorm, args=list(0,1), xlim=c(input$zscore,4), geom="area", fill="blue", alpha = 0.7)+ #fill with blue shade right of zscore
        geom_label(aes(0,.5,label=paste("The probability is", prob, sep=' ')), size=7) #add text box with probability
    }else if(input$zscore>4){ #if score is more than 4, no fill 
      ggplot(data.frame(x=c(-4,4)), aes(x)) +   #lim=c(-4,4)
        stat_function(fun=dnorm, args=list(0,1), color = "black", size = .5, geom="area", fill="grey", alpha = 0.4) +  #add grey background   
        scale_x_continuous(name="Z-Score") +            # Labels x axis "Z-Score"
        theme_classic() +                           # Makes the background white theme
        geom_label(aes(0,.5,label=paste("The probability is 0", sep=' ')), size=7) #add text box with probability
    }else{ #if zscore less than -4, then fill the whole graph 
      ggplot(data.frame(x=c(-4,4)), aes(x)) +   #lim=c(-4,4)
        stat_function(fun=dnorm, args=list(0,1), color = "black", size = .5, geom="area", fill="grey", alpha = 0.4) +   
        scale_x_continuous(name="Z-Score") +            # Labels x axis "Z-Score"
        theme_classic() +                           # Makes the background white theme
        stat_function(fun=dnorm, args=list(0,1), xlim=c(-4,4), geom="area", fill="blue", alpha = 0.7)+ #fill with blue background  
        geom_label(aes(0,.5,label=paste("The probability is 1", sep=' ')), size=7) #add text box with probability
      }
  })

}

shinyApp(ui = ui, server = server)