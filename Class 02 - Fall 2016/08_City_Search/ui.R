#This R script will contain codes for the UI of our final project app.
#May merge with server.R if possible.
library(shiny)

#By Shiyuan (Jonathan) Miao: Here's my proposed UI framework: 
#The UI will include two tabs, one for entering inputs and one for reviewing outputs
#The tab for entering inputs will be displayed first upon opening the app
#**Still arranging codes for setting up a second tab (below)**

#tabsetPanel(id = "inTabset",
#            tabPanel("panel1", h2("This is the first panel.")),
#            tabPanel("panel2", h2("This is the second panel."))
#)

library(shinythemes)
library(shinydashboard)



#Main UI
shinyUI(fluidPage(theme = shinytheme("superhero"), 
                  tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
          font-size: 75%;
        }
        "))),
                  
  titlePanel("Where in the U.S. Should You Live?"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
  #Entering inputs, one row per input

      #names(CitiesDB)
      #names(CitiesDB)[2]<-"Size"
      #names(CitiesDB)[3]<-"Economics"
      #names(CitiesDB)[4]<-"Diversity"
      #names(CitiesDB)[5]<-"Cosmopolitanism"
      
           #selectInput("indicator_input1","What kind of weather do you like?", 
                      #c("Snow all day everyday","Sunny all year", "Mild Winters")),
          
           sliderInput("range_1", label = "Population Size:",min = 1, max = 5, value = 1), 
           #An additional droplist may be better, ex. selectInput("range_1_2", label = "Preferred City Size:",
           #                                                      choices = list("Small (<200000)" = 1, "Medium (>200000 and <800000)" = 2, "Large (>800000)" = 3)
           #                                                      selected = 1),
           #The additional input can be addressed in the server file
  
           #selectInput("indicator_input2","City size preferences", 
                       #c("I want to know my neighbors","I'd rather not see anyone for days", "A few people")),
              
            sliderInput("range_2", label = "Economics:",min = 1, max = 5, value = 1),
  
           #selectInput("indicator_input3","What type of industry do you want to work in?", 
                       #c("Education","Medicine", "Business/Finance")),
  
            sliderInput("range_3", label = "Diversity:",min = 1, max = 5, value = 1),
  
  
           #selectInput("indicator_input4","What best describes your political affilitions?", 
            #           c("Conservative","Liberal", "Independent")),
           
            sliderInput("range_4", label = "Cosmopolitanism:",min = 1, max = 5, value = 1),
           
            sliderInput("range_5", label = "Weather:",min = 1, max = 5, value = 1),
           
            textInput("text.job", label = "Job Type", value = "Enter job type..."),
  
            submitButton("Submit")
  ),
  
  # fluidRow(
  #   
  #   column(5,
  #          h4("...Input parameter n"),
  #          h5("...Options for parameter n")
  #          )
  #   
  # ),
  
  #Ranking the priorities of the parameters after finishing the input
  # fluidRow(
  #   
  #   column(2,
  #          h4("Rank of preferences"),
  #          h5("Please all the preferences in order from most to least important  ")),
  #   
  #   column(2, 
  #          numericInput("indicator_input1", 
  #                       label = h5("Weather"), 
  #                       value = NULL)),
  #   
  #   column(2, 
  #          numericInput("indicator_input2", 
  #                       label = h5("Size of city"), 
  #                       value = NULL)),
  #   
  #   column(2, 
  #          numericInput("indicator_input3", 
  #                       label = h5("Industry"), 
  #                       value = NULL)),
  #   
  #   column(2, 
  #          numericInput("indicator_input4", 
  #                       label = h5("Political Preferences"), 
  #                       value = NULL))
  #   
  #   # column(2, 
  #   #        numericInput("indicator_inputN", 
  #   #                     label = h5("...Input parameter n"), 
  #   #                     value = NULL))
  #   
  # ),
  # 
  # #The final submit button
  # fluidRow(
  #   
  #   column(5, h5("When finished entering information, click \"Submit\", then click on \"the second tab\"")),
  #   column(5, submitButton("Submit"))
  #   
  # )
  #   ),
  
  #Test output print that demonstrates ranking; delete when no longer needed
  #mainPanel( plotOutput("myplot"))
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("About", 
                         h3("About the App"),
                         p("Graduate school was fun - we get it! But now it's time to decide",
                           "where you want to move after graduation. Worry not, this app", 
                           "predicts the top 5 cities in the U.S. that you should be living",
                           "in and some jobs to which you could apply based on a few inputs."),
                         h3("Instructions"),
                         p("To the left, you will see four attributes of a city: Population Size, Economics, Diversity,",
                           "and Cosmopolitanism. We ask you to rank the importance each attribute has on",
                           "where you would like to live on a scale of 1 to 5, 1 being most important",
                           "and 5 being least important."),
                         h3("Attributes Explained"),
                         h4("Population Size"),
                         p("The size of the city population"),
                         h4("Economics"),
                         p("The percentage growth rate over the last five years."),
                         h4("Diversity"),
                         p("The percentage of the population with mixed races."),
                         h4("Cosmopolitanism"),
                         p("The percentage of people who were born in other countries."),
                         h4("Weather"),
                         p("The temperature of the city. Giving this measure a higher preference will return warmer cities. ")
                        ),
                
                tabPanel("Results", tableOutput("values"), plotOutput("values.plots")),
                tabPanel("Plot", plotOutput("myplot")), 
                tabPanel("Job Opportunities", tableOutput("jobtable"),
                         downloadButton('downloadJobs', 'Download')
                         )
    )
  ) 
  )
))
