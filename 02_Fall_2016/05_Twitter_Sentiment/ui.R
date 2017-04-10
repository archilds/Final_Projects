navbarPage(
  theme = shinytheme("slate"),
  collapsible = FALSE,
  fluid = TRUE,
  "Sentiment Analysis of Twitter Data", # title
  id ="main", # navbar ID
  tabPanel("About", p("What can this app be used for?"),

           p(  "o   Our application can be used to examine the sociodemographic characteristics in different states in the US, analyze Twitter data for keywords of interest, conduct sentiment analysis of those Tweets, and understand the relationships between state characteristics and Twitter data."),

           p("  ·         How can we use this app?"),

           p("  o   Users can select their variables of interest for the sociodemographic characteristics for different states and map these data."),

           p("    o   Users can input a keyword for analysis in Twitter and get the frequency of Tweets for that keyword across states and map these data."),

           p("   o   This app will generate mean sentiment scores of the tweets containing the keyword and map these scores across states"),

           p("   o   Users can run simple linear regressions for the associations between the sociodemographic characteristics for each state and tweet frequencies and mean sentiment scores")),


  tabPanel("Interactive map",

                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"), # style sheet for the app
                          includeScript("gomap.js") # suppose to make shiny app zoom in when double clicked
                        ),
                        
                        leafletOutput("mymap", width="100%", height="100%"), # to plot the leaflet plot
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      #Side Panel Title
                                      h2("Sentiment Explorer"),
                                      
                                      #Side Panel Inputs
                                      fileInput('file1', 'Choose CSV File',
                                                accept=c('text/csv',
                                                         'text/comma-separated-values,text/plain',
                                                         '.csv'
                                                )
                                      ),
                                      actionButton("defaultDataButton", "Upload Default Data"),
                                      #Add a Horizontal Rule to the side panel
                                      checkboxInput('header', 'Header', TRUE),
                                      radioButtons('sep', 'Separator',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   ','),
                                      
                                      radioButtons('quote', 'Quote',
                                                   c(None='',
                                                     'Double Quote'='"',
                                                     'Single Quote'="'"),
                                                   '"'),
                                      
                                      tags$hr(),
                                      h5("Search for Tweets"),
                                      textInput("keyword", "Enter Keyword", ""),
                                      numericInput("timeout", "Enter Timeout", 15, min=0, max=500, step=1),
                                      actionButton("button", "Search For Tweets"),
                                      
                                      #Downlaod Plots
                                      tags$hr(),
                                      downloadButton('downloadData', 'Download')
                                      
                                      
                        ),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",

                                      #Add another Horizontal Rule to the side panel
                                      tags$hr(),
                                      h5("Change Map Properties"),
                                      sliderInput("obs", "Color Scale:",
                                                  min = 1, max = 9, value = 5),
                                      selectInput("variable", "Select Variable",""),
                                      htmlOutput("varselect", inline=TRUE)
                                      
                                      
                                      #Side Panel Outputs
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)
                                      
                        )
                    )     


  ),
  
  tabPanel("Table",
           tabsetPanel(
             tabPanel("Tweets Table", dataTableOutput("tweetsTable")),
             tabPanel("Data Table", dataTableOutput("dataTable"))
           )
  ),
  
  tabPanel("Plots",
           tabsetPanel(
             tabPanel("Normal RV Hist", plotOutput("newplot")),
             tabPanel("Poisson RV Hist", plotOutput("Phist")),
             tabPanel("Scatter Plot", plotOutput("Scatter"))
           )
  ),
  
  tabPanel("Analysis"

  ),

  tabPanel("Tutorial",
           p("o Interactive Maps: This is where the user chooses what he/she would like to analyze on a tweet basis in the given map area. Users can choose to upload their own datasets via the “choose CSV File” option or can decide to use some of our own in-built variables by clicking the “Upload Default Data” button. This allows the user to use some of the variables that we have provided (as a template) so that the user can be easily aided in his/her analysis by using important socio-demographic factors provided.  The “Search for Tweets” box allows the user to input their word/character of choice in order to gain a sentiment analysis of what they are looking for. The location of the tweet on the map as well as the sentiment and 8 emotion ratings are provided as a pop up while clicking at the tweet on the map. The radius of the tweet indicator (a circle) helps to understand the degree of sentiment in the emotion. The tweet’s URL is also provided in the pop-up box and can be clicked to access the tweet on the user’s twitter page. The enter timeout allows the user to collect the number of tweets for a user defined period of time from when they press search."),
           p("o Tables: The tweets table tabulates the tweets and creates an analysis of what emotion most strongly represents the tweet.  If the words in the tweet represents an emotion/emotions from our pre-assigned emotions, the tweet is a given a score of 1 for that emotion. Later on these scores are added to give a total score emotion score of the tweet Otherwise, it is a given a 0. Each emotion also has an adjusted score, which divides the score given to the emotion by the length of words. Data Table on the other hand provides an analysis of the demographics we are looking for on a state-wide basis and helps to provide important demographic and socio-demographic information on states for the user, which would aid the user in an analysis and help to understand which variables may play a role in determining the sentiments of tweets."),
           p("o Plots: In the plots section, the app has been designed to display a scatter plot with respect to the tweet frequency. By tabulating and collecting the frequency of tweets, the use of a scatterplot here is efficient to show the relationship of the tweets to specific explanatory variables. Therefore ensuring that the tweet frequency is a viable response variable, in line with the users’ needs. We have also constructed a line of best fit to give the user an idea of what the relationship is possibly like. The addition of a pie chart also helps to understand the variety of emotions within a tweet. As we our using 8 different emotional categories, we found that it would beneficial for the user to understand how positive/negative a tweet is in order to better aid the analysis process.")
  )

)
