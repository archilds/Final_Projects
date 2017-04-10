source("check_packages.R")
check_packages(c("shiny","DT","broom","dplyr", "RColorBrewer")) 

# set default information and load default dataset (year 2014)

load("subdata.rda")
years <- c(2014,2013,2012)
states <- c(paste(unique(pupld$STABR)))
library.default <- c(paste(unique(pupld$LIBNAME)))

#variables <- c("STABR", "LIBNAME", "POPU_LSA", "CENTLIB", "BRANLIB", "LIBRARIA", "TOTSTAFF", "TOTINCM", "STAFFEXP", "PRMATEXP", "ELMATEXP", "OTHMATEX", "TOTEXPCO", "OTHOPEXP", "TOTOPEXP", "BKVOL", "EBOOK", "AUDIO_PH", "AUDIO_DL", "VIDEO_PH", "VIDEO_DL", "DATABASE", "HRS_OPEN", "VISITS", "REGBOR", "TOTCIR", "LOANTO", "LOANFM", "GPTERMS", "PITUSR", "OBEREG", "LOCALE", "MICROF")

# UI START

fluidPage(theme = "bootstrap.css",
    titlePanel("Public Libraries Survey Data Exploration"),
    sidebarLayout(
    sidebarPanel(

# ABOUT SIDEBAR DISPLAY
      
      conditionalPanel(condition="input.conditionedPanels==1",
                       tags$i(h4("Created by Olivia King, Menghan Hu, and Marisa Millenson for PHP 2560 in fall 2016."))
      ),
      
# SUMMARY SIDEBAR DISPLAY

      conditionalPanel(condition="input.conditionedPanels==2",
                       selectInput(inputId = "year", label = "YEAR", choices = years, selected = 2014),
                       br(),
                       selectInput(inputId = "state", label = "STATE", selected = "RI", 
                                   choices = states ),
                       br(),
                       selectInput(inputId = "lib", label = "LIBRARY", selected = "PROVIDENCE PUBLIC LIBRARY", choices = library.default)
      ),
      
# REGRESSION SIDEBAR DISPLAY

conditionalPanel(condition="input.conditionedPanels==3",
                 h5(tags$i("A maximum of four possible predictor variables may be chosen when viewing scatterplots.")),
                 br(),
                 selectInput(inputId = "predictor_vars", label = "PREDICTOR VARIABLES", 
                             choices =  c("POPU_LSA", "TOTSTAFF", "TOTINCM","BKVOL","EBOOK","AUDIO_DL","VIDEO_DL","VISITS","REGBOR","TOTCIR" ), 
                             multiple = TRUE, selected = "POPU_LSA"),
                 br(),
                 selectInput(inputId = "outcome_var", label = "OUTCOME VARIABLE", 
                             choices = c("POPU_LSA", "TOTSTAFF", "TOTINCM","BKVOL","EBOOK","AUDIO_DL","VIDEO_DL","VISITS","REGBOR","TOTCIR" ), 
                             multiple = FALSE, selected = "TOTINCM"),
                 br(),
                 h4("Variable List"),
                 h5("POPU_LSA = Population of the legal service area"),
                 h5("TOTSTAFF = Total paid full time employees"),
                 h5("TOTINCM = Total operating revenue"),
                 h5("BKVOL = Print materials"),
                 h5("EBOOK = Ebooks available"),
                 h5("AUDIO_DL = Downloadable audio titles"),
                 h5("VIDEO_DL = Downlaodable video titles"),
                 h5("VISITS = Total annual library visits"),
                 h5("REGBOR = Registered users (cardholders)"),
                 h5("TOTCIR = Total annual circulation transactions")
)
    ),

mainPanel(
  tabsetPanel(

# ABOUT OUTPUT DISPLAY
        
        tabPanel(title="About", value=1,
                 h2("What can this app be used for?"),
                 "With this application you can explore the results of the Public Library Survey from 2012 to 2014. Summary data of general interest is provided at the state and individual library level; simple regression capabilities are provided at the national level. Each of the three years can be specified for any of these operations.",
                 p(),
                 "Examples of regressions that might be performed include examining the association between government-provided funding and amount of circulation materials. A sample regression is provided.",
                 p(),
                 h2("Where can I find the source data?"),
                "The data used for this application was provided by the Institute of Museum and Library Services and is publicly available at the", a("Public Library Survey data home page.", href="https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey/explore-pls-data/pls-data"), 
                    "Full documentation and supplementary information is available there."
                 ), 

# SUMMARY OUTPUT DISPLAY

        tabPanel(title="Summary", value=2,
                 h2(textOutput("state"), style="text-transform:uppercase;"),
                 p(),
                 fluidRow(
                   column(12,
                          # short table
                          tags$div(tableOutput("tableState"), style="background-color:#ECF0F1;border-radius:15px;")
                   )
                 ),
                 fluidRow(
                   column(6,
                          # collection pie chart
                          plotOutput("pieState_material"),
                          tags$div(h4("Collection Materials"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   ),
                   column(6,
                          # expenses pie chart
                          plotOutput("pieState_expense"),
                          tags$div(h4("Expenses"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   )
                 ),
                fluidRow(
                  column(12,
                 tags$div(h2(textOutput("lib")), style="margin-top:350px;"),
                 p()
                  )
                ),
                 fluidRow(
                   column(12,
                          # short table
                          tags$div(tableOutput("tableLib"), style="background-color:#ECF0F1;border-radius:15px;")
                   )
                 ),
                 fluidRow(
                   column(6,
                          # collection pie chart
                          plotOutput("pieLib_material"),
                          tags$div(h4("Collection Materials"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   ),
                   column(6,
                          # expenses pie chart
                          plotOutput("pieLib_expense"),
                          tags$div(h4("Expenses"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   )
                 )
        ),


        # REGRESSION OUTPUT DISPLAY

        tabPanel(title="Regression", value=3,
                  h2("Scatterplots of Individual Predictor Variables"),
                 plotOutput("scatterplot"),
                  h2("Complete Regression Output"), 
                 tags$div(dataTableOutput("regsummary"), style="background-color:#ECF0F1;border-radius:15px;padding:20px;margin:40px;")
        ),
        id = "conditionedPanels"
      )
    )
  )

)