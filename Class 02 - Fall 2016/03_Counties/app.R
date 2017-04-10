#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#**************************************************************#
#**************************************************************#
# APP SETUP ----
#**************************************************************#
#**************************************************************#

# Load packages
library(pacman)
pacman::p_load(shiny, shinyBS, shinythemes, colourpicker, ggplot2, ggthemes, gridExtra, 
               stargazer, xtable, leaflet, rgdal, scales, readxl, stringr, Hmisc)

# Increase file upload size limit
options(shiny.maxRequestSize = 9*1024^2)

# load basefile
basefile <- readRDS("data/basefile.Rds")

#load codebooks
codebook <- read_excel("data/codebook.xlsx")
quickcode <- read_excel("data/modal_codebook.xlsx")


#**************************************************************#
#**************************************************************#
# UI ----
#**************************************************************#
#**************************************************************#

# Begin fluidPage
ui <- fluidPage(theme = shinytheme("cyborg"),

   # call CSS file
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
     ),
   

   # UI - Application title ----
   #----------------------------------#
   titlePanel("County-level Election and Health Outcomes"),
   
   # Begin Tabbed Layout
   tabsetPanel(
     
     #===================================#
     # UI/MAJOR TAB - DESCRIPTION ----
     #===================================#
     tabPanel("App Description",
              
              tabsetPanel(id = "nav_level_two",
                
                # UI/Minor.Tab - Purpose ----
                # -------------------------------#
                tabPanel("Purpose",
                         tags$div(class = "descContainer",
                                  includeHTML("www/purpose.html"))
                        ), # end >> Purpose
                
                # UI/Minor.Tab - Codebook ----
                # -------------------------------#
                tabPanel("Codebook",
                         tableOutput("codebook")
                         ), # end >> Codebook
     
                # UI/Minor.Tab - Credits ----
                # -------------------------------#
                tabPanel("Credits and References",
                         tags$div(class = "descContainer",
                                  includeHTML("www/reflist.html"))
                         ) # end >> Credits
                
                ) # end >> tabsetPanel
              
              ), ## end DESCRIPTION TAB PANEL
     
     
     
     #===================================#
     # UI/MAJOR TAB - EXPLORE ----
     #===================================#
     tabPanel("Explore the Data",
              
              tabsetPanel(id = "nav_level_two",
                
                # UI/Minor.Tab - Histograms/Barplots ----
                # -------------------------------------------#
                tabPanel("Histograms & Bar Plots",
                         
                         # beg >> RefButtons Row
                         fluidRow(
                           
                           # refbuttons div
                           tags$div(class = "refButtons",
                                    
                                    actionButton("helpdist", "Help"),
                                    
                                    bsModal(id = "modal1", 
                                            title = "Help: Distributions", 
                                            trigger = "helpdist", size = "large",
                                            includeHTML("www/modal_distro.html")),
                                    
                                    actionButton("cbButton1", "Codebook"),
                                    
                                    bsModal(id = "cbMod1", 
                                            title = "What am I looking at?", 
                                            trigger = "cbButton1", size = "large",
                                            tags$div(class = "modalCodebook",
                                                     tableOutput("codebookHist")))
                                    
                                    ) # end refbuttons div
                           
                           ), # end >> RefButtons Row
                         
                         
                         # beg >> Hist/Barplot Inputs Row
                         fluidRow(
                           
                           # plot1 inputs
                           column(3,
                                  selectInput("plotType1",
                                              label = "Select plot 1",
                                              choices = c("Histogram", "Bar plot")),
                                  
                                  uiOutput("varName1"),
                                  
                                  numericInput("bins1",
                                               label = "Bins (for histograms)",
                                               value = 30)),
                           #plot2 inputs
                           column(3,
                                  selectInput("plotType2",
                                              label = "Select plot 2",
                                              choices = c("Histogram", "Bar plot")),
                                  
                                  uiOutput("varName2"),
                                  
                                  numericInput("bins2",
                                               label = "Bins (for histograms)",
                                               value = 30)),
                           #plot3 inputs
                           column(3,
                                  selectInput("plotType3",
                                              label = "Select plot 3",
                                              choices = c("Histogram", "Bar plot")),
                                  
                                  uiOutput("varName3"),
                                  
                                  numericInput("bins3",
                                               label = "Bins (for histograms)",
                                               value = 30)),
                           
                           #theme
                           column(3,
                                  selectInput("themeName",
                                              label = "Select theme",
                                              choices = c("Cyborg",
                                                          "Tufte" = "theme_tufte(ticks= F)",
                                                          "Few" = "theme_few()",
                                                          "FiveThirtyEight" = "theme_fivethirtyeight()",
                                                          "Economist" = "theme_economist()",
                                                          "Simple BW" = "theme_bw()")),
                                  
                                  colourInput("lineColor",
                                              label = "Line color", "black"),
                                  
                                  colourInput("fillColor",
                                              label = "Fill color", "#2a9fd6"))
                           
                           ), # end >> Hist/Barplots Inputs Row
                         
                         
                         # beg >> Hist/Barplot Output Row
                         fluidRow(
                           
                           column(4, plotOutput("expPlot1")),
                           column(4, plotOutput("expPlot2")),
                           column(4, plotOutput("expPlot3"))
                           
                           ), # end >> Hist/Barplot Output Row
                         
                         
                         # beg >> Summary Stats Row
                         fluidRow(
                           
                           column(4, htmlOutput("sumPlot1")),
                           column(4, htmlOutput("sumPlot2")),
                           column(4, htmlOutput("sumPlot3"))
                           
                           ) # end >> Summary Stats Row
                         
                         ), # end >> Histograms/Barplots Minor.Tab
                
                
                # UI/Minor.Tab - Scatterplots ----
                # -------------------------------------------#
                tabPanel("Scatterplots",
                         
                         # beg >> RefButtsns Row
                         fluidRow(
                           
                           # refbuttons div
                           tags$div(class = "refButtons",
                                    
                                    actionButton("helpbi", "Help"),
                                    
                                    bsModal(id = "modal2", 
                                            title = "Help: Scatterplot", 
                                            trigger = "helpbi", size = "large",
                                            includeHTML("www/modal_scatter.html")),
                                    
                                    actionButton("cbButton2", "Codebook"),
                                    
                                    bsModal(id = "cbMod2", 
                                            title = "What am I looking at?", 
                                            trigger = "cbButton2", size = "large",
                                            tags$div(class = "modalCodebook",
                                                     tableOutput("codebookScatter")))
                                    ) # end refbuttons div
                           
                           ), # end >> RefButtons Row
                         
                         # beg >> Scatterplot Row
                         fluidRow(
                           
                           # scatterplot inputs
                           column(3,
                                  
                                  uiOutput("bivx"),
                                  
                                  uiOutput("bivy"),
                                  
                                  selectInput("themeName.biv",
                                              label = "Select theme",
                                              choices = c("Cyborg",
                                                          "Tufte" = "theme_tufte(ticks= F)",
                                                          "Few" = "theme_few()",
                                                          "FiveThirtyEight" = "theme_fivethirtyeight()",
                                                          "Economist" = "theme_economist()",
                                                          "Simple BW" = "theme_bw()")),
                                  
                                  colourInput("loessLineColor",
                                              label = "LOESS line color", "#D62949"),
                                  
                                  colourInput("regLineColor",
                                              label = "Regression line color", "#FFC328"),
                                  
                                  colourInput("pointColor",
                                              label = "Point color", "#2A9FD6"),
                                  
                                  sliderInput("pointAlpha", 
                                              label = "Point opacity",
                                              min = 0, max = 1, value = 0.3, step = .1),
                                  
                                  sliderInput("pointSize",
                                              label = "Point size",
                                              min = 1, max = 10, value = 2)), # end scatterplot inputs
                           
                           # scatterplot output
                           column(8,
                                  
                                  plotOutput("scatterPlot"),
                                  
                                  checkboxInput("loess",
                                                label = "LOESS curve", FALSE),
                                  
                                  checkboxInput("regress",
                                                label = "Regression line", FALSE)) # end scatterplot output
                           
                           ) # end >> Scatterplot Row
                         
                         ), # end >> Scatterplot Minor.Tab
                
                
                # UI/Minor.Tab - Box/Violin Plots ----
                # -------------------------------------------#
                tabPanel("Box and Violin Plots",
                         
                         # beg >> RefButtons Row
                         fluidRow(
                           
                           # refbuttons div
                           tags$div(class = "refButtons",
                                    
                                    actionButton("helpboxvi", "Help"),
                                    
                                    bsModal(id = "modal3", 
                                            title = "Help: Box and Violin Plots", 
                                            trigger = "helpboxvi", size = "large",
                                            includeHTML("www/modal_boxvi.html")),
                                    
                                    actionButton("cbButton3", "Codebook"),
                                    
                                    bsModal(id = "cbMod3", 
                                            title = "What am I looking at?", 
                                            trigger = "cbButton3", size = "large",
                                            tags$div(class = "modalCodebook",
                                                     tableOutput("codebookBox")))) # end rebuttons div
                           
                           ), # end >> RefButtons Row
                         
                         # beg >> Boxplot/Violin Plot Output Row
                         fluidRow(
                           
                           # bp/violin inputs
                           column(3,
                                  
                                  selectInput("boxType",
                                              label = "Plot type", choices = c("Box", "Violin")),
                                  
                                  uiOutput("boxGrp"),
                                  
                                  uiOutput("boxY"),
                                  
                                  selectInput("themeName.box",
                                              label = "Select theme",
                                              choices = c("Cyborg",
                                                          "Tufte" = "theme_tufte(ticks= F)",
                                                          "Few" = "theme_few()",
                                                          "FiveThirtyEight" = "theme_fivethirtyeight()",
                                                          "Economist" = "theme_economist()",
                                                          "Simple BW" = "theme_bw()")),
                                  
                                  selectizeInput("boxGrpCol",
                                                 label = "Color palette:",
                                                 choices = c("Multicolor" = "Set1", 
                                                             "Dark2", "Accent",
                                                             "Pastel", "Greys"))), #end bp/violin inputs
                           
                           # bp/violin output
                           column(8,
                                  plotOutput("boxPlot"))
                           
                           ) # end >> Boxplot/Violing Plot Output Row
                         
                         ), # end >> Box/Violin Plots Minor.Tab
                
                
                # UI/Minor.Tab - Current Dataset ----
                # -------------------------------------------#
                tabPanel("Current Dataset",
                         
                         # beg >> RefButtons Row
                         fluidRow(
                           
                           #refbuttons div
                           tags$div(class = "refButtons",
                                    actionButton("helpdata", "Help"),
                                    
                                    bsModal(id = "modal4", 
                                            title = "Help: Dataset", 
                                            trigger = "helpdata", size = "large",
                                            includeHTML("www/modal_dataset.html")),
                                    
                                    actionButton("cbButton4", "Codebook"),
                                    
                                    bsModal(id = "cbMod4", 
                                            title = "What am I looking at?", 
                                            trigger = "cbButton4", size = "large",
                                            tags$div(class = "modalCodebook",
                                                     tableOutput("codebookDataset")))) # end refbuttons div
                      ), # end >> RefButtons Row
                      
                      # beg >> Data Table Row
                      fluidRow(
                        
                        dataTableOutput("currData")
                        
                        ) # end >> Data Table Row
                      
                      ), # end >> Current Dataset Minor.Tab
                
                
                # UI/Minor.Tab - Leaflet Map ----
                # -------------------------------------------#
                tabPanel("Map",
                         
                         # beg >> RefButtons Row
                         fluidRow(
                           
                           # refbuttons divs
                           tags$div(class = "refButtons",
                                    
                                    actionButton("helpmap", "Help"),
                                    
                                    bsModal(id = "modal5", 
                                            title = "Help: Map", 
                                            trigger = "helpmap", size = "large",
                                            includeHTML("www/modal_map.html")),
                                    
                                    actionButton("cbButton5", "Codebook"),
                                    
                                    bsModal(id = "cbMod5", 
                                            title = "What am I looking at?", 
                                            trigger = "cbButton5", size = "large",
                                            tags$div(class = "modalCodebook",
                                                     tableOutput("codebookMap")))) #end refbuttons div
                           
                           ), # end >> RefButtons Row
                         
                         # beg >> Map Input Row
                         fluidRow(
                           
                           column(3,
                                  uiOutput("mapVar")),
                           
                           column(3,
                                  numericInput("mapQuantiles",
                                               label = "Quantiles (continuous variables):",
                                               min = 3, max = 20, value = 10)),
                           
                           column(3,
                                  selectizeInput("colPal",
                                                 label = "Color palette:",
                                                 choices = list(
                                                   Sequential = c("YlOrRd", "YlOrBr", "YlGnBu", "Reds",
                                                                  "Purples", "Greens", "Blues"),
                                                   Qualitative = c("Multicolor" = "Set1", "Dark2", "Accent"),
                                                   Divergent = c("Spectral", "RdYlGn", "PuOr")))),
                           
                           column(3,
                                  sliderInput("polyOpaque",
                                              label = "Data layer opacity:",
                                              min = 0, max = 1, value = .6, step = .2))
                           
                           ), # end>> Map Input Row
                         
                         # beg >> Map Output Row
                         fluidRow(
                           
                           leafletOutput("intMap")
                           
                           ) # end >> Map Output Row
                         
                         ) # end >> Leaflet Map Minor.Tab
                
                ) # end tabsetPanel
              
              ), # end >> EXPLORE THE DATA TAB PANEL
     
     
     #===================================#
     # UI/MAJOR TAB - STATS ----
     #===================================#
     tabPanel("Analyze the Data",
              
              tabsetPanel(id = "nav_level_two",
                
                # UI/Minor.Tab - Regression Models ----
                # -------------------------------------------#
                tabPanel("Regression Models",
                         
                         # beg >> RefButtons Row
                         fluidRow(
                           
                           # refbuttons div
                           tags$div(class = "refButtons",
                                    
                                    actionButton("helpreg", "Help"),
                                    
                                    bsModal(id = "modal6", 
                                            title = "Help: Regression Models", 
                                            trigger = "helpreg", size = "large",
                                            includeHTML("www/modal_stats.html")),
                                    
                                    actionButton("cbButton6", "Codebook"),
                                    
                                    bsModal(id = "cbMod6", 
                                            title = "What am I looking at?", 
                                            trigger = "cbButton6", size = "large",
                                            tags$div(class = "modalCodebook",
                                                     tableOutput("codebookReg"))),
                                    
                                    actionButton("modelExp", "Interpretations"),
                                    
                                    bsModal(id = "modelModal", 
                                            title = "Understand Your Results", 
                                            trigger = "modelExp", size = "large",
                                            includeHTML("www/model_exp.html"))) #end refbuttons div
                           
                           ), # end >> RefButtons Row
                         
                         # beg >> Regression Model Row
                         fluidRow(
                           
                           # model inputs
                           column(3,
                                  
                                  selectInput("model",
                                              label = "Select Model",
                                              choices = c("Linear", "Logistic")),
                                  
                                  uiOutput("outcome"),
                                  
                                  uiOutput("ex.numIn")),
                           
                           # model output
                           column(9,
                                  
                                  verbatimTextOutput("ex.type"),
                                  
                                  plotOutput("diagnosticGrid"))
                           
                           ) # end >> Regression Model Row
                         
                         ) # end >> Regression Model Minor.Tab
                
                ) # end tabsetPanel
              
              ), # END STATISTICAL ANALYSIS TAB PANEL
     
     
     #===================================#
     # UI/MAJOR TAB - USER UPLOAD ----
     #===================================#
     tabPanel("Add Your Data",
              
              tabsetPanel(id = "nav_level_two",
                
                # UI/Minor.Tab - Upload Your Own ----
                # -------------------------------------------#
                tabPanel("Upload",
                         
                         # beg >> RefButtons Row
                         fluidRow(
                           
                           #refbuttons div
                           tags$div(class = "refButtons",
                                    
                                    actionButton("helpupload", "Help"),
                                    
                                    bsModal(id = "modal7", 
                                            title = "Help: Upload", 
                                            trigger = "helpupload", size = "large",
                                            includeHTML("www/modal_upload.html"))) # end refbuttons div
                           
                           ), # end >> RefButtons Row
                         
                         # beg >> Upload Row
                         fluidRow(
                           
                           # input options
                           column(2,
                                  
                                  fileInput('file1', 'Choose file to upload',
                                            accept = c('text/csv',
                                                       'text/comma-separated-values',
                                                       'text/tab-separated-values',
                                                       'text/plain',
                                                       '.csv',
                                                       '.tsv')),
                                  
                                  tags$hr(),
                                  
                                  radioButtons('sep', 'Separator', c(Comma=',',
                                                                     Semicolon=';',
                                                                     Tab='\t'), ','),
                                  
                                  radioButtons('quote', 'Quote', c(None='',
                                                                   'Double Quote'='"',
                                                                   'Single Quote'="'"), '"'),
                                  
                                  tags$hr(),
                                  
                                  p('Please upload only CSV files with a header of your variable names. 
                                    You must select a variable to be used as your FIPS number below:'),
                                  
                                  uiOutput('fips'),
                                  
                                  actionButton("mergeIt", label = "Merge")
                                  
                                  ), # end input options
                           
                           # data display pre/post upload
                           column(8,
                                  
                                  h4('Your Data'),
                                  dataTableOutput('uploadDT'),
                                  
                                  h4('Pre Uploaded Data'),
                                  dataTableOutput('baseDT'),
                                  
                                  h4('Merged Data'),
                                  dataTableOutput('mergeDT')
                                  
                                  ) # end data display
                           
                           ) #end >> Upload Row
                         
                         ) # end >> Upload Minor.Tab
                
                ) # end tabsetPanel
              
              ) # END USER UPLOAD PANEL
     
     
     ) # end --- Page tabsetPanel Container

) #end ---- Fluid page



#**************************************************************#
#**************************************************************#
# SERVER ----
#**************************************************************#
#**************************************************************#

# Begin Server
server <- function(input, output) {
  
  #dynamic UI credit
  # https://shiny.rstudio.com/articles/dynamic-ui.html
  
  
  #===================================================#
  #===================================================#
  ##### DATA SETUP ####
  #===================================================#
  #===================================================#
  
  
  # create dataframe object for basefile
  bdata <- reactiveValues(
    basefile = readRDS("data/basefile.Rds"))
  
  # dataset used by graphics, tables, and model
  bid <- reactive ({
    
    if(is.null(mergeDF())) {
      basefile
      } else {
        mergeDF()
        }
    
    })
  
  
  # get most recent basefile names for use in dropdowns
  bid.names <- reactive({ 
    
    bn.sub <- bid()[-1:-4]
    names(bn.sub)
    
    })
  
  bid.labs <- reactive({
    
    bn.sub <- bid()[-1:-4]
    Hmisc::label(bn.sub)
    
    })
  
  
  # Set  Dropdown Sections
  dropdown_items <- reactive ({
    
    if (is.null(mergeDF())) {
      
      list("Election Results" = 
             setNames(bid.names()[1:18], bid.labs()[1:18]),
           "Demographics" = 
             setNames(bid.names()[c(19:35,54)], bid.labs()[c(19:35,54)]),
           "Social Characteristics" = 
             setNames(bid.names()[36:40], bid.labs()[36:40]),
           "Health Status" = 
             setNames(bid.names()[41:48], bid.labs()[41:48]),
           "Economic Characteristics" = 
             setNames(bid.names()[c(49:53,55:56)], bid.labs()[c(49:53,55:56)])
      )
      
    } else {
      
      list("Election Results" = 
             setNames(bid.names()[1:18], bid.labs()[1:18]),
           "Demographics" = 
             setNames(bid.names()[c(19:35,54)], bid.labs()[c(19:35,54)]),
           "Social Characteristics" = 
             setNames(bid.names()[36:40], bid.labs()[36:40]),
           "Health Status" = 
             setNames(bid.names()[41:48], bid.labs()[41:48]),
           "Economic Characteristics" = 
             setNames(bid.names()[c(49:53,55:56)], bid.labs()[c(49:53,55:56)]),
           "User Data" = 
             bid.names()[57:length(bid.names())]
      )
      
    }
    
  })
  
  #===================================================#
  #===================================================#
  # HISTOGRAMS AND BOXPLOTS REACTIVE SETUP ####
  #===================================================#
  #===================================================#
  
  # plot reactives
  p1.type <- reactive({input$plotType1})
  p2.type <- reactive({input$plotType2})
  p3.type <- reactive({input$plotType3})
  
  #plot variable reactives
  v1.type <- reactive({input$varName1})
  v2.type <- reactive({input$varName2})
  v3.type <- reactive({input$varName3})
  
  # bin reactives
  bins1 <- reactive({input$bins1})
  bins2 <- reactive({input$bins2})
  bins3 <- reactive({input$bins3})
  
  # theme reactives
  line.color <- reactive({input$lineColor})
  fill.color <- reactive({input$fillColor})
  
  # default theme for all sections
  theme.default <- 'theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "#4d4d4d"),
      panel.grid.minor = element_line(color = "#4d4d4d"),
      plot.background = element_rect(fill = "#060606", color = "#060606"),
      axis.title = element_text(face = "bold", size = 20, color = "#cccccc"),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "#cccccc", size = 12),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(1, "cm"),
      legend.text = element_text(color = "#cccccc", size = 14)
      )'
  
  # theme selector
  theme.select <- reactive({
    if (input$themeName == "Cyborg") {
      theme.default
      } else {
        input$themeName
        }
    })
  
  # static y label
  y_label <- ylab("counties")
  
  
  
  #===================================================#
  #===================================================#
  # HISTOGRAMS AND BAR PLOT OUTPUT ####
  #===================================================#
  #===================================================#
  
    ### PLOT 1
  
    # Plot1 live dropdown
    output$varName1 <- renderUI({
      selectInput("varName1", "Select variable", 
                  choices = dropdown_items(),
                  selected = "pct_clinton")
      })
  
   # Plot1 output
   output$expPlot1 <- renderPlot({
     
      # Plot1 select
      if (p1.type() == "Histogram") {
        gp.layer <- geom_histogram(aes_string(x = v1.type()), col = line.color(), 
                                   fill = fill.color(),
                                   bins = bins1())
      } else if (p1.type() == "Bar plot") {
        gp.layer <- geom_bar(aes(x = factor(eval(parse(text = v1.type())))), 
                             col = line.color(), fill = fill.color())
      } 
     
      # Theme select 
      skin <- eval(parse(text = theme.select()))
      # Generate plot
      ggplot(bid()) + xlab(v1.type()) + y_label + gp.layer + skin
      
      })


   #### PLOT 2
   
   # Plot2 live dropdown
   output$varName2 <- renderUI({
     selectInput("varName2", "Select variable" , 
                 choices = dropdown_items(),
                 selected = "pct_trump")
     })
   
   # Plot2 output
   output$expPlot2 <- renderPlot({

     # Plot2 select
     if (p2.type() == "Histogram") {
       gp.layer <- geom_histogram(aes_string(x = v2.type()), col = line.color(), 
                                  fill = fill.color(),
                                  bins = bins2())
       } else if (p2.type() == "Bar plot") {
         gp.layer <- geom_bar(aes(x = factor(eval(parse(text = v2.type())))), 
                              col = line.color(), fill = fill.color())
         } 
     
     # Theme select 
     skin <- eval(parse(text = theme.select()))
     # Generate plot
     ggplot(bid()) + xlab(v2.type()) + y_label + gp.layer + skin
     
     })
   
   
   #### PLOT 3
   
   # Plot3 live dropdown
   output$varName3 <- renderUI({
     selectInput("varName3", "Select variable",
                 choices = dropdown_items(),
                 selected = "drugmort_rate")
     
     })
   
   # Plot3 output
   output$expPlot3 <- renderPlot({
     
     # Plot3 select
     if (p3.type() == "Histogram") {
       gp.layer <- geom_histogram(aes_string(x = v3.type()), col = line.color(), 
                                  fill = fill.color(),
                                  bins = bins3())
       } else if (p3.type() == "Bar plot") {
         gp.layer <- geom_bar(aes(x = factor(eval(parse(text = v3.type())))), 
                            col = line.color(), fill = fill.color())
         } 
     
     # Theme select 
     skin <- eval(parse(text = theme.select()))
     
     # Generate plot
     ggplot(bid()) + xlab(v3.type()) + y_label + gp.layer + skin
     
     })
   
   #===================================================#
   # Summary Statistics Tables ----
   #===================================================#
   
   # set xtable options to omit credit line
   options(xtable.comment = FALSE)
   
   # Plot1 Summary
   output$sumPlot1 <- renderUI({
     
     tags$div(class = "startables",
              
              if (p1.type() == "Histogram") {
                
                HTML(
                  stargazer(data.frame(bid()[[v1.type()]]), type = "html", 
                            summary = T, flip = T, median = T, iqr = T,
                            covariate.labels = "", digits = 2))
                
                } else if (p1.type() == "Bar plot") {
                  
                  ftab <- table(bid()[[v1.type()]]) # save freqs
                  ptab <- prop.table(ftab) # save props
                  fp <- cbind(ftab, ptab) # bind freqs and props
                  
                  xt.fp <- xtable(fp, digits = c(0, 0, 2)) # save to xtable
                  colnames(xt.fp) <- c("Freq", "%") # feed in column names
                  
                  #print to HTML
                  HTML(
                    print.xtable(xt.fp, 
                                 type = "html", quote = F,
                                 html.table.attributes = "class='xtab'"))
                  })
     })
   
   # Plot2 Summary
   output$sumPlot2 <- renderUI({
     
     tags$div(class = "startables",
              
              if (p2.type() == "Histogram") {
                
                HTML(
                  stargazer(data.frame(bid()[[v2.type()]]), type = "html", 
                            summary = T, flip = T, median = T, iqr = T,
                            covariate.labels = "", digits = 2))
                
                } else if (p2.type() == "Bar plot") {
                  
                  ftab <- table(bid()[[v2.type()]]) # save freqs
                  ptab <- prop.table(ftab) # save props
                  fp <- cbind(ftab, ptab) # bind freqs and props
                  
                  xt.fp <- xtable(fp, digits = c(0, 0, 2)) # save to xtable
                  colnames(xt.fp) <- c("Freq", "%") # feed in column names
                  
                  #print to HTML
                  HTML(
                    print.xtable(xt.fp, 
                                 type = "html", quote = F,
                                 html.table.attributes = "class='xtab'"))
                  })
     })
   
   # Plot3 Summary
   output$sumPlot3 <- renderUI({
     
     tags$div(class = "startables",
              
              if (p3.type() == "Histogram") {
                
                HTML(
                  stargazer(data.frame(bid()[[v3.type()]]), type = "html", 
                            summary = T, flip = T, median = T, iqr = T,
                            covariate.labels = "", digits = 2))
                
                } else if (p3.type() == "Bar plot") {
                  
                  ftab <- table(bid()[[v3.type()]]) # save freqs
                  ptab <- prop.table(ftab) # save props
                  fp <- cbind(ftab, ptab) # bind freqs and props
                  
                  xt.fp <- xtable(fp, digits = c(0, 0, 2)) # save to xtable
                  colnames(xt.fp) <- c("Freq", "%") # feed in column names
                  
                  #print to HTML
                  HTML(
                    print.xtable(xt.fp, 
                                 type = "html", quote = F,
                                 html.table.attributes = "class='xtab'"))
                  })
     })
   

   
   #===================================================#
   #===================================================#
   # SCATTERPLOT ####
   #===================================================#
   #===================================================#
   
   
   # save line colors
   lcol <- reactive ({ input$loessLineColor })
   rcol <- reactive ({ input$regLineColor })
   pt.col <- reactive ({ input$pointColor })
   pt.alpha <- reactive ({ input$pointAlpha })
   pt.size <- reactive ({ input$pointSize })
   
   # theme select for scatterplot
   theme.select.biv <- reactive({
     if (input$themeName.biv == "Cyborg") {
       theme.default
       } else {
         input$themeName.biv
         }
     })
   
   
   # X axis dynamic dropdown
   output$bivx <- renderUI ({ selectInput("bivx",
                              label = "X axis",
                              choices = dropdown_items(),
                              selected = "PCT_smoker")
     })
   
   # X axis dynamic dropdown
   output$bivy <- renderUI ({ selectInput("bivy",
                                          label = "Y axis",
                                          choices = dropdown_items(),
                                          selected = "AVG_Medicare_Expenditure")
     })
    
   # render Scatterplot
   output$scatterPlot <- renderPlot({
     
     if (input$loess == TRUE & input$regress == TRUE) {
       
       ggplot(bid(), aes_string(x = input$bivx, y = input$bivy)) + 
         geom_point(col = pt.col(), size = pt.size(), alpha = pt.alpha()) + 
         eval(parse(text = theme.select.biv())) + 
         geom_smooth(method = "loess", col = lcol()) + 
         geom_smooth(method = "lm", col = rcol())
       
       } else if (input$loess == TRUE) {
         
         ggplot(bid(), aes_string(x = input$bivx, y = input$bivy)) + 
           geom_point(col = pt.col(), size = pt.size(), alpha = pt.alpha()) + 
           eval(parse(text = theme.select.biv())) + 
           geom_smooth(method = "loess", col = lcol())
         
         } else if (input$regress == TRUE) {
           
           ggplot(bid(), aes_string(x = input$bivx, y = input$bivy)) + 
             geom_point(col = pt.col(), size = pt.size(), alpha = pt.alpha()) + 
             eval(parse(text = theme.select.biv())) + 
             geom_smooth(method = "lm", col = rcol())
           
           } else {
             
             ggplot(bid(), aes_string(x = input$bivx, y = input$bivy)) + 
               geom_point(col = pt.col(), size = pt.size(), alpha = pt.alpha()) + 
               eval(parse(text = theme.select.biv()))
           }
     
     }, height = 640)
   

   #===================================================#
   #===================================================#
   # BOXPLOTS AND VIOLIN PLOTS ----
   #===================================================#
   #===================================================#
   
   # theme select for boxplots
   theme.select.box <- reactive({
     
     if (input$themeName.box == "Cyborg") {
       theme.default
       } else {
         input$themeName.box }
     
     })
   

   # dynamic dropdowns
   output$boxGrp <- renderUI ({ selectInput("boxGrp",
                                            label = "Group",
                                            choices = dropdown_items(),
                                            selected = "flipped_2016")
     })
   
   output$boxY <- renderUI ({ selectInput("boxY",
                                          label = "Y axis",
                                          choices = dropdown_items(),
                                          selected = "PCT_unemployed")
     })
   
   # boxplot/violin reactives
   box.x <- reactive ({ input$boxGrp })
   box.y <- reactive ({ input$boxY })
   box.col <- reactive ({ input$boxGrpCol })
   
   whiskOutlierCol <- reactive({
     if (input$themeName.box == "Cyborg") {
       "#cccccc"
       } else {
         "#0b0b0b" }
     })
   
   # render plots
   output$boxPlot <- renderPlot ({
     
     bd <- bid()
     
     # remove observations with NAs in the selected variables
     box.df <- na.omit(bd[,c(box.x(), box.y())])
     
     if (input$boxType == "Box") {
       
       ggplot(box.df, aes_string(x = box.x(), y = box.y(), fill = box.x())) + 
         geom_boxplot(outlier.color = whiskOutlierCol(), 
                      color = whiskOutlierCol(),
                      size = 1, width = .5) + 
         eval(parse(text = theme.select.box())) +
         scale_fill_brewer(palette = box.col()) +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank())
       
       } else {
         
         ggplot(box.df, aes_string(x = box.x(), y = box.y(), fill = box.x())) + 
           geom_violin() + eval(parse(text = theme.select.box())) + 
           scale_fill_brewer(palette = box.col()) +
           theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank())
       }
     
     }, height = 640)
   
  
  #===================================================#
  #===================================================#
  # CURRENT DATA TABLE ----
  #===================================================#
  #===================================================#
  
  # datatable options
  # http://shiny.rstudio.com/articles/datatables.html 
   
  output$currData <- renderDataTable({
    bid()
    
    }, options = list(lengthMenu = c(10, 20, 50, 100),
                      pageLength = 10))

  
   
  #===================================================#
  #===================================================#
  # MAPPING ----
  #===================================================#
  #===================================================#
  

  # dynamic variable dropdown
   output$mapVar <- renderUI ({ selectInput("mapVar",
                                            label = "Select variable to map:",
                                            choices = dropdown_items(),
                                            selected = "pct_trump")
     })


   # Leaflet reactives
   # https://rstudio.github.io/leaflet/shiny.html

   # Base Dataframe
   # this reactive will run every time the basefile is updated
   basemap_df <- reactive ({
     
     bd <- bid()
     bd$GEOID <- bd$FIPS
     bd$FIPS <- NULL

     # shapefile sources:
     # counties: https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
     # states: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2015&layergroup=States+%28and+equivalent%29

     # counties
     counties_shp <- readOGR(dsn = "cb_2015_us_county_20m",
                             layer = "cb_2015_us_county_20m", stringsAsFactors = F)

     # merge value data with map data
     basemap <- merge(counties_shp, bd, by = "GEOID")

     return(basemap)
     
     })

   
   # Reactive dataframe
   # updated everytime a new mapping variable is selected
   leafmap_df <- reactive ({
     
     bm <- basemap_df()
     mv <- input$mapVar

     names(bm)[names(bm) == mv] <- "value"

     return(bm)
     
     })


   # Popups

   # put popup tooltip in its own reactive
   # will run every time a new mapping variable is selected

   tt_value <- reactive ({
     # detects whether the selected variable is numeric/categorical
     # sends the correct value output to the tooltip
     if (is.numeric(leafmap_df()$value)) {
       
       tip.val <- round(leafmap_df()$value, digits = 2)
       return(tip.val)
       
       } else {
         
         tip.val <- leafmap_df()$value
         return(tip.val)
         
         }
     
     })

   # popup info
   popup_tt <- reactive ({

     # popup contents
     popup_info <- paste0("<strong>County: </strong>",
                          basemap_df()$NAME,
                          "<br /><strong>Selected variable: </strong>",
                          tt_value(),
                          "<br /><br /><strong>2012 winner: </strong>",
                          basemap_df()$winner_2012,
                          "<br /><strong>2016 winner: </strong>",
                          basemap_df()$winner_2016,
                          "<br /><br /><strong>% White: </strong>",
                          round(basemap_df()$PCT_county_white, digits = 2),
                          "<br /><strong>% Black: </strong>",
                          round(basemap_df()$PCT_county_black, digits = 2),
                          "<br /><strong>% Hispanic: </strong>",
                          round(basemap_df()$PCT_county_hispanic, digits = 2),
                          "<br /><strong>% Asian: </strong>",
                          round(basemap_df()$PCT_county_asian, digits = 2),
                          "<br /><br /><strong>% Unemployed: </strong>",
                          round(basemap_df()$PCT_unemployed, digits = 2),
                          "<br /><strong>Drug mortality rate: </strong>",
                          round(basemap_df()$drugmort_rate, digits = 2))

     # return the paste string to the reactive
     return(popup_info)
     
     })


   # update color quantiles and legend
   # any time this selection is changed
   mapColors <- reactive ({
     
     if (is.numeric(tt_value())) {
       
       outpal <- colorQuantile(input$colPal, domain = tt_value(), n = input$mapQuantiles)
       return(outpal)
       
       } else {
         
         outpal <- colorFactor(input$colPal, domain = tt_value())
         return(outpal)
         
         }
     })

   # gets index of mapping variable
   # to feed to the legend title
   mapVarIndex <- reactive ({

     # column index for legend title
     v.index <- match(input$mapVar, bid.names())
     return(v.index)
     
     })

   # Map Output

   # static basemap
   # loading this separately is part of preventing the map being
   # redrawn every time someone updates the mapping var

   output$intMap <- renderLeaflet ({

     # leaflet map guide:
     # https://www.datascienceriot.com/mapping-us-counties-in-r-with-fips/kris/

     # plot map
     leaflet(data = basemap_df()) %>%
       addTiles(options = tileOptions(noWrap = TRUE)) %>%
       setView(lng = -93.85, lat = 37.45, zoom = 4)
     
     })


   # looks for updates to the mapping variable and rerenders the
   # color overlay and legend
   observe({
     pal <- mapColors()

     leafletProxy("intMap", data = leafmap_df()) %>%
       clearShapes %>%
       clearControls %>%
       addPolygons(fillColor = ~pal(leafmap_df()$value),
                   fillOpacity = input$polyOpaque,
                   color = "#BDBDC3",
                   weight = 1,
                   popup = popup_tt()) %>%
       addLegend(position = "topright",
                 pal = pal,
                 values = tt_value(),
                 title = bid.names()[mapVarIndex()],
                 opacity = 1)
     
     
     })


   #===================================================#
   #===================================================#
   # STATISTICAL ANALYSIS ----
   #===================================================#
   #===================================================#
   
   # dynamic dropdowns
   output$outcome <- renderUI ({ 
     
      selectInput("outcome",
                  label = "Outcome variable:",
                  choices = dropdown_items()) 
     
     })
   
   
   output$ex.numIn <- renderUI ({
     
      selectInput("ex.numIn",
                  label = "Explanatory variables (numeric)",
                  choices = dropdown_items(), 
                  multiple = TRUE)  
     
     })
   

   #reactives
   o.var <- reactive ({ input$outcome })
   ex.num <- reactive ({ input$ex.numIn })
   
   # model
   fit <- reactive({
     
     if(is.null(o.var()) | is.null(ex.num())) {
       
       return(NULL)
       
       } else if (input$model == "Logistic") {
         
         # code source:
         ## https://github.com/Sullivanstatistics/php2560-shiny-projects/blob/master/LinearRegression/server.R
         fmla <- as.formula(paste("factor(", o.var(), ")~",
                                  paste(c(unlist(ex.num())),
                                        collapse = "+"),sep=""))
         
         fit <- glm(formula = fmla, data = bid(), family = binomial)
         
         return(fit)
         
         } else if  (input$model == "Linear") {
           
           fmla <- as.formula(paste(o.var(), "~", 
                                    paste(c(ex.num(), unlist(ex.num())), 
                                          collapse = "+")))
           
           fit <- lm(formula = fmla, data = bid())

           return(fit)
           
           }
     })
   
   
   # print model
   output$ex.type <- renderPrint({
     
     if(is.null(fit())) {
       
       #https://stackoverflow.com/questions/4071586/printing-newlines-with-print-in-r
       
        writeLines("> Feed me an explanatory variable.
                   \n> But be mindful!
                   \n> The model will run regardless of which variables you enter,
                   \n> whether or not the results mean anything...
                   \n> 
                   \n> A couple of models you might consider...
                   \n> Linear: continuous outcome: % of County, Trump, explanatory: Local Tax Rate + Net Migration Rate
                   \n> Logistic: binary outcome: Winner 2016, explanatory: % of County, White + Drug Mortality Rate")
       
       } else {
         summary(fit())
         }
     
     })
   

   
   # render the plot grid
   output$diagnosticGrid <- renderPlot({
    
     # nod for layout matrix
     # https://stackoverflow.com/questions/30156443/r-setting-multiple-plot-heights-with-par
     
     if(is.null(fit())) {
       
       ggplot(bid()) + 
         labs(title = "Diagnostic plots will appear here") +
         eval(parse(text = theme.default))
       
       } else {
         
         # lots of good help here:
         # https://zief0002.github.io/Computing-Club/notes/2015_S/2015_S_02_Visual_Diagnostics.html#17
         
         # fortify model
         fort_fit <- fortify(fit())
         
         # save plot skin
         skin <- eval(parse(text = theme.select()))
         
         # base plot
         base_diag <- ggplot(fort_fit) + skin
         
         # residuals vs. fitted
         resid_plot <- base_diag + 
           geom_hline(yintercept = 0, color = "#d62949", size = 1) + 
           geom_point(aes(x = .fitted, y = .resid), color = "#2a9fd6", 
                      size = 2, alpha = .3) +
           labs(x = "fitted", y = "residuals")
         
         # quantile-quantile plot
         qq_plot <- base_diag + 
           geom_abline(color = "#d62949", size = 1) +
           stat_qq(aes(sample = .stdresid), color = "#2a9fd6", alpha = .3) +
           labs(x = "theoretical", y = "std residuals")
         
         # standardized residuals vs. fitted
         stdres_plot <- base_diag +
           geom_smooth(aes(x = .fitted, y = sqrt(.stdresid)), color = "#d62949", size = 1,
                       se = FALSE) +
           geom_point(aes(x = .fitted, y = sqrt(.stdresid)), color = "#2a9fd6", alpha = .3) +
           labs(x = "fitted", y = "sqrt(std residuals)")
         
         # Cook's distance plot
         cooksd_plot <- base_diag + 
           geom_bar(aes(x = seq_along(.cooksd), y = .cooksd),
                    color = "#2a9fd6", size = 1,
                    stat = "identity", position = "identity") + 
           labs(x = "observation", y = "Cook's distance")
         
         # Residuals vs. leverages
         reslev_plot <- base_diag +
           geom_smooth(aes(x = .hat, y = .stdresid),
                       se = FALSE, color = "#d62949", size = 1) +
           geom_point(aes(x = .hat, y = .stdresid), color = "#2a9fd6", alpha = .3) +
           labs(x = "std residuals", y = "leverage")
         
         # Cook's distance dot plot
         cooksdot_plot <- base_diag + 
           geom_hline(yintercept = 0, color = "#d62949", size = 1) +
           geom_point(aes(x = .fitted, y = .resid, size = .cooksd), 
                      color = "#2a9fd6", alpha = .3) + 
           scale_size_area("Cook's Distance")
         
         # diagnostic plot grid
         grid.arrange(resid_plot, qq_plot, 
                      stdres_plot, cooksd_plot, 
                      reslev_plot, cooksdot_plot,
                      nrow = 3, ncol = 2)
 
         }
     })
   
   
   
   #===================================================#
   #===================================================#
   # PURPOSE / CODEBOOK ----
   #===================================================#
   #===================================================#
   
   output$codebook <- renderTable(
     codebook
     )
   
   output$codebookHist <- renderTable(
     quickcode
     )
   
   output$codebookScatter <- renderTable(
     quickcode
     )
   
   output$codebookBox <- renderTable(
     quickcode
     )
   
   output$codebookDataset <- renderTable(
     quickcode
     )
   
   output$codebookMap <- renderTable(
     quickcode
     )
   
   output$codebookReg <- renderTable(
     quickcode
     )
   
   
   
   #===================================================#
   #===================================================#
   # USER UPLOADS ----
   #===================================================#
   #===================================================#

   # Reactive dataframe handling inspiration
   # https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb
   # https://stackoverflow.com/questions/17056766/conditionally-subsetting-and-calculating-a-new-variable-in-dataframe-in-shiny

   upData <- reactive({
     
     if(is.null(input$file1$datapath)) {
       
       return(NULL)
       
       } else {
         
         read.csv(input$file1$datapath, 
                  sep = input$sep,
                  quote = input$quote, header = T,
                  row.names = NULL) 
         }
     })
   
   # select FIPS var
   fips.var <- reactive({ input$fipsInput })
   

   # merge datasets
   mergeDF <- reactive({
     
     # actionButton state info
     # https://github.com/rstudio/shiny/issues/167
     # https://stackoverflow.com/questions/24184070/actionbutton-in-rshiny-alternative-to-reset-value
     
     if(input$mergeIt == 0) return(NULL)
     
     isolate({
          # for use within this reactive
          up <- upData()
          bd <- bdata$basefile
     
          # create FIPs merge variable
          up$FIPS_MERGE <- str_pad(up[[fips.var()]], 5, "left", pad = "0")
          up$FIPS <- NULL

          # merge upload with basefile
          mdf <- merge(bd, up, by.x = "FIPS", by.y = "FIPS_MERGE", sort = T)
     
          return(mdf)
        })
   })

   # show uploaded dataset
   output$uploadDT <- renderDataTable(
     
     options = list(lengthMenu = c(10, 20, 50, 100),
                    pageLength = 5),
     
     if(is.null(upData())) {
       return(NULL)
       } else { upData() }
     
     )
   
   # show pre-uploaded data 
   output$baseDT <- renderDataTable({
     
     bd <- readRDS("data/basefile.Rds")
     return(bd)
     
     }, options = list(lengthMenu = c(10, 20, 50, 100),
                       pageLength = 5))
   
   # show merged dataset
   output$mergeDT <- renderDataTable({
     
     if(is.null(mergeDF())) {
       return(NULL)
       } else { mergeDF() }
     
     }, options = list(lengthMenu = c(10, 20, 50, 100),
                     pageLength = 5))
   
   
   # fips variable specifier (dropdown)
   output$fips <- renderUI({
     
     if (is.null(upData())) {
       return(NULL)
       } else {
         selectInput("fipsInput", "Important! - Select FIPS",
                     names(upData()))
         }
    })
   
 }


# Run the application 
shinyApp(ui = ui, server = server)

