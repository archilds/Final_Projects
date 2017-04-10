install.packages("shiny")
install.packages("DescTools") # Cramer's V
install.packages("Kendall") # Kendall's Tau
install.packages("lattice")
install.packages("lsr") # Cramer's V
install.packages("MASS")
install.packages("MVN")
install.packages("psych") # Cohen's kappa
install.packages("rococo") # Gamma rank correlation
install.packages("RVAideMemoire") # Spearman's rank correlation
install.packages("stats")
install.packages("xtable")

library("shiny")
library("DescTools") # Cramer's V
library("Kendall") # Kendall's Tau
library("lattice")
library("lsr") # Cramer's V
library("MASS")
library("MVN")
library("psych") # Cohen's kappa
library("rococo") # Gamma rank correlation
library("RVAideMemoire") # Spearman's rank correlation
library("stats")
library("xtable")

ui <- fluidPage(
  titlePanel("Measures of association"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("header", "Are variable names included in the dataset?",
                   choices=c(Yes=TRUE, No=FALSE)),
      radioButtons("sep", "Choose the dataset value separator:",
                   choices=c(Comma=",", Space=" ", Semicolon=";", Tab="\t")),
      fileInput("df", "Load the dataset:"),
      actionButton("defaultbutton", "Default data"),
      helpText("Click the button to load a default dataset")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
                 br(),
                 p(strong("Motivation:"),
                   br(),
                   "There are many single-valued measures of association between two variables that exist, each of which is appropriate for specific data and situations. This application is designed to aid you in choosing the appropriate measure. It will then calculate the statistic for you, with a 95% bootstrap confidence interval, and provides you with an interpretation."),
                 br(),
                 p(strong("In this application:"),
                   br(),
                   'This application contains a left-sidebar for loading several types of data (with or without column names; comma, space, semicolon, or tab separated values), and a main page containing seven tabs: "Introduction" (the current tab), "View dataset", "Variable selection", "Data exploration - Graphical", "Data exploration - Tabular", "Calculate association", and "Answer and explanation".',
                   br(),
                   tags$ul(
                     tags$li('The "View dataset" tab allows you to view the loaded dataset or default dataset. You are allowed to view up to 1,000 rows (due to computation time).'), 
                     tags$li('In the "Variable selection" tab, you will select the two variables from the loaded dataset that you wish to calculate the association between. You will also select what type of variable each is (continuous or discrete).'), 
                     tags$li('The two "Data exploration" tabs allow you to view plots or tables (depending on the chosen variable type) of your selected variables. This may useful in determining which measure of association to choose. Please note that if you load a large dataset, and choose two continuous variables with many values, then the data exploration table will be very large, and the application may take a long time to load and may crash.'),
                     tags$li('In the "Calculate association" tab, you may be asked to provide additional information regarding your data or preferred measure of association interpretation. The final drop-down menu in this tab will be to choose the measure of association you wish to calculate.'),
                     tags$li('The "Answer and explanation" tab is the final tab, and states the calculated statistic, the 95% bootstrap confidence interval (based on 1,000 resamples), and some information regarding the chosen association measure.')
                   )),
                 br(),
                 p(strong("Assumptions and suggestions:"),
                   br(),
                   'This application does take a few things for granted:',
                   br(),
                   tags$ul(
                     tags$li('In a user-uploaded dataset, each column must represent a variable, and all variables must contain the same number of observations (i.e. have the same length).'),
                     tags$li('The first row of the dataset can contain variable names. If there are no variable names, then the variables will automatically be named "V1", "V2", "V3", etc. If any variables names are missing, then the missing variables will be named "X", "X.1", "X.2", "X.3", etc.'),
                     tags$li("Missing data must be coded as NA. Columns containing at least 50% missing data will be removed. For the remaining data, only complete cases will be used."),
                     tags$li("All data will be converted to type numeric. We recommend that the user recodes all variables to be numerical, in order to avoid unintentionally introducing missing data from coercing other variables to type numeric."),
                     tags$li("If a dataset is loaded into the application, we suggest that the user reloads the application if they want to use a new dataset. We also suggest restarting the application before using the default dataset.")
                   )),
                 br(),
                 p(strong("Instructions:"),
                   br(),
                   'First, upload your dataset to the left by clicking "Choose File", and selecting dataset\'s file path. If you do not have a dataset available, then click the "Default data" button to load a predetermined dataset',
                   tags$a(href="https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/birthwt.html", '("Risk Factors Associated with Low Infant Birthweight"; click here for detailed information on the dataset)'),
                   'containing 10 variables into the application. Once your data is loaded, follow the course of the drop-down menus and queries that appear.')),
        tabPanel("View dataset",
                 br(),
                 uiOutput("row_choice"),
                 br(),
                 uiOutput("select_cols"),
                 br(),
                 tableOutput("dataset")),
        tabPanel("Variable selection",
                 br(),
                 uiOutput("var_select1"),
                 uiOutput("helptext1"),
                 br(),
                 uiOutput("var_select2"),
                 uiOutput("helptext2"),
                 br(),
                 uiOutput("var_type1a"),
                 uiOutput("var_type2a"),
                 br(),
                 textOutput("type_text")),
        tabPanel("Data exploration - Graphical",
                 tags$style(type='text/css', '#royston_test {background-color: rgba(255,255,255,1);}'),
                 tags$style(type='text/css', '#royston_test {border-color: rgba(255,255,255,1);}'),
                 br(),
                 textOutput("text_graphical"),
                 br(),
                 textOutput("royston_text"),
                 br(),
                 verbatimTextOutput("royston_test"),
                 br(),
                 textOutput("text_qq"),
                 br(),
                 plotOutput("explore_hist"),
                 br(),
                 textOutput("explore_hist_explanation"),
                 br(),
                 textOutput("persp_text"),
                 br(),
                 plotOutput("persp_plot"),
                 br(),
                 textOutput("text_plot"),
                 br(),
                 plotOutput("explore_plot")),
        tabPanel("Data exploration - Tabular",
                 br(),
                 textOutput("text_tabular"),
                 tableOutput("explore_table")),
        tabPanel("Calculate association",
                 br(),
                 uiOutput("var_type1b"),
                 uiOutput("var_type2b"),
                 htmlOutput("interpret"),
                 uiOutput("choose_interpret"),
                 uiOutput("choose_pearson"),
                 uiOutput("choose_cor"),
                 uiOutput("choose_cor2"),
                 uiOutput("choose_cor3"),
                 textOutput("tab3text")),
        tabPanel("Answer and explanation",
                 br(),
                 textOutput("correlation"),
                 br(),
                 textOutput("explanation"))
      )
    )
  )
)

options(shiny.maxRequestSize=101*1024^2) # it can accept up to 101MB of data, upgradable

server <- function(input, output, session) {
  dat <- reactive({ # (I believe that the dataset is the only "reactive" object that we need)
    df1 <- input$df
    if (input$defaultbutton == 0 && is.null(df1)==TRUE){
      return()
    } else if (input$defaultbutton && is.null(df1)==TRUE) { # so no errors appear
      return(birthwt)
    }
      
    dat1 <- read.table(df1$datapath, header=as.logical(input$header), sep=input$sep)
    type <- sapply(dat1, class) # converting whatever type of data we have into numeric
    for (i in 1:length(type)) {
      if (type[i]=="factor" | type[i]=="character" | type[i]=="logical") {
        dat1[, i] <- as.numeric(dat1[, i])
      } else {
        dat1[, i] <- dat1[, i]
      }
    }
      
    # This removes any column with more than 50% missing data
    missing <- apply(apply(dat1, 2, is.na), 2, sum)
    for (i in 1:length(missing)) {
      if (missing[i]/nrow(dat1) <= 0.50) {
        dat1[, i] <- dat1[, i]
      } else {
        dat1[, i] <- NULL
      }
    }
    
    indices <- which(apply(apply(dat1, 2, is.na), 1, sum)!=0) # indices of columns with no missing data
    if (length(indices)!=0) {
      dat1 <- dat1[-indices, ] # of the columns left, use only complete cases
      return(dat1)
    } else {
      return(dat1)
    }
    # IMPORTANT NOTE: all columns are now type "numeric"
  })
  
  output$row_choice <- renderUI({
    if (is.null(dat())==TRUE) {
      return()
    } else {
      numericInput("dataset_rows",
                   "How many rows of the dataset would you like to display?",
                   value=10, min=0, max=100)
    }
  })
  
  output$select_cols <- renderUI({
    if (is.null(dat())==TRUE) {
      return()
    } else {
      selectInput("dataset_cols",
                  "Choose any variables that you would like to omit from being displayed:",
                  choices=colnames(dat()), multiple=TRUE)
    }
  })
  
  output$dataset <- renderTable({
    if (is.null(dat())==TRUE) {
      return()
    } else if (is.numeric(input$dataset_rows)==FALSE) {
      return()
    } else if (input$dataset_rows==0) {
      return()
    } else {
      if (length(input$dataset_cols>0)) {
        indices <- c()
        for (i in 1:length(input$dataset_cols)) {
          indices[i] <- which(colnames(dat())==input$dataset_cols[i])
        }
        if (input$dataset_rows>=1000) {
          show_rows <- min(nrow(dat()), 1000)
          dat()[1:show_rows, -indices]
        } else {
          dat()[1:input$dataset_rows, -indices]
        }
      } else {
        if (input$dataset_rows>=1000) {
          show_rows <- min(nrow(dat()), 1000)
          dat()[1:show_rows, ]
        } else {
          dat()[1:input$dataset_rows, ]
        }
      }
    }
  })
  
  output$helptext1 <- renderUI({
    if (is.null(dat())==TRUE) {
      return()
    } else {
      helpText("For plots, this will be the variable on the x-axis.")
    }
  })
  
  output$helptext2 <- renderUI({
    if (is.null(dat())==TRUE) {
      return()
    } else {
      helpText("For plots, this will be the variable on the y-axis.")
    }
  })
  
  output$var_select1 <- renderUI({
    if (is.null(dat())==TRUE) {
      return()
    } else {
      selectInput("var_select1", "Select Variable 1",
                  choices=c("Select a variable", colnames(dat())))
    }
  })
  
  output$var_select2 <- renderUI({
    if (is.null(dat())==TRUE) {
      return()
    } else {
      selectInput("var_select2", "Select Variable 2",
                  choices=c("Select a variable", colnames(dat())))
    }
  })
  
  output$var_type1a <- renderUI({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_select2)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_select2=="Select a variable") {
      return()
    } else {
      selectInput("var_type1a", "Is Variable 1 continuous or discrete?",
                  choices=c("Select variable type", "Continuous", "Discrete"))
    }
  })
  
  output$var_type2a <- renderUI({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_select2)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_select2=="Select a variable") {
      return()
    } else {
      selectInput("var_type2a", "Is Variable 2 continuous or discrete?",
                  choices=c("Select variable type", "Continuous", "Discrete"))
    }
  })
  
  output$type_text <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else {
      b <- 'After selecting the variable types, proceed to one of the next tabs, "Data exploration - Graphical" or "Data exploration - Tabular", if you would like to examine your data either graphically or tabularly. (This may be particularly helpful if both of your variables are continuous.) Otherwise, proceed to the tab "Calculate association", to continue the decision process and select a measure of association.'
    }
  })
  
  output$var_type1b <- renderUI({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a!="Discrete") {
      return()
    } else {
      selectInput("var_type1b", "Is Variable 1 nominal or ordinal?",
                  choices=c("Select variable type", "Nominal", "Ordinal"))
    }
  })
  
  output$var_type2b <- renderUI({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type2a!="Discrete") {
      return()
    } else {
      selectInput("var_type2b", "Is Variable 2 nominal or ordinal?",
                  choices=c("Select variable type", "Nominal", "Ordinal"))
    }
  })
  
  output$text_graphical <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" | input$var_type2a=="Continuous") {
      return()
    } else if (input$var_type1a=="Discrete" & input$var_type2a=="Discrete") {
      print('The variables you have selected are both discrete. If you would like to explore the selected variables tabularly, please click on the "Data exploration - Tabular" tab.')
    }
  })
  
  output$text_tabular <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Discrete" & input$var_type2a=="Discrete") {
      return()
    } else if (input$var_type1a=="Continuous" | input$var_type2a=="Continuous") {
      print('The variables you have selected are not both discrete. If you would like to explore the selected variables graphically, please click on the "Data exploration - Graphical" tab. The selected variables can be viewed by clicking the "View dataset" tab and using the provided filters.')
    }
  })
  
  #without the following two observe functions, changing from a discrete to a continuous variable in the type a dropdown after already specifying the type b dropdown and without refreshing the app causes a bug. as there is no type b option for continuous as there is for discrete (i.e. nominal vs. ordinal), changing the type a option from discrete to continuous does not reset the type b option without these functions.
  
  observe({
    choices <- input$var_type1a
    updateSelectInput(session, "var_type1b", choices=choices)
  })
  
  observe({
    choices <- input$var_type2a
    updateSelectInput(session, "var_type2b", choices=choices)
  })
  
  #the following eight outputs have largely to do with the continuous/continuous data exploration, which requires checking more assumptions than the other pairs of types.
  output$royston_text <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      paste("Are the variables bivariately normally distributed? One way of determining this is to use the Royston test. The output from this test includes its judgment of whether or not the data are bivariately normally distributed (using hypothesis testing).")
    }
  })
  
  #we use verbatimTextOutput here so that the output from the royston test in the mvn package is presented legibly; we use invisible() so that there is no output within it (i.e. "NULL")  
  output$royston_test <- renderPrint({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      invisible()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      invisible()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      Inputs <- data.frame(dat()[,input$var_select1],dat()[,input$var_select2])
      roystonTest(Inputs, qqplot=FALSE)
    } else {
      invisible()
    }
  })
  
  output$text_qq <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      paste("However, there are other ways to determine if two variables are multivariate normal, such as graphically. A quantile-quantile (Q-Q) plot for the chi-squared distribution is a standard graphical way to compare two distributions, by comparing the proposed distribution with its theoretical quantiles. The points should lie on or close to the identity line. In this case, the proposed distribution is for the squared Mahalanobis distance between the two variables.")
    }
  })
  
  output$explore_hist <- renderPlot({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else {
      if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
        Inputs <- data.frame(dat()[,input$var_select1],dat()[,input$var_select2])
        a <- roystonTest(Inputs, qqplot=TRUE)
      } else if (input$var_type1a=="Continuous" & input$var_type2a=="Discrete") {
        boxplot(dat()[, input$var_select1] ~ factor(dat()[, input$var_select2]),
                xlab=input$var_select2, ylab=input$var_select1,
                main=paste("Box-and-whisker plot of", input$var_select1, "vs", input$var_select2))
      } else if (input$var_type1a=="Discrete" & input$var_type2a=="Continuous") {
        boxplot(dat()[, input$var_select2] ~ factor(dat()[, input$var_select1]),
                xlab=input$var_select1, ylab=input$var_select2,
                main=paste("Box-and-whisker plot of", input$var_select2, "vs", input$var_select1))
      }
    }
  })
  
  output$explore_hist_explanation <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if ((input$var_type1a=="Continuous" & input$var_type2a=="Discrete") | (input$var_type1a=="Discrete" & input$var_type2a=="Continuous")) {
    print('A box-and-whisker plot is an easy visual summary of a continuous and a discrete variable. The "box" part of the plot extends from the first quartile to the third quartile (the interquartile range); the median is between the two. The end points are drawn at 1.5 times the interquartile range, or they are drawn at the minimum and maximum vales. The "whiskers" are drawn to the data\'s minimum and maximum, and are a representation of the data\'s variability.')
    }
  })
  
  output$persp_text <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      paste("Perspective plots are another way of examining bivariate normality. The X and Y dimensions illustrate the values of the two variables, and the Z dimension illustrates the value of the bivariate probability density function. If the two variables are bivariate normal, the Z dimension should look like a bell-shaped curve.")
    } else {
      return()
    }
  })
  
  output$persp_plot <- renderPlot({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      Inputs <- data.frame(dat()[,input$var_select1],dat()[,input$var_select2])
      mvnPlot(roystonTest(Inputs), type = "persp", default = FALSE, xlab="", ylab="", main="Perspective plot")
      mtext(input$var_select1,1,1)
      mtext(input$var_select2,4,-12,padj=1)
    } else {
      return()
    }
  })
  
  output$text_plot <- renderText({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else {
      if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
        paste("Are the variables linearly related? If the curve below looks like a straight line, then this may be the case.")
      }
    }
  })
  
  output$explore_plot <- renderPlot({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      par(mfrow=c(1, 1))
      plot(dat()[, input$var_select1], dat()[, input$var_select2], xlab=input$var_select1,
           ylab=input$var_select2, main="Scatter plot with LOWESS smoothing line", pch=20, cex=0.65)
      lines(lowess(dat()[, input$var_select1], dat()[, input$var_select2]), col="red", lwd=2)
    }
  })
  
  output$explore_table <- renderTable({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Discrete" & input$var_type2a=="Discrete") {
      table(dat()[, input$var_select1], dat()[, input$var_select2])
    }
  })
  
  #this output applies just to continuous/continuous
  output$choose_pearson <- renderUI({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      selectInput("choose_pearson", "Are your variables bivariate normally distributed, linearly related, and do they contain no influential outliers?",
                  choices=c("Select answer", "Yes", "No"))
    }
  })
  
  #this output and the next one apply just to continuous/ordinal
  #to paste text with breaks in it, can treat as HTML; can assign the different chunks to different names and then paste them with <br/> separator; must wrap in HTML    
  output$interpret <- renderUI({
    str1 <- "Which of the following interpretations do you prefer?"
    str2 <- paste("(A) The proportion of variance explained by the correlation between", input$var_select1, "and", input$var_select2)
    str3 <- paste("(B) The difference between the percentage of concordant and discordant pairs between", input$var_select1, "and", input$var_select2)
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Discrete") {
      if (input$var_type2b=="Ordinal") {
        HTML(paste(str1, str2, str3, sep = '<br/>'))
      }
    } else if (input$var_type1a=="Discrete" & input$var_type2a=="Continuous") {
      if (input$var_type1b=="Ordinal") {
        HTML(paste(str1, str2, str3, sep = '<br/>'))
      }
    }
  })
  
  output$choose_interpret <- renderUI({
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Discrete") {
      if (input$var_type2b=="Ordinal") {
        selectInput("interpret", "What is your preferred interpretation?",
                    choices=c("Select interpretation", "(A)", "(B)"))
      }
    } else if (input$var_type1a=="Discrete" & input$var_type2a=="Continuous") {
      if (input$var_type1b=="Ordinal") {
        selectInput("interpret", "What is your preferred interpretation?",
                    choices=c("Select interpretation", "(A)", "(B)"))
      }
    }
  })
  
  #the following four observe functions are required for situations in which you change certain early inputs without refreshing the app. the if/then logic is complicated and not all paths include the same inputs; therefore, certain inputs can "carry over" if you try to retrace your steps in choosing a measure, and this can be unintentionally included in the if/then logic determining which dropdowns you see. the result is extra dropdowns appearing. the observe functions reset these values each time you choose whether the variables are continuous or discrete, essentially resetting at step 1.
  
  observe({
    choices <- input$var_type1a
    updateSelectInput(session, "var_type1b", choices=choices)
  })
  
  observe({
    choices <- input$var_type2a
    updateSelectInput(session, "var_type2b", choices=choices)
  })
  
  observe({
    choices <- input$var_type1a
    updateSelectInput(session, "choose_pearson", choices=choices)
  })
  
  observe({
    choices <- input$var_type2a
    updateSelectInput(session, "choose_pearson", choices=choices)
  })
  
#this is the "choose your measure of association" step for all paths except those with an extra step (for which the true "choose your measure" step is included in output$choose_cor2 or output$choose_cor3). for those paths, this step includes a different dropdown anterior to "choose your measure of association", even though we have named the output "choose_cor".
  
  output$choose_cor <- renderUI({
    str1 <- paste("Which of the following interpretations do you prefer?")
    str2 <- paste("(A) The proportion of variance explained by the correlation between", input$var_select1, "and", input$var_select2)
    str3 <- paste("(B) The difference between the percentage of concordant and discordant pairs between", input$var_select1, "and", input$var_select2)
    if (is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type") {
      return()
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
      if (input$choose_pearson=="Select answer") {
        return()
      } else if (input$choose_pearson=="Yes") {
        selectInput("choose_cor", "Choose the measure of association",
                    choices=c("Select a measure of association",
                              "Pearson's r"))
      } else if (input$choose_pearson=="No") {
        a <- HTML(paste(str1, str2, str3, sep = '<br/>'))
        b <- selectInput("interpret", "What is your preferred interpretation?",
                         choices = c("Select interpretation", "(A)", "(B)"))
        return(list(a, b))
      }
    } else if (input$var_type1a=="Continuous" & input$var_type2a=="Discrete") {
      if (is.null(input$var_type2b)==TRUE) {
        return()
      } else if (input$var_type2b=="Select variable type") {
        return()
      } else if (input$var_type2b=="Nominal") {
          selectInput("choose_cor", "Choose the measure of association",
                      choices=c("Select a measure of association",
                                "Coefficient of determination"))
        } else if (input$var_type2b=="Ordinal") {
          if (input$interpret=="(A)") {
          selectInput("choose_cor", "Choose the measure of association",
                      choices=c("Select a measure of association",
                                "Spearman's \U03C1"))
        } else if (input$interpret=="(B)") {
          selectInput("choose_cor", "Choose the measure of association",
                      choices=c("Select a measure of association",
                                "Kendall's \U03C4"))
        } else {
          return()
        }
      }
    } else if (input$var_type2a=="Continuous" & input$var_type1a=="Discrete") {
        if (is.null(input$var_type1b)==TRUE) {
          return()
        } else if (input$var_type1b=="Select variable type") {
          return()
        } else if (input$var_type1b=="Nominal") {
          selectInput("choose_cor", "Choose the measure of association",
                      choices=c("Select a measure of association",
                                "Coefficient of determination"))
        } else if (input$var_type1b=="Ordinal") {
          if (input$interpret=="(A)") {
            selectInput("choose_cor", "Choose the measure of association",
                        choices=c("Select a measure of association",
                                  "Spearman's \U03C1"))
          } else if (input$interpret=="(B)") {
            selectInput("choose_cor", "Choose the measure of association",
                        choices=c("Select a measure of association",
                                  "Kendall's \U03C4"))
          } else {
            return()
          }
        }
      } else if (input$var_type1a=="Discrete" & input$var_type2a=="Discrete") {
        if (is.null(input$var_type1b)==TRUE | is.null(input$var_type2b)==TRUE) {
          return()
        } else if (input$var_type1b=="Select variable type" | input$var_type2b=="Select variable type") {
          return()
        } else {
          if (input$var_type1b=="Nominal" & input$var_type2b=="Nominal") {
            selectInput("choose_cor", "Choose the measure of association",
                        choices=c("Select a measure of association",
                                  "Cohen's \U03BA",
                                  "Cramer's V",
                                  "Spearman's \U03C1"))
          } else if (input$var_type1b=="Nominal" & input$var_type2b=="Ordinal") {
            return()
          } else if (input$var_type1b=="Ordinal" & input$var_type2b=="Nominal") {
            return()
          } else if (input$var_type1b=="Ordinal" & input$var_type2b=="Ordinal") {
            a <- HTML(paste(str1, str2, str3, sep = '<br/>'))
            b <- selectInput("interpret", "What is your preferred interpretation?",
                             choices = c("Select interpretation", "(A)", "(B)"))
            return(list(a, b))
          }
        }
      }
  })
  
  output$choose_cor2 <- renderUI({
    if (is.null(input$choose_pearson)==TRUE | is.null(input$interpret)==TRUE) {
      return()
    } else if (input$choose_pearson!="No") {
      return()
    } else if (input$choose_pearson=="No" & input$interpret=="(A)") {
      selectInput("choose_cor", "Choose the measure of association",
                  choices=c("Select a measure of association",
                            "Spearman's \U03C1"))
    } else if (input$choose_pearson=="No" & input$interpret=="(B)") {
      selectInput("choose_cor", "Choose the measure of association",
                  choices=c("Select a measure of association",
                            "Kendall's \U03C4"))
    } else {
      return()
    }
  })
  
  output$choose_cor3 <- renderUI({
    if (is.null(input$var_type1b)==TRUE | is.null(input$var_type2b)==TRUE | is.null(input$interpret)==TRUE) {
      return()
    } else if (input$var_type1b!="Ordinal" | input$var_type2b!="Ordinal") {
      return()
    } else if (input$var_type1b=="Ordinal" & input$var_type2b=="Ordinal" & input$interpret=="(A)") {
      selectInput("choose_cor", "Choose the measure of association",
                  choices=c("Select a measure of association",
                            "Spearman's \U03C1"))
    } else if (input$var_type1b=="Ordinal" & input$var_type2b=="Ordinal" & input$interpret=="(B)") {
      selectInput("choose_cor", "Choose the measure of association",
                  choices=c("Select a measure of association",
                            "Kendall's \U03C4"))
    } else {
      return()
    }
  })
  
  output$tab3text <- renderText({
    if (is.null(dat())==TRUE | is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE | is.null(input$choose_cor)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type" | input$choose_cor=="Select a measure of association") {
      return()
    } else {
      paste('Once you have selected a measure of association, proceed to the final tab: "Answer and explanation". The answer may take several seconds to appear, due to computation time for the bootstrapping algorithm. Please note that there are no association measures included in this application for two discrete variables in the case where one variable is of type "ordinal" and the other is of type "nominal".')
    }
  })
  
  output$correlation <- renderText({
    if (is.null(dat())==TRUE | is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE | is.null(input$choose_cor)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type" | input$choose_cor=="Select a measure of association") {
      return()
    } else {
      if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
        if (input$choose_cor=="Pearson's r") {
          final_estimate <- cor(dat()[, input$var_select1], dat()[, input$var_select2])
          set.seed(123)
          samp_r <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_r[i] <- cor(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
          }
          final_ci <- round(c(sort(samp_r)[25], sort(samp_r)[976]), 2)
          paste(round(final_estimate, 2), " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          final <- spearman.ci(as.numeric(as.character(dat()[, input$var_select1])),
                               as.numeric(as.character(dat()[, input$var_select2])))
          final_estimate <- round(final$estimate, 2)
          final_ci <- round(final$conf.int, 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          final <- Kendall(dat()[, input$var_select1], dat()[, input$var_select2])
          final_estimate <- round(final$tau, 2)
          set.seed(123)
          samp_tau <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_temp <- Kendall(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
            samp_tau[i] <- samp_temp$tau
          }
          final_ci <- round(c(sort(samp_tau)[25], sort(samp_tau)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        }
      } else if (input$var_type1a=="Continuous" & input$var_type2a=="Discrete") {
        if (input$choose_cor=="Coefficient of determination") {
          final <- lm(dat()[, input$var_select1] ~ factor(dat()[, input$var_select2]))
          final_estimate <- round(summary(final)$r.squared, 2)
          set.seed(123)
          samp_R2 <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_fit <- lm(samp_dat[, input$var_select1] ~ factor(samp_dat[, input$var_select2]))
            samp_R2[i] <- round(summary(samp_fit)$r.squared, 2)
          }
          final_ci <- round(c(sort(samp_R2)[25], sort(samp_R2)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          final <- spearman.ci(as.numeric(as.character(dat()[, input$var_select1])),
                               as.numeric(as.character(dat()[, input$var_select2])))
          final_estimate <- round(final$estimate, 2)
          final_ci <- round(final$conf.int, 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          final <- Kendall(dat()[, input$var_select1], dat()[, input$var_select2])
          final_estimate <- round(final$tau, 2)
          set.seed(123)
          samp_tau <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_temp <- Kendall(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
            samp_tau[i] <- samp_temp$tau
          }
          final_ci <- round(c(sort(samp_tau)[25], sort(samp_tau)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        }
      } else if (input$var_type1a=="Discrete" & input$var_type2a=="Continuous") {
        if (input$choose_cor=="Coefficient of determination") {
          final <- lm(dat()[, input$var_select2] ~ factor(dat()[, input$var_select1]))
          final_estimate <- round(summary(final)$r.squared, 2)
          set.seed(123)
          samp_R2 <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_fit <- lm(samp_dat[, input$var_select2] ~ factor(samp_dat[, input$var_select1]))
            samp_R2[i] <- round(summary(samp_fit)$r.squared, 2)
          }
          final_ci <- round(c(sort(samp_R2)[25], sort(samp_R2)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          final <- spearman.ci(as.numeric(as.character(dat()[, input$var_select1])),
                               as.numeric(as.character(dat()[, input$var_select2])))
          final_estimate <- round(final$estimate, 2)
          final_ci <- round(final$conf.int, 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          final <- Kendall(dat()[, input$var_select1], dat()[, input$var_select2])
          final_estimate <- round(final$tau, 2)
          set.seed(123)
          samp_tau <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_temp <- Kendall(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
            samp_tau[i] <- samp_temp$tau
          }
          final_ci <- round(c(sort(samp_tau)[25], sort(samp_tau)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        }
      } else if (input$var_type1a=="Discrete" & input$var_type2a=="Discrete") {
        if (input$choose_cor=="Cohen's \U03BA") {
          final <- cohen.kappa(data.frame(factor(dat()[, input$var_select1]),
                                          factor(dat()[, input$var_select2])))
          final_estimate <- round(final$kappa, 2)
          set.seed(123)
          samp_kappa <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_temp <- cohen.kappa(data.frame(factor(samp_dat[, input$var_select1]), factor(samp_dat[, input$var_select2])))
            samp_kappa[i] <- samp_temp$kappa
          }
          final_ci <- round(c(sort(samp_kappa)[25], sort(samp_kappa)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Cramer's V") {
          final <- CramerV(dat()[, input$var_select1], dat()[, input$var_select2])
          final_estimate <- round(final[1], 2)
          set.seed(123)
          samp_v <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_v[i] <- CramerV(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
          }
          final_ci <- round(c(sort(samp_v)[25], sort(samp_v)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Goodman and Kruskal's \U03B3") {
          final_estimate <- rococo(dat()[, input$var_select1], dat()[, input$var_select2])
          set.seed(123)
          samp_gamma <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_gamma[i] <- rococo(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
          }
          final_ci <- round(c(sort(samp_gamma)[25], sort(samp_gamma)[976]), 2)
          paste(round(final_estimate, 2), " (", final_ci[1], ", ", final_ci[2], ")", sep="")
          
        } else if (input$choose_cor=="Kendall's \U03C4") {
          final <- Kendall(dat()[, input$var_select1], dat()[, input$var_select2])
          final_estimate <- round(final$tau, 2)
          set.seed(123)
          samp_tau <- c()
          for (i in 1:1000) {
            samp <- sample.int(nrow(dat()), replace=TRUE)
            samp_dat <- dat()[samp, ]
            samp_temp <- Kendall(samp_dat[, input$var_select1], samp_dat[, input$var_select2])
            samp_tau[i] <- samp_temp$tau
          }
          final_ci <- round(c(sort(samp_tau)[25], sort(samp_tau)[976]), 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          final <- spearman.ci(as.numeric(as.character(dat()[, input$var_select1])),
                               as.numeric(as.character(dat()[, input$var_select2])))
          final_estimate <- round(final$estimate, 2)
          final_ci <- round(final$conf.int, 2)
          paste(final_estimate, " (", final_ci[1], ", ", final_ci[2], ")", sep="")
        }
      }
    }
  })
  
  output$explanation <- renderText({
    if (is.null(dat())==TRUE | is.null(input$var_select1)==TRUE | is.null(input$var_type1a)==TRUE | is.null(input$var_select2)==TRUE | is.null(input$var_type2a)==TRUE | is.null(input$choose_cor)==TRUE) {
      return()
    } else if (input$var_select1=="Select a variable" | input$var_type1a=="Select variable type" | input$var_select2=="Select a variable" | input$var_type2a=="Select variable type" | input$choose_cor=="Select a measure of association") {
      return()
    } else {
      if (input$var_type1a=="Continuous" & input$var_type2a=="Continuous") {
        if (input$choose_cor=="Pearson's r") {
          print("The Pearson product-moment correlation coefficient (r) is a measure of the linear relationship between two variables. Its values range from -1 to 1, where the extreme values indicate perfect linear relationships, and 0 indicates the absence of a linear relationship. Positive and negative coefficients correspond to a positive and negative linear relationship respectively.")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          print("Spearman's rank correlation coefficient (\U03C1) measures the extent to which the relationship between two variables can be explained using a monotonic function. When there are no ties in the data, correlations of +1 and -1 are obtained. Positive and negative values correspond to a positive and negative functional relationship, respectively.")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          print("Kendall's \U03C4 is a measure of the ordinal association between two variables. Ordinal association is determined by considering corresponding pairs of observations for each of the variables, and determining whether the orderings for the pairs are the same or not. A coefficient of 1 means that all pairs of orderings are concordant, whereas -1 indicates complete discordance.")
        } else {
          return()
        }
      } else if (input$var_type1a=="Continuous" & input$var_type2a=="Discrete") {
        if (input$choose_cor=="Coefficient of determination") {
          print("The coefficient of determination (R\U00B2) is the proportion of variance in the outcome variable that is predictable using the explanatory variable. Its value ranges from 0 (which indicates that none of the variation in the response is attributable to the explanatory variable) to 1 (where the independent variable fully explains the variation in the response).")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          print("Spearman's rank correlation coefficient (\U03C1) measures the extent to which the relationship between two variables can be explained using a monotonic function. When there are no ties in the data, correlations of +1 and -1 are attained. Positive and negative values correspond to a positive and negative functional relationship, respectively.")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          print("Kendall's \U03C4 is a measure of the ordinal association between two variables. Ordinal association is determined by considering corresponding pairs of observations for each of the variables, and determining whether the orderings for the pairs are the same or not. A coefficient of 1 means that all pairs of orderings are concordant, whereas -1 indicates complete discordance.")
        } else {
          return()
        }
      } else if (input$var_type1a=="Discrete" & input$var_type2a=="Continuous") {
        if (input$choose_cor=="Coefficient of determination") {
          print("The coefficient of determination (R\U00B2) is the proportion of variance in the outcome variable that is predictable using the explanatory variable. Its value ranges from 0 (which indicates that none of the variation in the response is attributable to the explanatory variable) to 1 (where the independent variable fully explains the variation in the response).")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          print("Spearman's rank correlation coefficient (\U03C1) measures the extent to which the relationship between two variables can be explained using a monotonic function. When there are no ties in the data, correlations of +1 and -1 are attained Positive and negative values correspond to a positive and negative functional relationship, respectively.")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          print("Kendall's \U03C4 is a measure of the ordinal association between two variables. Ordinal association is determined by considering corresponding pairs of observations for each of the variables, and determining whether the orderings for the pairs are the same or not. A coefficient of 1 means that all pairs of orderings are concordant, whereas -1 indicates complete discordance.")
        } else {
          return()
        }
      } else if (input$var_type1a=="Discrete" & input$var_type2a=="Discrete") {
        if (input$choose_cor=="Cohen's \U03BA") {
          print("Cohen's \U03BA is a measure of inter-rater reliability. Specifically, for two raters, it assesses the agreement between each rater's classification of N items into G disjoint groups. A coefficient value of 1 indicates perfect agreement, while a coefficient of 0 means that the agreement is no better than what could be obtained by mere chance.")
        } else if (input$choose_cor=="Cramer's V") {
          print("Cramer's V measures the correlation between two nominal variables. Its value is 0 when there is no association between the two variables, and 1 when there is complete agreement.")
        } else if (input$choose_cor=="Goodman and Kruskal's \U03B3") {
          print("Goodman and Kruskal's \U03B3 measures the rank correlation of two (ordinal) variables. The statistic is calculated by considering corresponding pairs of observations for each of the variables, and determining whether the orderings for the pairs are the same (concordant) or not. A correlation of -1 is indicative of perfect discordance, while a value of 1 means there is perfect concordance; the coefficient is 0 when there is no association between the two variables.")
        } else if (input$choose_cor=="Kendall's \U03C4") {
          print("Kendall's \U03C4 is a measure of the ordinal association between two variables. Ordinal association is determined by considering corresponding pairs of observations for each of the variables, and determining whether the orderings for the pairs are the same or not. A coefficient of 1 means that all pairs of orderings are concordant, whereas -1 indicates complete discordance.")
        } else if (input$choose_cor=="Spearman's \U03C1") {
          print("Spearman's rank correlation coefficient (\U03C1) measures the extent to which the relationship between two variables can be explained using a monotonic function. When there are no ties in the data, correlations of +1 and -1 are attained. Positive and negative values correspond to a positive and negative functional relationship, respectively.")
        } else {
          return()
        }
      }
    }
  })
}

shinyApp(ui=ui, server=server)
