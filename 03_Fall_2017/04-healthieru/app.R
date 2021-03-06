source("webscrape.R")

ui <- fluidPage(
  titlePanel("Healthier U - Weight Loss Program"),    # Adds Title
  #Adds Navigation Bar at the top of the Page
  navbarPage("HealthyU", 
             #Creates a tab
             tabPanel("Instructions",
                      #Under this tab the main Panel depicts the paragraph below
                      mainPanel(
                        #Paragraph output
                        p("Welcome to our Weight Loss Program!"),
                        #break
                        br(),
                        #Paragraph output
                        p("The user will select the following criteria: gender, height, current weight (left knob on the slider), desired weight (right knob on the slider), and commitment. 
                          The program will use those data points to anaylze whether the user is fit for a weight loss program using the BMI formula. 
                          If the user is underweight, HealthierU will allow the user to know a weight loss program is not recommended. 
                          If the user is not underweight, then HealthierU will move on to further analysis regarding workout options."),
                        #break
                        br(),
                        #Paragraph output
                        p("Let's get started!"),
                        #break
                        br(),
                        #Image is inserted
                        img(src="exercise.png")
                        )),
             #Another Tab is inserted
             tabPanel("Personal Information",
                      sidebarPanel(
                        # Adds buttons for selecting gender and metric system
                        radioButtons("gender", "Gender", c("Male","Female")),
                        radioButtons("metric_sys", "Units", choices = c("Metric","Standard"), selected = "Standard"),
                        # Adds result of DEPENDENT slider bars created in output below
                        wellPanel(uiOutput("ui_height")),     
                        wellPanel(uiOutput("ui_weight")),     
                        # Adding input slider bars reference name, label, min/max values, and starting value respectively
                        sliderInput("target.date", "Maximum Number of Weeks To Achieve Desired Weight", min = 1, max = 100, value = 50),
                        sliderInput("intensity", "Number of Hours Devoted to Exercising Every Week", min = 1, max = 20, value = 5)
                      )
             ),
             #Inserts another tab 
             tabPanel("BMI Analysis",
                      mainPanel(
                        plotOutput("weight_distribution"),    # Prints the bmi graph that was created in output below
                        textOutput("labelBMI"),               # Prints the text of your BMI that was created in ouput below
                        textOutput("labelTargetBmi"),         # Prints the text of your target BMI that was created in output below
                        textOutput("labelUSABmi"),            # Prints the text of the your BMI compared to the US pop as created in output below
                        textOutput("labelUSATargetBmi"),      # Prints the text of the your target BMI compared to the US pop as created in output below
                        span(textOutput("labelBmiNotes"),style="color:red") #Prints a note on BMI in red
                      )
             ),
             #Inserts another tab
             tabPanel("Workout Analysis",
                      tableOutput("exercises")              # Prints the exercises table that was created in output below
             )
             )
  )


################## Creating Output (server) section of Shiny App #######################

server <- function(input, output) {
  
  ### Creating dependent slider bars that go into input section ###    
  # Makes a cm or in Slider Bar depending on whether user selects Metric or Standard.
  output$ui_height <- renderUI({
    switch(input$metric_sys,
           "Metric" = sliderInput("height", "Current Height (cm)", min = 140, max = 215, value = 178),   # If metric_sys = "Metric", use this slider bar
           "Standard" = sliderInput("height", "Current Height (in)", min = 55, max = 85, value = 70)     # If metric_sys = "Standard", use this slider bar
    )
  })
  
  # Makes a kg or lbs Slider Bar depending on whether user selects Metric or Standard.
  output$ui_weight <- renderUI({
    switch(input$metric_sys,
           "Metric" = sliderInput("weights", "Desired Weight & Current Weight (kg)", min = 40, max = 160,value = c(80,90)),   # If metric_sys = "Metric", use this slider bar
           "Standard" = sliderInput("weights", "Desired Weight & Current Weight (lbs)", min = 85, max = 350,value = c(180,200))   # If metric_sys = "Standard", use this slider bar
    )
  })
  
  #######################################################################################    
  
  ### Adds Graph for BMI Distribution ###
  #labelBMI
  output$labelBMI<-renderText({weight_distribution(input)$labelBMI})
  #labelTargetBmi
  output$labelTargetBmi<-renderText({weight_distribution(input)$labelTargetBmi})
  #labelUSABmi
  output$labelUSABmi<-renderText({weight_distribution(input)$labelUSABmi})
  #labelUSATargetBmi
  output$labelUSATargetBmi<-renderText({weight_distribution(input)$labelUSATargetBmi})
  #labelBmiNotes
  output$labelBmiNotes<-renderText({weight_distribution(input)$labelBmiNotes})
  
  #Create a plot from the weight_distribution function input
  output$weight_distribution <- renderPlot({
    weight_distribution(input)$plot
  })
  
  weight_distribution <- function(input){
    
    # Checks if inputs are NULL, else returns NULL. 
    # Used to take away false error messages when ShinyApp initializes
    # Makes sure input is created only AFTER ShinyApp is finished fully loading
    req(input$height)  
    req(input$metric_sys)
    req(input$gender)
    req(input$weights)
    
    # Turning reactive variables into more feasable variables
    gender <- input$gender          # Converts Shiny App user input for gender slidebar into one variable
    height <- input$height          # Converts Shiny App user input for height slidebar into one variable
    weight <- input$weights[2]      # Converts Shiny App user input for current weight slidebar into one variable
    target.weight<-input$weights[1] # Converts Shiny App user input for target weight slidebar into one variable
    units<-input$metric_sys         # Converts Shiny App user input for metric system option into one variable
    bmi <- health.analysis(height,weight, target.weight,units)$BMI                # Gets BMI from previously specified function
    diagnosis <- health.analysis(height,weight, target.weight,units)$Diagnosis    # Gets BMI diagnosis from previously specified function
    target.bmi <- health.analysis(height,weight, target.weight,units)$Target.BMI  # Converts height (in) and weight (lb) into target BMI
    target.diagnosis <- health.analysis(height,weight, target.weight,units)$Target.Diagnosis   # Gets BMI diagnosis from previously specified function
    
    # Changes parameters and colors of graph depending on selected gender
    if (gender == "Male") {
      mean <- 28.7                # Average BMI of U.S. Adult Males (20+ yrs old)
      sd <- sqrt(5223)*0.13       # Converting given standard error of mean and sample size into standard deviation
      fill1 <- "cadetblue1"       # Used for overall shading of normal curve
      fill2 <- "cadetblue3"       # Darker shade in between current and target BMI
      color1 <- "cadetblue4"      # Used for outline of normal curve
      y <- 0                      
    } else if (gender =="Female"){
      mean <- 29.2
      sd <- sqrt(5413)*0.17
      fill1 <- "lightpink1"
      fill2 <- "lightpink3"
      color1 <- "lightpink4"
      y <- 0.01                   # Only shifts the labels if gender=female to re-adjust for new y-axis
    }
    
    bmi.percent <- round(pnorm(bmi,mean,sd) * 100, 2)            # Rounds bmi to 2 decimal places just for the graph
    target.bmi.percent <- round(pnorm(target.bmi,mean,sd) * 100, 2)            # Rounds bmi to 2 decimal places just for the graph
    
    # Creates cumulative normal distribution graph shading in given BMI
    plot <- ggplot(data.frame(x=c(10,48)), aes(x)) +    # Creates plot from x = 10.6 to 40.6
      # Plots a normal curve with mean = 25.6, sd = 4. Colors area below light blue.
      stat_function(fun=dnorm, args=list(mean,sd), color = color1, size = 2, geom="area", fill=fill1, alpha = 0.4) +     
      scale_x_continuous(name="BMI") +            # Labels x axis "BMI"
      #ggtitle(paste0("Percentile of United States Adult ",gender, "s Less Than Current BMI")) +   # Adds a graph title
      theme_classic() +                           # Makes the background white theme
      # Shades the normal curve between current and target BMI a darker color
      stat_function(fun=dnorm, args=list(mean,sd), xlim=c(target.bmi,bmi), geom="area", fill=fill2, alpha = 0.7) +
      
      # Creates lines and text on graph
      geom_vline(xintercept=c(18.5,25,30)) +                 # Adds black vertical lines in desired x location
      geom_text(aes(mean,0, label=paste0("|", "\n", "U.S. Average")), color=color1) +  # Adds average tick mark
      geom_text(aes(bmi,0.0025, label=paste0("Current", "\n", "BMI", "\n", "|")), color=color1) +  # Adds average tick mark
      geom_text(aes(target.bmi,0.0025, label=paste0("Target","\n", "BMI", "\n", "|")), color=color1) +  # Adds average tick mark
      geom_text(aes(15,0.05-y, label="Underweight")) +       # Adds Underweight text in top corresponding region
      geom_text(aes(22,0.05-y, label="Normal Weight")) +     # Adds Normal Weight text in top corresponding region
      geom_text(aes(27.5,0.05-y, label="Overweight")) +      # Adds Overweight text in top corresponding region
      geom_text(aes(32,0.05-y, label="Obese"))    +          # Adds Obese text in top corresponding region
      theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())    # Removes y axis information
    
    labelBMI=paste("Your BMI: ", round(bmi,digits=1), " (", diagnosis, ")")
    labelTargetBmi=paste("\n","Your Target BMI: ", round(target.bmi,digits=1), " (", target.diagnosis, ")")
    labelUSABmi=paste0("% of U.S. Adult ",gender, "s < Your BMI: ", bmi.percent,"%")
    labelUSATargetBmi=paste0("\n", "% of U.S. Adult ",gender, "s < Your Target BMI: ", target.bmi.percent,"%")
    labelBmiNotes=paste0("Note: BMI may be a misinformative measure", "\n", " of health as it doesn't take into account",
                         "\n", " for muscle mass or body shape.")
    
    #Return a list of the following outputs
    return(list(
      plot=plot,
      labelBMI=labelBMI,
      labelTargetBmi=labelTargetBmi,
      labelUSABmi=labelUSABmi,
      labelUSATargetBmi=labelUSATargetBmi,
      labelBmiNotes=labelBmiNotes
      
      
    ))
  }
  
  #######################################################################################
  
  ### Creating Table of Exercises ###
  output$exercises <- renderTable({
    
    # Checks if inputs are NULL, else returns NULL. 
    # Used to take away false error messages when ShinyApp initializes
    # Makes sure input is created only AFTER ShinyApp is finished fully loading
    req(input$height)  
    req(input$metric_sys)
    req(input$gender)
    req(input$weights)
    
    # Turning reactive variables into more feasable variables
    units<-input$metric_sys           # Converts Shiny App user input for metric system option into one variable
    height <- input$height            # Converts Shiny App user input for height slidebar into one variable
    weight<-input$weights[2]          # Converts Shiny App user input for current weight slidebar into one variable
    target.weight<-input$weights[1]   # Converts Shiny App user input for desired weight slidebar into one variable
    bmi <- health.analysis(height,weight, target.weight,units)$BMI                   # Gets BMI from previously specified function
    target.bmi <- health.analysis(height,weight, target.weight,units)$Target.BMI     # Converts height (in) and weight (lb) into target BMI
    target.date<-input$target.date    # Converts Shiny App user input for desired date slidebar into one variable
    intensity<-input$intensity        # Converts Shiny App user input for intensity slidebar into one variable
    
    if (units == "Standard"){
      cal.per.week <- -((target.weight - weight) / target.date * 3500)   # How many calories should be lost per week on average
      reg_table<- reg_table %>% group_by(Activity)%>%
        mutate(burn.rate=mean(c(target.weight, weight))*standard,
               burn.calories=burn.rate*intensity) 
    } else if (units =="Metric"){
      cal.per.week <- -((target.weight - weight) / target.date * 3500/ 0.453592)   # How many calories should be lost per week on average
      reg_table<- reg_table %>% group_by(Activity)%>%
        mutate(burn.rate=mean(c(target.weight, weight))*metric,
               burn.calories=burn.rate*intensity) 
    }
    summary_table=reg_table %>% filter((.9*cal.per.week)<=burn.calories) 
    
    if (target.bmi < 17) {                 
      if (bmi > 17){                     # Checks to see if target weight is too low
        print("Desired weight is very underweight and may be unhealthy. Please consider a different weight.")
      } else{                            # Checks to see if current weight needs a weight loss program
        print("You are already very underweight and may not need a weight loss program.")
      }
    } else if (cal.per.week > 7000){       # Checks to see if weight loss plan is too extreme
      print("Losing more than 2 lbs/week may be considered unrealistic and unsafe.")
    } else if (nrow(summary_table)==0){    # Checks to see if any exercises are available
      print("No exercises match your criteria. Please change intensity and/or target date.")
    } else{
      # Prints all exercises that can burn that many calories per hour or more
      summary_table %>% 
        select(Activity,burn.rate) %>% 
        arrange(burn.rate) %>% 
        rename("Calories Per Hour"=burn.rate)
    }
  })
}
shinyApp(ui = ui, server = server)
