 #This R script will contain background codes (functions, calculations, etc.) of our final project app.
#May merge with ui.R if possible.

#source("check_packages.R")
#check_packages(c("shiny","shinythemes","shinydashboard","dplyr"))
library(shiny)
library(dplyr)
library(rvest)
library(knitr)
library(ggplot2)
library(maps)
library(ggmap)

#Dataset to use(will change to dataset with weather)
#load("~/R/final shiny app/CitiesDB.final.Rda")
load("DB_Normalized.Rda")
dataset<-CitiesDB_Normalized
#CitiesDB$Size <- as.numeric(CitiesDB$Size)
#CitiesDB$Weather <- as.numeric(CitiesDB$Weather)

#Global Functions -- this filters over only one column
filter.function <- function(df, var) {
  df1 <- df[order(df[, var], decreasing = T), ]
  df2 <- df1[df[, var] > quantile(df[, var], .4),]
  return(df2)
}

#CATEGORICAL
filter.function2 <- function(df, var, user_input) {
  df2 <- df1[var==user_input]
  if(nrow(df2)<=5) {
    return(df)
  } else {
    return(df2)
  }
}

#Alt. version
#filter_var1 = function(citylist_start, string01) {
# citylist_end = citylist[eval(parse(text=string01)),]
# if(nrow(total)<=5) {
#     return(citylist_start)
#  } else {
#     return(citylist_end)
#  }
#}
#[one for each input parameter]


shinyServer(function(input, output) {
  
  sliderValues <- reactive({
    # Compose data frame with input preferences and rankings and
    #order the dataframe using ranked variables
 
    rank.indicators <- data.frame(
      Name = c("Size","Economics","Diversity","Cosmopolitanism","Average Temperature"),
      Ranking = as.numeric(c(input$range_1,
                             input$range_2,
                             input$range_3,
                             input$range_4,
                             input$range_5)),
      stringsAsFactors = FALSE
    )
    
    #Check if the input indicators are all different
    #Consider this code: length(Ranking) == length(unique(Ranking))
    #If not, return some message such as "Only one item can be ranked 1"
    
    #Additional input
    #csize.indicator = as.numeric(input$range_1_2)
    
    #SORTING FOR NUMERIC VARIABLES
  
    # rank.indicators <- rank.indicators[order(rank.indicators[, "Ranking"]), ]
    # index <- as.character(rank.indicators$Name)
    # final.data <- list()
    # final.data[[1]] <- CitiesDB
    # for (i in 1:length(index)) {
    #   final.data[[i + 1]] <- filter.function (final.data[[i]], index[i])
    # }
    
    final.data = CitiesDB_Normalized
    final.data$score = final.data$Size_normalized*2^(5-as.numeric(rank.indicators[which(rank.indicators$Name=="Size"),]$Ranking)) +
                       #A possible method is to multiply the above line by as.numeric(final.data$Size_scale == csize.indicator)
                       final.data$Economics_normalized*2^(5-as.numeric(rank.indicators[which(rank.indicators$Name=="Economics"),]$Ranking)) +
                       final.data$Diversity_normalized*2^(5-as.numeric(rank.indicators[which(rank.indicators$Name=="Diversity"),]$Ranking)) +
                       final.data$Cosmopolitanism_normalized*2^(5-as.numeric(rank.indicators[which(rank.indicators$Name=="Cosmopolitanism"),]$Ranking)) +
                       final.data$AverageTemperature_normalized*2^(5-as.numeric(rank.indicators[which(rank.indicators$Name=="Average Temperature"),]$Ranking))
                       #[Jonathan] To address the weather preferences, we can use ex.
                       #abs(as.numeric(x) - AverageTemperature_normalized), where x is an additional boolean object
                       #indicating warmer or colder preference from the UI (ex. warmer = T, colder = F)
    final.data_out = final.data[order(final.data$score, decreasing = T), ]
    final.data_out[1:5,] #This will be the output table
    #Consider just having the city list as the output without all the background data (ex. normalized values) by selecting multiple columns
    
  })
  
  
  # #ordered_indicators<- sort(c(range_1, range_2, range_3, range_4))
  # #Test codes that reads user-entered rankings and generate a string vector that lists parameters accordingly
  # priority_rank_test = reactive({
  #   priority_rank = rep(0, 5)
  #   priority_rank[input$indicator_input1] = "|Weather|"
  #   priority_rank[input$indicator_input2] = "|City Size|"
  #   priority_rank[input$indicator_input3] = "|Work Opportunities|"
  #   priority_rank[input$indicator_input4] = "|Political Preferences|"
  #   #priority_rank[input$indicator_inputN] = "|...Input parameter n|"
  #   print(priority_rank)
  # })
  #output$priority_rank_test = renderText({priority_rank_test()})
  
  
  output$myplot = renderPlot({
    # ggplot with proper reference to reactive function <<data.r()>>
    #library(ggplot2)
    #library(ggmap)
    #load us map data
    #map<-get_map(location='united states', zoom=4, maptype = "terrain",
    #source='google')
    #plot all states with ggplot
    #ggmap(map)
    
    library(ggplot2)
    library(maps)
    #load us map data
    all_states <- map_data("state")
    
    #load geocodes for final filtered data
    #locations <- cbind(geocode(as.character(data.civil$Location)), data.civil)
    
    #plot all states with ggplot
    p <- ggplot()
    p <-
      p + geom_polygon(
        data = all_states,
        aes(x = long, y = lat, group = group),
        colour = "white",
        fill = "grey10"
      )+ ggtitle("Top Cities to Live In")+
      geom_point(data=sliderValues(), aes(x = lon, y = lat, color = City,size=5))
    
    p
    
  })
  
  output$values <- renderTable({
    sliderValues()[,1:5]
    
  })
  
  output$values.plots<- renderPlot({
    library(ggplot2)    # load ggplot2 plotting package
    
    ggplot(data=sliderValues(), aes(x=City, y=Size)) +
      geom_point(aes(size=Size)) +
      scale_size_continuous(range=c(2,15)) +
      theme(legend.position = "none")
  })
  
  findjobs <- reactive({ 
    
    query = as.character(input$text.job)
    loc = as.character(sliderValues()[1,1])
    session <- html_session("http://www.indeed.com")
    form <- html_form(session)[[1]]
    filled_form <- set_values(form, q = query, l = loc)
    session1 <- submit_form(session, filled_form )
    #Salary Information
    salary_links <- session1 %>%
      html_nodes(css = "#resultsCol li:nth-child(2) a") %>%
      html_attr("href")
    salary_links <- paste(session$url, salary_links, sep='')
    salaries <- lapply(salary_links, . %>%
                         read_html() %>%
                         html_nodes("#salary_display_table .salary") %>%
                         html_text())
    salary <- unlist(salaries)
    #WEb Urls and Job Titles
    #weburl
    data_sci_indeed <- session1
    job_title <- data_sci_indeed %>%
      html_nodes("[itemprop=title]") %>%
      html_text()
    #Companies and Locations
    company <- data_sci_indeed %>%
      html_nodes("[itemprop=hiringOrganization]") %>%
      html_text()
    
    location <- data_sci_indeed %>%
      html_nodes("[itemprop=addressLocality]") %>%
      html_text()
    #Descriptions and Links
    description <- data_sci_indeed %>%
      html_nodes("[itemprop=description]") %>%
      html_text()
     link <- data_sci_indeed %>%
      html_nodes("[itemprop=title]") %>%
      html_attr("href")
    link <- paste('[Link](https://www.indeed.com', link, sep='')
    link <- paste(link, ')', sep='')
    #Jobs
    indeed_jobs <- data.frame(job_title,company,location,description,link)
    
   indeed_jobs
    
    })
  
  output$jobtable <- renderTable( {findjobs()} )
  
  output$downloadJobs <- downloadHandler(
    filename = "indeed_jobs.csv",
    content = function(file_temp) {
      write.csv(findjobs(), file_temp)
    }
  )
  
  #We need codes that could:
  #Scrape a list of US cities and their info from potentical data sources, and store them in a list;
  #Filter the list of cities based on different input parameters (creating functions likely necessary);
  #Perhaps also rank the list of cities to select the final top 5 choices, then list them as part of the output;
  #Also list the city info and/or reasons for the final choices, if possible;
  #(What if after all the filters, less than 5 cities remain?)
  #Scrape the top jobs available based on preferred job type if indicated, then list them as part of the output
  
  
})