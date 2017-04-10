library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("sandstone"),#style="background-color:#D0D3D4;",#allowTransparent = TRUE
                  
              
  #tags$head(
    #  tags$style(type='text/css', "input { width: 100%; }","label {width: 100%; font-size:100%; font-style: 'Lobster', cursive;}")# control numericInput box
    #  ,
    #tags$style(type='text/css',"shiny-bound-output,.well {background-color: #99CCFF; font-size:100%; } "),#  control font / ineffective
  #),
  # Application title
  navbarPage(inverse=TRUE,"Flight Information", collapsible = TRUE,
             tabPanel("Introduction",
                      sidebarLayout(
                        sidebarPanel(h4("Enter the required information"),
                                     selectInput(inputId = "year",h5("Select Year"),
                                                 choices=list("2011" = 2011, "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                              "2015" = 2015), selected =2011),
                                     
                                     selectInput(inputId = "quarter",h5("Select Quarter"),
                                                 choices=list("1st Quarter"= 1,"2nd Quarter"= 2, "3rd Quarter"= 3,
                                                              "4th Quarter"= 4), selected =1),
                                     
                                     uiOutput("city_list_dep1"),#departure city selection
                                     uiOutput("city_list_arr1") #arrival city selection
                                     
                                     
                                     #div(class="busy",
                                     #   img(src="https://i.stack.imgur.com/e8nZC.gif",height = 72, width = 72)
                                     #)
                        , width = 3),
                        mainPanel(
                          wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px; width: 110%;background-color: #F5F5F5",
                          includeMarkdown("markdown.md"),
                          includeMarkdown("email_html.Rmd")
                          )
                        )
                      )
             ),
             navbarMenu("Data Display",
                        tabPanel("Raw Table", 
                                 span(helpText("In the “Raw Table” section, a sample data will be provided for our users. We have used the report from the Department of Transportation, especially the table 6 from the report. We have listed all the all departure city and arrival city pairs in selected year along with the millage, numbers of passengers, airplane fare and etc. Users could find each variable’s name from the “Data Variable Description” section. ")),
                                 wellPanel(id = "rPanel",style = "overflow-y:scroll; max-height: 600px; width: 100%;background-color: #F5F5F5",
                                           
                                           tableOutput("raw_table_out")
                                 
                                 )
                        ),
                        tabPanel("Data Variable Discrption", 
                                 span(helpText("In the “Data Variable Discription” section, we have provided the detailed explanations for each variable we have been using in this app. ")),
                                 tableOutput("MetaData")
                                 
                        )
             ),
            
             navbarMenu("Departure Analysis",
                        tabPanel("Average Fare", 
                                 fluidRow(
                                   
                                   plotOutput('stat_depart_city_out1', height = "800px"),
                                   downloadButton('download_formula_stat_depart_city1', 'Download the Plot'),
                                   span(helpText("Note: If the total number of city we can disply is less than 10 city, we only display the maximum number of city we have")),
                                   span(helpText("The “Average Fare” graph presents the average fare from departure city to other arrival cities with the fare listed on X-axis and each destination cities listed on Y-axis. Users could find the average price for each possible arrival cities ordered by the highest fare to the lowest fare. "))
                                   )
                                 ),
                        tabPanel("Passengers per day",
                                 fluidRow(
                                   
                                   plotOutput("stat_depart_city_out2", height = "800px"),
                                   downloadButton('download_formula_stat_depart_city2', 'Download the Plot'),
                                   span(helpText("Note: If the total number of city we can disply is less than 10 city, we only display the maximum number of city we have")),
                                   span(helpText("The “ Passenger Per Day” graph shows the average passenger numbers from departure city to other arrival cities with the average passenger numbers listed on X-axis and each destination cities listed on Y-axis. Users could find the average passengers for each possible arrival cities ordered by the route that has the most number of travelers to the route that has the least number of passengers."))
                                   )
                                 ),
                        tabPanel("Correlation",
                                 plotOutput("stat_depart_city_out34"),
                                 downloadButton('download_formula_stat_depart_city34', 'Download the Plot'),
                                 span(helpText("We conducted linear regression analysis on the relationship between travel miles and flight fare as well as the relationship between passenger numbers and flight fare respectively on each departure city. We would also label the correlation coefficient on both 'The Relationship Between Overall Avarage Fare and City Distance' graph and 'The Relationship Between Overall Avarage Fare and Passengers Per Day' graph."))
                                 ),
                        tabPanel("Carrier Market Share",
                                 plotOutput("formula_summary_out_put"),
                                 downloadButton('download_formula_summary', 'Download the Plot'),
                                 span(helpText("The “Carrier Market Share” graph indicates the total numbers of time when each flight company being the market share leader and the lowest fare provider for selected flight route. This graph is indentical to the graph in 'Market Share Carrier' of the 'Arrival Analysis'. Regarding each flight carrier abbreviate, 9K represents the Cape Air, AA represents the American Airlines, AS represents the Alaska Airlines, B6 represents the JetBlue Airways, Co represents the Continental Airline, DL represents the Delta Airlines, F9 represents Frontier Airlines, FL represents the AirTran Airways, G4 represents the Allegiant Airline, NK represents the Spirit Airlines, SY represents the Sun Country Airlines, U5 presents the USA 3000 Airlines, UA represents the United Airlines, US represents the US Airways, VX represents the Virgin America and WN represents the Southwest Airlines."))
                                 )
                        ),
             navbarMenu("Departure Visualization",
             tabPanel("Complete visualization",
                                 fluidRow(
                                   #column(10,div(style = "height:400%;width:200%;background-color: yellow;", plotOutput("depart_city_out")))
                                   plotOutput("depart_city_out",height = "700px"),
                                   downloadButton('downloadPlot_map_depart', 'Download the Plot'),
                                   span(helpText("In the section of departure visualization, the app generates a map graph that lay out the route(s) with various price ranges in a different color(s) from selected departure city to arrival cities. For each given departure city as the starting point, the arrow in this graph represents the route from selected departure city to arrival cities and each distinctive color matches particular average airfare range. For the first map graph, users could see the original map graph that labels all the flight routes from selected departure city to various arrival cities and identify the defined price range as we labeled in different colors."))
                                 )
              ),
             tabPanel("Fare-filtered visualization",
                      fluidRow(
                        selectInput(inputId = "price",h5("Select Price Range"),
                                    choices=list("Fare <$150"= 1,"Fare $150~200"= 2, "Fare $200~250"= 3,
                                                 "Fare $250~300"= 4,"Fare $300~350"=5,"Fare $350 and up"=6), selected =3),
                        plotOutput("depart_city_out_price",height = "700px"),
                        downloadButton('downloadPlot_map_depart_filter', 'Download the Plot'),
                        span(helpText("Based on the first graph, users could explicitly select one price range they are particularly interested in. The second graph would filter the user-selected price range from departure city to some arrival cities that may fall into that particular price range."))
                      )
             )
             ),
            
             navbarMenu("Arrival Analysis",
                        tabPanel("Average Fare",
                                 
                                 fluidRow(
                                   plotOutput('stat_arrival_city_out1', height = "800px"),
                                   downloadButton('download_formula_stat_arrival_city1', 'Download the Plot'),
                                   span(helpText("Note: If the total number of city we can disply is less than 10 city, we only display the maximum number of city we have")),
                                   span(helpText("The “Average Fare” graph presents the average fare of arrival city from other departure cities with the fare listed on X-axis and each departure cities listed on Y-axis. Users could find the average price for each possible departure cities ordered by the highest fare to the lowest fare."))
                                 )
                        ),
                        tabPanel("Passengers per day",
                                 
                                 fluidRow(
                                   plotOutput("stat_arrival_city_out2", height = "800px"),
                                   downloadButton('download_formula_stat_arrival_city2', 'Download the Plot'),
                                   span(helpText("Note: If the total number of city we can disply is less than 10 city, we only display the maximum number of city we have")),
                                   span(helpText("The “ Passenger Per Day” graph shows the average passenger numbers of arrival city from other departure cities with the average passenger numbers listed on X-axis and each departure cities listed on Y-axis."))
                                 )
                        ),
                        tabPanel("Correlation",
                                 plotOutput("stat_arrival_city_out34"),
                                 downloadButton('download_formula_stat_arrival_city34', 'Download the Plot'),
                                 span(helpText("We have conducted linear regression analysis on the relationship between travel miles and flight fare as well as the relationship between passenger numbers and flight fare respectively on each selected arrival city. We would also label the correlation coefficient on both 'The Relationship Between Overall Avarage Fare and City Distance' graph and 'The Relationship Between Overall Avarage Fare and Passengers Per Day' graph."))
                        ),
                        tabPanel("Carrier Market Share",
                                 plotOutput("formula_summary_arrival_out_put"),
                                 downloadButton('downloadformula_summary_arrival', 'Download the Plot'),
                                 span(helpText("The “Carrier Market Share” graph indicates the total numbers of the time for each flight company being the market share leader and the lowest fare provider. This graph is indentical to the graph in 'Market Share Carrier' of the 'Departure Analysis'. Regarding each flight carrier abbreviate, 9K represents the Cape Air, AA represents the American Airlines, AS represents the Alaska Airlines, B6 represents the JetBlue Airways, Co represents the Continental Airline, DL represents the Delta Airlines, F9 represents Frontier Airlines, FL represents the AirTran Airways, G4 represents the Allegiant Airline, NK represents the Spirit Airlines, SY represents the Sun Country Airlines, U5 presents the USA 3000 Airlines, UA represents the United Airlines, US represents the US Airways, VX represents the Virgin America and WN represents the South West Airlines."))
                        )
             ),
             navbarMenu("Arrival Visualization",
                        tabPanel("Complete visualization",
                                 fluidRow(
                                   #column(10,div(style = "height:400%;width:200%;background-color: yellow;", plotOutput("depart_city_out")))
                                   plotOutput("arrival_city_out",height = "700px"),
                                   downloadButton('downloadPlot_map_arr', 'Download the Plot'),
                                   span(helpText("In the section of arrival visualization, the app generates the map graph that lay out the route(s) with the various price ranges in different color(s) of selected arrival city from departure cities. For each given arrival city as the starting point, the arrow represents the route of selected arrival city from departure cities and each distinctive color matches particular average airfare range. For the first map graph, users could see the original map graph that labels all the flight routes of selected arrival city from various departure cities and identify the defined price range as we labeled in different colors."))
                                 )
                                 
                        ),
                        tabPanel("Fare-filtered visualization",
                                 fluidRow(
                                   selectInput(inputId = "price1",h5("Select Price Range"),
                                               choices=list("Fare <$150"= 1,"Fare $150~200"= 2, "Fare $200~250"= 3,
                                                            "Fare $250~300"= 4,"Fare $300~350"=5,"Fare $350 and up"=6), selected =3),
                                   plotOutput("arrival_city_out_price",height = "700px"),
                                   downloadButton('downloadPlot_map_arr_filter', 'Download the Plot'),
                                   span(helpText("Based on the first graph, users could explicitly select one price range they are particularly interested in. The map would filter the user-selected price range of arrival city from some arrival cities that may fall into that particular price range."))
                                 )
                        )
             ),
             navbarMenu("Fare Analysis", 
             tabPanel("All Quarters in Selected Year", 
                      fluidRow(
                        list(
                          column(8,plotOutput("q_time_trend_given_year")),
                          downloadButton('downloadPlot', 'Download the Plot')
                        )
                      ),
                      fluidRow(
                        list(
                          span(h3("The carrier names of the largest carrier and lowest carrier")),
                          tableOutput("q_time_trend_given_year2"),
                          downloadButton('downloadtable1', 'Download Above Table'),
                          span(h3("The baggage information of largest carrier and lowest carrier")),
                          tableOutput("q_time_trend_given_year3"),
                          downloadButton('downloadtable2', 'Download Above Table'),
                          span("The “Quarterly Time Trend Analysis in a Given Year” graph specifies three fare types - the average fare for all carriers, the average fare for the largest carrier as the market share leader and the average fare for the as the cheapest price provider quarterly in a given year. Along with the graph 1, the app generates a table corresponding to the results. Table 1 lists all the carriers ordered by the largest as the market share leader to the lowest as the cheapest price provider quarterly in a given year. The baggage information for each unique carrier in the previous table is presented in a separate table accordingly.")
                          )
                      )
             ),
             tabPanel("Selected Quarter 2011-2015", 
                      fluidRow(
                        list(
                          column(8,plotOutput("qtime_across_out")),
                          downloadButton('downloadPlot11', 'Download the Plot')
                        )
                      ),
                      fluidRow(
                        list(
                          span(h3("The carrier names of the largest carrier and lowest carrier")),
                          tableOutput("qtime_across_out2"),
                          downloadButton('downloadtable11', 'Download Above Table'),
                          span(h3("The baggage information of largest carrier and lowest carrier")),
                          tableOutput("qtime_across_out3"),
                          downloadButton('downloadtable22', 'Download Above Table'),
                          span("The “Time Trend Analysis in Selected Quarter Across Years” specifies three fare types - the average fare for all carriers, the average fare for the largest carrier as the market share leader and the average fare for the lowest carrier as the cheapest price provider in a selected quarter through the year 2011 to 2015. Along with the graph 2, the app generates a table corresponding to the results. Table 2 lists all the carriers ordered by the largest as the market share leader to the lowest as the cheapest price provider in a selected quarter through the year 2011 to 2015. The baggage information for each unique carrier in the previous table is presented in a separate table accordingly.")
                        )
                      )
             )
             ),
             tabPanel("Reference",
                      wellPanel(id = "ttPanel",style = "overflow-y:scroll; max-height: 600px; width: 100%;background-color: #F5F5F5",
                      includeMarkdown("Data Preparation.Rmd")
                      )
             )
              
  
  )
  )
)