

ui <- navbarPage("LITTLE GAMES",
                 tabPanel("GOMOKU",
                 navlistPanel(
    tabPanel(title = "BATTLE",
             sidebarLayout(
               sidebarPanel(useShinyjs(),
                            div(id = "battle_page",
                 sliderInput(inputId = "battle_num", 
                             label = "choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 actionButton("click_battle", "Play")
               ),
               tags$hr(),
               actionButton("reset_battle", "Reset")
               ),
               
               mainPanel(plotOutput("battle", click = "battle_click"),
                            textOutput("battle_result"))
             ) ),
    tabPanel(title = "COMPUTER",
             sidebarLayout(
               sidebarPanel(useShinyjs(),
                            div(id = "computer_page",
                 sliderInput(inputId = "computer_num", 
                             label = "Choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 radioButtons("color", "Choose Youe Color", choices = c("BLACK", "WHITE"), selected = NULL),
                 radioButtons("level","Choose Level", choices = c("EASY", "HARD"), selected = NULL),
                 actionButton("click_computer","Play")
               ), 
               tags$hr(),
               actionButton("reset_computer", "Reset")
               
               ),
               mainPanel( plotOutput("computer", click = "computer_click"),
                             textOutput("computer_result"))
             )
            
             
             
    ),
    tabPanel(title = "RECORD",
             sidebarLayout(
               sidebarPanel(
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
             )
  )),
  tabPanel(title = "R snake"),#R snake,
  tabPanel(title = "Mine Sweeper"),#mine sweeper
  tabPanel(title = "R flag")#R flag
)


server <- function(input, output) {
  source("R/gomoku_packages.R")
  source("R/gomoku_backstage_functions.R")
  source("R/app_battle.R")
  source("R/app_black.R")
  source("R/gomoku_main_function.R")
  source("R/gomoku_plot.R")
  source("R/gomoku_harder.R")
  source("R/app_white.R")
  
  data = reactive(input$color)
  
  gomoku_battle(points, input, output)
  
  gomoku_black(points, input, output)
  
  gomoku_white(points, input, output)
  
  observeEvent(input$reset_battle,{
    reset("battle_page")
    output$battle = renderPlot({plot_cover()})
  })
  
  observeEvent(input$reset_computer,{
    reset("computer_page")
    output$computer = renderPlot({plot_cover()})
  })
            


}

shinyApp(ui, server)

