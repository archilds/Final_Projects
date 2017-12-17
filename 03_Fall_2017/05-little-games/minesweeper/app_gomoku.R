library(shiny)
ui <- navbarPage("GAME",
    tabPanel(title = "BATTLE",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "battle_num", 
                             label = "choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 actionButton("click_battle", "Play")
               ), mainPanel(plotOutput("battle"))
             ) ),
    tabPanel(title = "COMPUTER",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "computer_num", 
                             label = "Choose a number", 
                             value = 19, min = 5, max = 99, step = 2),
                 radioButtons("color", "Choose Youe Color", choices = c("BLACK", "WHITE"), selected = "BLACK"),
                 radioButtons("level","Choose Level", choices = c("EASY", "HARD"), selected = "EASY"),
                 actionButton("click_computer","Play")
               ), mainPanel( plotOutput("computer"))
             )
            
             
             
    ),
    tabPanel(title = "RECORD",
             sidebarLayout(
               sidebarPanel(
                 actionButton("refresh", "Refresh")
               ),mainPanel(tableOutput("record"))
             )
             ),
    tabPanel(title = "MINESWEEPER",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("WidthInput", "Width", 5, 50, 10),  #slider for width input
                 sliderInput("LengthInput", "Length", 5, 50, 10), #slider for length input
                 sliderInput("MinesInput", "Mines", 1, 100, 5), #slider for length input
                 actionButton("start", "Start")
                 #      selectInput("restartOption", "Restart", c("SELECT CHOICE","YES","NO"))
               ),
               mainPanel(plotOutput("mine")) #show the main game panel
             )
    )
)


server <- function(input, output) {
  source("/gomoku/gomoku_packages.R")
  source("/gomoku/gomoku_backstage_functions.R")
  source("/gomoku/gomoku_battle.R")
  source("/gomoku/gomoku_easy.R")
  source("/gomoku/gomoku_main_function.R")
  source("/gomoku/gomoku_plot.R")
  source("/gomoku/gomoku_harder.R")
  source("/gomoku/gomoku_hard.R")
  source("/minesweeper/mine_sweeper.R")
  
  observeEvent(input$click_battle, {
    output$battle = renderPlot({
      gomoku_battle(n = input$battle_num)
    })
  })
  
  observeEvent(input$click_computer, { output$computer = renderPlot({
    if(input$color == "BLACK"){
      if(input$level == "EASY"){gomoku_easy(n = input$computer_num, choose = 1)}
      if(input$level == "HARD"){gomoku_hard(n = input$computer_num, choose = 1)}
    }
    if(input$color == "WHITE"){
      if(input$level == "EASY"){gomoku_easy(n = input$computer_num, choose = 2)}
      if(input$level == "HARD"){gomoku_hard(n = input$computer_num, choose = 2)}
    }
  })
  })
  
  actionstart <- eventReactive(input$start, { #trigger of starting button
    output$mine = renderPlot({mine_sweeper(input$WidthInput, input$LengthInput, input$MinesInput)
  })}
  )
 
  
}

shinyApp(ui, server)