#play with computer(player is black)
gomoku_black = function(points, input, output) {
  
    #click play 
  observeEvent(input$click_computer,{

    
  if(input$color == "BLACK" ){
    
    points = matrix(rep(0, input$computer_num^2), nrow = input$computer_num, ncol = input$computer_num)

    output$computer = renderPlot({chessboard(input$computer_num, points)})

    
    
    #initialize variables
    i = 1
    j = 1
    player = list() 
    computer = list()
    playedlist = list()
    
    #play the game
    observeEvent(input$computer_click, {
      if(computer_start == 0){
      if(input$color == "BLACK"){
      
      
      point = adjust(input$computer_click, input$computer_num)

      #player plays
      if (!if_in(point = point, points = points)) #break when the point had chessman on it
      {
        points[point[1], point[2]] <<- 1
     
        
        xy = paste(point, collapse = ":")
        playedlist <<- c(playedlist, xy)
        
        output$computer = renderPlot({chessboard(input$computer_num, points)})
        
         player[[i]] <<- point
        
          if(if_win(player)==1){
            computer_start <<- 1
            output$computer = renderPlot({plot_computer_result("You Win!", "black")})
            r_table_computer <<- r_table_computer %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Computer", "Player Wins!"))
            output$computer_result = renderText("You Win!")
          }
        
         
        #Computer plays
        if(input$level == "HARD"){new = computer_play_hard(player, computer, playedlist, input$computer_num)}
        if(input$level == "EASY"){new = computer_play(player,computer, playedlist, input$computer_num)}
        point = unlist(new)
        points[point[1], point[2]] <<- 2
        xy <- paste(point, collapse = ":")
        playedlist <<- c(playedlist, xy)
        computer[[i]]<<- point
    
          if(if_win(computer)==1){
            computer_start <<- 1
            output$computer = renderPlot({plot_computer_result("You Lose!","black")})
            r_table_computer <<- r_table_computer %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Computer", "Computer Wins!"))
            output$computer_result = renderText("You Lose!")
          }
        
      #update variables
       i <<- i+1
        
        
      }
      }
      }
    })
  }
  })
  }


#make the plot(including chessboard and chessmen)
chessboard = function(n , points){
  img<-readJPEG("App_Yimo_Zhang/R/wood.jpg")
  par(mar = rep(0, 4)) 
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(img,0,0,1+n,1+n)
  segments(1, 1:n, n, 1:n)
  segments(1:n, 1, 1:n, n)
  temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
  points(rep(temp, 3), rep(temp, each = 3),
         pch = 19, cex = 6/sqrt(n))
  box()
  for(i in 1:n){
    for(k in 1:n){
      l = list()
      l$x = i
      l$y = k
      shape = points[i,k]
      if(shape > 0)
        points(l, cex = 3*19/n, pch = c(19, 21)[shape], bg = c("black", "white")[shape])
    }
  }
  
}

#adjust the click position
adjust = function(adjust_point, n){
  l = adjust_point
  x = min(n, max(1, round(l$x)))
  y = min(n, max(1, round(l$y)))
  return(c(x,y))
}

#check if a spot is available
if_in = function(point, points){
  x = point[1]
  y = point[2]
  if(points[x,y] < 1)
    return(0)
  else{return(1)}
}
