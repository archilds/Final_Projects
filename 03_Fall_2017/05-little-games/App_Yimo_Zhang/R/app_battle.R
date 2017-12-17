#play with another person
gomoku_battle = function(points, input, output) {
 
  observeEvent(input$click_battle, {
    
    
    points = matrix(rep(0, input$battle_num^2), nrow = input$battle_num, ncol = input$battle_num)
    
    output$battle = renderPlot({chessboard(input$battle_num, points)})
    
   
    i = 1
    j = 1
    black = list() 
    white = list() 
    
    
    observeEvent(input$battle_click, {
      
      if(battle_start == 0)
      {
      point = adjust(input$battle_click, input$battle_num)
      
      
      if (!if_in(point = point, points = points)) #break when the point had chessman on it
      {
        points[point[1], point[2]] <<- j
  
        
        
        
        output$battle = renderPlot({chessboard(input$battle_num, points)})
        
        
        if(j == 1){
          black[[i]] <<- point
   
          if(if_win(black)==1){
            battle_start <<- 1
            output$battle = renderPlot({plot_battle_result("Black Wins!")})
            r_table_battle <<- r_table_battle %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Battle", "Black Wins!"))
            output$battle_result = renderText("Black Wins!")
          }
        }
        
        #check the white chessman
        if(j == 2){
          white[[i]] <<- point
          if(if_win(white)==1){
            battle_start <<- 1
            output$battle = renderPlot({plot_battle_result("White Wins!")})
            r_table_battle <<- r_table_battle %>%
              rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), "Battle", "White Wins!"))
            output$battle_result = renderText("White Wins!")
          }
        }
      
        if(j == 1){j <<- 2}
        else{
          j <<- 1
          i <<- i + 1}
     
        
      }
      }
    })
  })
  }


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


plot_cover = function(){
  n = 100
  x=c(1:n)
  y=c(1:n)
  taiji = readPNG("App_Yimo_Zhang/R/taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("App_Yimo_Zhang/R/wood.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  rasterImage(bg,0,0,1+n,1+n)
  text(x = seq(32,68, length.out = 6), y=rep(8*n/9,6), col = colfunc(6),
       cex = 3.5, label = unlist(strsplit("GOMOKU", NULL)), family = "JP1", lwd = 2.5)
  
  rasterImage(taiji,30, 15, 70, 75)
}



plot_battle_result = function(result)
{
  if(result == "White Wins!"){
    img = readPNG("App_Yimo_Zhang/R/white_wins.png")
    color = "white"
    }
  if(result == "Black Wins!"){
    img = readPNG("App_Yimo_Zhang/R/black_wins.png")
    color = "black"
    }
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("App_Yimo_Zhang/R/wood.jpg")
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  rasterImage(bg,0,0,1+n,1+n)
  text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = color, family = "JP1")
  rasterImage(img, 30, 15, 70, 75)
}
