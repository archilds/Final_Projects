gomoku_battle <- function(n = 19) {

  dev.new()
  if (!interactive()) return() #check if R is running interactively; if not, quit the game
  if(n < 5) stop("Hmm, n is too small for the game to play!")
  if(n %% 2 < 1) stop("Sorry, n must be a odd number!")

#Setting of the game
  img<-readJPEG("wood.jpg")
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  rasterImage(img,0,0,1+n,1+n)
  segments(1, 1:n, n, 1:n)#draw horizontal lines
  segments(1:n, 1, 1:n, n)#draw vertical lines
  temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
  points(rep(temp, 3), rep(temp, each = 3),
         pch = 19, cex = 6/sqrt(n))#draw the black point with the shape of solid circle
  box() #draw the outline of the plot
  
  
#Playing the game
  playedlist <- NULL #record the points have been stepped on
  i <- 1 #rounds that will have be played
  black = list() #record the black chessman
  white = list() #record the while chessman
  repeat {
    for (j in 1:2) {
      repeat {
        options(locatorBell = FALSE)
        l = locator(1)
        l$x <- min(n, max(1, round(l$x))) #modify the x-location to where nearest point
        l$y <- min(n, max(1, round(l$y))) #modify the y-location to where nearest point
        xy <- paste(l, collapse = ":") #record the step
        if (!is.element(xy, playedlist)) #break when the point had chessman on it
          break
      }
      playedlist <- c(playedlist, xy) #add the step to the playlist if it is successfully played
      points(l, cex = 3*19/n, pch = c(19, 21)[j], bg = c("black", "white")[j]) #draw the step (black first)
                                                                          #black as solid circle, while as filled circle
      #check the black chessmen
      if(j == 1){
        black[[i]] = c(l$x, l$y)#update the black chessmen set
        if(if_win(black)){
          return("Black Wins!")
        }
      }

      #check the white chessman
      if(j == 2){
        white[[i]] = c(l$x, l$y)#update the white chessmen set
        if(if_win(white)==1){
          return("White Wins!")
        }
      }
      
      if (2*(i) >= n^2) break #break when the chessboard has been filled
    }
    i = i+1 #enter the next round
    if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
  }
  dev.off()
}





