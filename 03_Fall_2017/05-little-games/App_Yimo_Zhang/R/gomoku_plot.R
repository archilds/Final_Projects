
stage0 = function(){
  dev.new()
  n = 100
  x=c(1:n)
  y=c(1:n)
  taiji = readPNG("taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("bg.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  bg = readJPEG("wood.jpg")
  rasterImage(bg,0,0,1+n,1+n)
    text(x = seq(32,68, length.out = 6), y=rep(8*n/9,6), col = colfunc(6),
             cex = 3.5, label = unlist(strsplit("GOMOKU", NULL)), family = "JP1", lwd = 2.5)
    text(x = 50, y = 28, col = "black", label = "BATTLE", family = "JP1", cex =2.5, lwd = 2.5)
    text(x = 50, y = 13, col = "white", family = "JP1", cex = 2.5, label = "COMPUTER", lwd = 2.5)
    rasterImage(taiji,30, 35, 70, 75)
    

}


stage2 = function(){
  taiji = readPNG("taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"))
          
  bg = readJPEG("bg.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  colfunc1 = colorRampPalette(c("black","gray90"))
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  bg = readJPEG("wood.jpg")
  rasterImage(bg,0,0,1+n,1+n)
    text(x = seq(25,75, length.out = 8), y=rep(8*n/9,8), col = colfunc(8),
             cex = 3.5, label = unlist(strsplit("COMPUTER", NULL)), family = "JP1", lwd = 2.5)
    text(x = 50, y = 28, label = "BLACK", col = "black", family = "JP1", cex = 2.5, lwd = 2.5)
    text(x = 50, y = 13, label = "WHITE", col = "white", cex=2.5, family = "JP1", lwd = 2.5)
    rasterImage(taiji,30, 35, 70, 75)

}

stage1 = function(){
  taiji = readPNG("taiji.png")
  windowsFonts(JP1 = windowsFont("Pristina"))
  
  bg = readJPEG("bg.jpg")
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  colfunc1 = colorRampPalette(c("black","gray90"))
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  bg = readJPEG("wood.jpg")
  rasterImage(bg,0,0,1+n,1+n)
  text(x = seq(25,75, length.out = 10), y=rep(8*n/9,10), col = colfunc(10),
       cex = 3.5, label = unlist(strsplit("DIFFICULTY", NULL)), family = "JP1", lwd = 2.5)
  text(x = 50, y = 28, label = "EASY", col = "black", family = "JP1", cex = 2.5, lwd = 2.5)
  text(x = 50, y = 13, label = "HARD", col = "white", cex=2.5, family = "JP1", lwd = 2.5)
  rasterImage(taiji,30, 35, 70, 75)

  
  
}

gameover = function(result, img){
  colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
  colfunc1 = colorRampPalette(c("black","gray90"))
  windowsFonts(JP1 = windowsFont("Pristina"))
  bg = readJPEG("wood.jpg")
  n = 100
  x=c(1:n)
  y=c(1:n)
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  rasterImage(bg,0,0,1+n,1+n)
  text(x = 50, y = 28, label = "PLAY AGAIN", col = "black", family = "JP1", cex = 2.5, lwd = 2.5)
  text(x = 50, y = 13, label = "QUIT", col = "white", cex = 2.5, family = "JP1", lwd = 2.5)
  rasterImage(img, 30, 35, 70, 75)
  
  if(result == "White Wins!"){text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = "white", family = "JP1")
  }
  if(result == "Black Wins!"){text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = "black", family = "JP1")
  }
  if(result == "You Win!"){
    result = toupper(result)
    len = str_count(result)
    text(x = seq(25,75, length.out = len), y = rep(8*n/9,len), label = unlist(strsplit(result, NULL)), cex = 3.5, col = colfunc(len), family = "JP1", lwd = 5)
  }
  if(result == "You Lose!"){
    result = toupper(result)
    len = str_count(result)
    text(x = seq(25,75, length.out = len), y = rep(8*n/9,len), label = unlist(strsplit(result, NULL)), cex = 3.5, col = colfunc1(len), family = "JP1", lwd = 2.5)
  }
}  
  
               
  

  
  
    
 







