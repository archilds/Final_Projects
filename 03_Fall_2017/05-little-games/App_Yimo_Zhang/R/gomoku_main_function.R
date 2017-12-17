
gomoku = function(n = 19)
{
  click = 0
  result = ""
  
    #Choose whom to play wit
    stage0()

    click = first_choose()
    
    if(click == 1){result = gomoku_self(n)}
    
    
    if(click == 2){
      #Choose color
      stage1()
      click = second_choose()
    
      if(click == 3){
        stage2()
        click = third_choose()
        if(click == 5){result = gomoku_easy(choose = 1)}
        if(click == 6){result = gomoku_easy(choose = 2)}
      }
      
      if(click == 4){
        stage2()
        click = third_choose()
        if(click == 5){result = gomoku_hard(choose = 1)}
        if(click == 6){result = gomoku_hard(choose = 2)}
      }
    }
    
    
    if(result == "White Wins!"){img = readPNG("white_wins.png")}
    if(result == "Black Wins!"){img = readPNG("black_wins.png")}
    if(result == "You Win!"){img = readPNG("happy_face.png")}
    if(result == "You Lose!"){img = readPNG("sad_face.png")}
    
  gameover(result = result, img = img)
  repeat{
    options(locatorBell = FALSE)
    l=locator(1)
    x = l$x
    y = l$y
    if(x>40 & x<60 & y>8 & y<16){return("Game Closed")}
    if(x>32 & x<69 & y>23 & y<31){
      dev.off()
      gomoku(n)
    }
  }
}
  

first_choose = function(){
  repeat{
    options(locatorBell = FALSE)
    l = locator(1)
    x = l$x
    y = l$y
    if(x>37 & x<63 & y>23 & y<31){return(1)}
    if(x>30 & x<70 & y>8 & y<16){return(2)}
  }
}


third_choose = function(){
  repeat{
    options(locatorBell = FALSE)
    l = locator(1)
    x = l$x
    y = l$y
    if(x>40 & x<61 & y>23 & y<31){return(5)}
    if(x>39 & x<61 & y>8 & y<16){return(6)}
  }
}

second_choose = function(){
  repeat{
    options(locatorBell = FALSE)
    l = locator(1)
    x = l$x
    y = l$y
    if(x>42 & x<58 & y>23 & y<31){return(3)}
    if(x>40 & x<60 & y>8 & y<16){return(4)}
  }
}

