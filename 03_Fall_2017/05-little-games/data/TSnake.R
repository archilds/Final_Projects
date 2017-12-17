#' R Snake
#' 
#' @param Keyboards events
#' @return Snake moving
#' @export
Snake<-function( ){
  
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")
      
      library(name, character.only=TRUE)
    }
  }
  
  install_packages(c("ggplot2","Cairo","ggmap","grid","scales","png","jpeg")) 
  if (.Platform$OS.type == "windows") x11() 
  else x11(type = "Xlib")
init<-function( ){ ## Initialize the environment variable
  e<<-new.env()
  e$stage<-0 ## Scene
  e$width<-e$height<-20  ## Grid segmentation
  e$step<-1/e$width ## Distance moved per unit time
  e$m<-matrix(rep(0,e$width*e$height),nrow=e$width)  ## Dot-matrix
  e$dir<-e$lastd<-'up' ## Direction of movement
  e$head<-c(2,2) ## Start position
  e$lastx<-e$lasty<-2 ## Initialize the head of the snake
  e$tail<-data.frame(x=c(),y=c()) ## Initialize the tail of the snake
  
  e$col_furit<- runif(1,min=1,max=length(colors())) ## Fruit color
  e$col_head<- runif(1,min=1,max=length(colors()))  ## Snake head color
  e$col_tail<- runif(1,min=1,max=length(colors()))  ## Snake tail color
  e$col_path<- 0 ## path color
  
}

 
index<-function(col) which(e$m==col)  ## Get the index value of the matrix




stage1<-function( ){  ## Being in the game
  e$stage<-1


  furit<-function( ){ ## Randomized fruit position
    if(length(index(e$col_furit))<=0){ ## Nonexistent position
      idx<-sample(index(e$col_path),1)
      
      fx<-ifelse(idx%%e$width==0,10,idx%%e$width)
      fy<-ceiling(idx/e$height)
      e$m[fx,fy]<-e$col_furit
      
      print(paste("furit idx",idx))
      print(paste("furit axis:",fx,fy))
    }
  }
  

  fail<-function( ){    ## Check the failure
    ## Head out of ledge
    if(length(which(e$head<1))>0 | length(which(e$head>e$width))>0){
      print("game over: Out of ledge.")
      keydown('q')
      return(TRUE)
    }
    
    ## Head eats tail
    if(e$m[e$head[1],e$head[2]]==e$col_tail){
      print("game over: head eats tail")
      keydown('q')
      return(TRUE)
    }
    
    return(FALSE)
  }
  

  head<-function( ){    ## Snake head
    e$lastx<-e$head[1]
    e$lasty<-e$head[2]
    
    ## Direction operator
    if(e$dir=='up') e$head[2]<-e$head[2]+1
    if(e$dir=='down') e$head[2]<-e$head[2]-1
    if(e$dir=='left') e$head[1]<-e$head[1]-1
    if(e$dir=='right') e$head[1]<-e$head[1]+1
    
  }
  

  body<-function( ){    ## Snake body
    e$m[e$lastx,e$lasty]<-0
    e$m[e$head[1],e$head[2]]<-e$col_head ## Snake
    if(length(index(e$col_furit))<=0){ ## Nonexistent position
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
    }
    
    if(nrow(e$tail)>0) { ## If snake has tail
      e$tail<-rbind(e$tail,data.frame(x=e$lastx,y=e$lasty))
      e$m[e$tail[1,]$x,e$tail[1,]$y]<-e$col_path
      e$tail<-e$tail[-1,]
      e$m[e$lastx,e$lasty]<-e$col_tail
    }
    
    print(paste("snake idx",index(e$col_head)))
    print(paste("snake axis:",e$head[1],e$head[2]))
  }

  drawTable<-function( ){ ## The canvas background
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
    rect(-3, -3, 3, 3, col="cornsilk") ## Background color

  }
  

  drawMatrix<-function( ){     ## Fill in the data according to the matrix
    idx<-which(e$m>0)
    px<- (ifelse(idx%%e$width==0,e$width,idx%%e$width)-1)/e$width+e$step/2
    py<- (ceiling(idx/e$height)-1)/e$height+e$step/2
    pxy<-data.frame(x=px,y=py,col=e$m[idx])
    points(pxy$x,pxy$y,col=pxy$col,pch=15,cex=4.4)
  }
  
  furit()
  head()
  if(!fail()){
    body()
    drawTable()
    drawMatrix()  
  }
}


stage0<-function( ){ ## Startup screen
  e$stage<-0

  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  bg = readJPEG("data/BGB1.jpg")
  rasterImage(bg,0,0,1,1)
  text(0.5,0.7,label="Snake Game",cex=5,col="yellow")
  text(0.5,0.4,label="Any keyboard to start",cex=2,col="pink")
  text(0.5,0.3,label="Up,Down,Left,Right to control direction",cex=2,col="pink")
}


stage2<-function( ){  ## Get result
  e$stage<-2
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
  bgg = readJPEG("data/grass1.jpg")
  rasterImage(bgg,0,0,1,1)
  text(0.5,0.7,label="Congratulations!",cex=4,col="pink")
  text(0.5,0.6,label=paste("You got",nrow(e$tail),"fruits!"),cex=3,col="pink")
  text(0.5,0.4,label="'Space'' to restart, 'Q' to quit.",cex=2,col="yellow")
}



keydown<-function(K){ ## Keyboards events
  print(paste("keydown:",K,",stage:",e$stage));
  
  if(e$stage==0){ ## Startup screen
    init()
    stage1()
    return(NULL)
  }  
  
  if(e$stage==2){ ## Get result
    if(K=="q") q()
    else if(K==' ') stage0()  
    return(NULL)
  } 
  
  if(e$stage==1){ ## Being in the game
    if(K == "q") {
      stage2()
    } else {
      if(tolower(K) %in% c("up","down","left","right")){
        e$lastd<-e$dir
        e$dir<-tolower(K)
        stage1()  
      }
    }
  }
  return(NULL)
}


  
  par(mai=rep(0,4),oma=rep(0,4)) ## Set the global canvas without edge
  e<<-new.env() ## Encapsulate variables
  stage0() ## Startup screen
  
  getGraphicsEvent(prompt="Snake Game",onKeybd=keydown) ## Keyboards events registration
}

Snake()
