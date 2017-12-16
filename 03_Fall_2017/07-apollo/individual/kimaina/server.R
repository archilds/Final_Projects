library(shiny)
library(ggplot2)


# shiny server definition
shinyServer(function(input, output) {
  
  # First compute the p-value given the test statistic and the type of test
  # Then output it as text.
  
  output$pValue <- renderText({ 
   
    if (input$testType=="left") {
      # case when it is a left tailed
      pVal <- pnorm(input$zValue,lower.tail=TRUE)
    } else if (input$testType=="right") {
      # case when it is a right tailed
        pVal <- pnorm(input$zValue,lower.tail=FALSE)
      
    } else {
      # case when it is 2 tailed
        pVal <- 2*pnorm(abs(input$zValue),lower.tail=FALSE)  #two-tailed
      }
    # now output the value
    paste('P-value: ',round(pVal,digits=4))
  })
  
  
  
  output$stdDistPlot <- renderPlot({
    
    # plot the standard norm distribution
    plot <- ggplot(data.frame(x=c(-6,6)),aes(x=x)) + # Creates plot from x = -10 to 10
      stat_function(fun=dnorm ,  alpha = 0.4) +
      xlab("z") +
      ylab("PDF")+ 
      annotate("segment",color="blue",x=input$zValue,xend=input$zValue,y=0,yend=dnorm(input$zValue))
    if (input$testType=="both") {
      # case when 2 tailed
      # add left tail
      plot<- plot + stat_function(fun=shade_tail(dnorm,abs(input$zValue),"right"),geom="area",fill="blue",alpha=0.2)
      # add right tail
      plot<-plot + stat_function(fun=shade_tail(dnorm,-abs(input$zValue),"left"),geom="area",fill="blue",alpha=0.2)
      # add a segment line
      plot <- plot + annotate("segment",color="blue",x=-input$zValue,xend=-input$zValue,y=0,yend=dnorm(input$zValue))  
      # add z value (righgt)
      plot<-plot + annotate("text",x=abs(input$zValue),y=-.01,label=paste("z* =",round(abs(input$zValue),2)))
      # add z value (left)
      plot<-plot + annotate("text",x=-abs(input$zValue),y=-.01,label=paste("z* =",round(-abs(input$zValue),2)))
    }
    else {
      # case when either right or left tailed
      plot<-plot + stat_function(fun=dnorm) + stat_function(fun=shade_tail(dnorm,input$zValue,input$testType),geom="area",fill="blue",alpha=0.2)
      # plot text z value
      plot<-plot + annotate("text",x=input$zValue,y=-.01,label=paste("z* =",round(input$zValue,2)))
    }
    # Makes the background white theme
    plot<-plot+theme_classic()
    # aesthetics
    plot<-plot+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())    # Removes y axis information
   # return plot
     return(plot)
    }) 
})


#function for ggplot that shades the tails
shade_tail <-function (fx,limit,tail) {
  function (x) {
    # for selecting the areas for shading under the distribution of interest as defined by fun
    y <- fx(x)
    if (tail=="right") y[x < limit] <- NA 
    else y[x>limit] <- NA
    return(y)
  }
}