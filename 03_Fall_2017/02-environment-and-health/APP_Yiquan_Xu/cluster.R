# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
library(shiny)
#transform it into percentage.
aqi <- annual_aqi%>%mutate(Per_CO = `Days CO`/sum(`Days CO`), Per_NO2 = `Days NO2`/sum(`Days NO2`),
                           Per_Ozone = `Days Ozone`/sum(`Days Ozone`), Per_SO2 = `Days SO2`/sum(`Days SO2`))
ui <- fluidPage(
  headerPanel('k-means clustering'),
  tabsetPanel(
  tabPanel(title="Pollution",
    selectInput('xcol', 'X Variable', names(aqi)[20:23]),
    selectInput('ycol', 'Y Variable', names(aqi)[20:23],
                selected = names(aqi)[22]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9),
    plotOutput('pollution')
  ),
  tabPanel(title="Health",
           selectInput('x',"X Variable",names(annual_aqi)[5:10]),
           selectInput('y',"Y Variable",names(annual_aqi)[5:10],
                                              selected = names(annual_aqi)[6]),
           numericInput('clus', 'Cluster count', 3,
                        min = 1, max = 9),
           plotOutput('health')
           )
  )
)
server <- function(input, output) {
  selectedData <- reactive({
    aqi[, c(input$xcol, input$ycol)]
  })
  Data <- reactive(
    annual_aqi[,c(input$x,input$y)]
  )
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  clus <- reactive({
    kmeans(Data(), input$clus)
  })
  output$pollution <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  output$health <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(Data(),
         col = clus()$cluster,
         pch = 20, cex = 3)
    points(clus()$centers, pch = 4, cex = 4, lwd = 4)
  })
}

shinyApp(ui = ui, server = server)
