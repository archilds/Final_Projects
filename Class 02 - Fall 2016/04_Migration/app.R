# set libraries 
# library(shiny)
# library(shinythemes)
# library(ggplot2)
# library(dplyr)
# library(maptools)
# library(rgdal)
# library(ggmap)
# library(Cairo)
# library(GGally)
# library(survival)
# library(plotly)
# library(gganimate)

source("check_packages.R")
check_packages(c("shiny","shinythemes","ggplot2","dplyr", "maptools", "rgdal", "ggmap", "Cairo", "GGally", "survival", "plotly")) 


################################################################################
# PREPARE THE INPUT DATA
################################################################################
# 0.ABOUT page: insert the animated .gif # 

# 1. EXPLORE page: use person-level data set # 
#person level data set (needs some cleaning)
samhs.personleveltmp <- read.csv("/Users/rebeccawang/Google Drive/Year 3/Sem1/stat computing/wang_final_shinyapp/perslevel.csv", stringsAsFactors = FALSE)

samhs.personlevel<- samhs.personleveltmp %>% 
  mutate(sex     = ifelse(male==1, "Male", "Female"), 
         movetype= ifelse(downmove1==1, "1", 
                   ifelse(neutralmove1==1, "2", "3")), 
         jobbeformove1 = ifelse(startjob1==1, "upper white collar", 
                          ifelse(startjob2==1, "lower white collar", 
                          ifelse(startjob3==1, "semi-skilled", 
                          ifelse(startjob4==1, "unskilled", "unemployed")))),
         jobaftermove1 = ifelse(jobafter1==0, "upper white collar", 
                         ifelse(jobafter1==1, "lower white collar", 
                         ifelse(jobafter1==2, "semi-skilled", 
                         ifelse(jobafter1==3, "unskilled", "unemployed")))))

samhs.personlevel<- samhs.personlevel %>% select( male, 
                                                     ageatsurvey,  
                                                     agewhenmoved1,
                                                     birthplaceurban, 
                                                     edcat,
                                                     edu,
                                                     totmoves, 
                                                     begprov, 
                                                     jobbeformove1,
                                                     jobaftermove1) 
varnames <- names(samhs.personlevel)

# 2. ANALYSIS page: use person-year level data set #
samhs.persyr <- read.csv("/Users/rebeccawang/Google Drive/Year 3/Sem1/stat computing/wang_final_shinyapp/analysis_persyr.csv", stringsAsFactors = FALSE)

# 3. CENSUS page: #
censusdata <- read.csv("/Users/rebeccawang/Google Drive/Year 3/Sem1/stat computing/wang_final_shinyapp/stats_sa_mig_rates.csv", stringsAsFactors = FALSE)
censusdata <- censusdata %>% group_by(time) %>% mutate(tot=sum(Migrants, na.rm=TRUE))
censusdata <- censusdata %>% group_by(time, Destination) %>% mutate(tot=sum(Migrants, na.rm=TRUE))

provdata <- data.frame(Province = c("EC", "FS", "GP", "KZN", "LIM", "MP", "NC", "NW", "WC"), 
                            GDP = c(3651, 6213, 9681, 4767, 4259, 6251, 6688, 6677, 8694),
                       Population = c(6743800, 2824500, 11191700, 10645400, 
                                      5439600, 3617600, 1103900, 3200900, 5223900), 
                       Life_Expectancy = c(53, 50.7, 62.9, 54.4, 58.3, 56.9, 52.9, 56.6, 63.7))


################################################################################
# CODE THE USER INTERFACE
################################################################################
ui <- navbarPage(theme = shinytheme("sandstone"), 
                 title="Internal Migration in South Africa",

#-------------------------------------------------------------------------------
# 0. ABOUT THIS APP
#-------------------------------------------------------------------------------
tabPanel("About", fluidPage(
  fluidRow(h3("Introduction")),
  tags$hr(),
  fluidRow(column(4, 
                  h5("This app explores migration trends using the South Africa Migration and Health Survey, 
                      a nationally representative survey from 2000. South Africa has a high prevalence of internal 
                      labor migration, and many individuals move multiple times (often in a circular pattern, 
                     oscillating between the urban destination and rural home) over their life. 
                     Historically, migration patterns were tied to apartheid-era mobility restrictions, 
                    but migration remains a critical livelihood strategy for many households in 
                     contemporary periods as well. The figure to the right depicts one representative individual’s 
                    residence history between 1980-2000. We see the individual experienced multiple migration 
                    events both within and between two provinces in the southeast part of South Africa. Each number corresponds 
                    to a residency spell."),
                  br(),
                  h5("The following analysis takes an event history approach and treats migration as a repeat event. 
                    The data are transformed to the person-year level in order to conduct a discrete-time logistic 
                    regression, and the main results show how occupational mobility at the first migration event (whether 
                    you moved into a better, worse, or the same occupation) affects the likelihood of subsequent migration.")), 
     column(7, offset = 1, plotOutput("plot0", height="400px"))
  ), #close FluidRow
   br(),
   br(),
   br(),
  fluidRow(h3("Navigate the App")), #close FluidRow
      tags$hr(),
  fluidRow(   
      column(4, 
             h4("1. Explore the Data"),
             h5("Select any two variables and examine the bivariate relationship. Additionally, sub-sample the
                 data based on cutoffs for age, total number of migrations experienced, and educational 
                attainment and assess how the bivariate relationship changes.")),#close Column
      column(4, h4("2. Examine the Results"), 
                h5("Survival curves, disaggregated by age, can be examined interactively with different 
                   age cut-offs and restrictions on the number of lifeyears. The regression estimates are also 
                   presented as odds ratios. ")), #close column
      column(4, h4("3. Census data"), 
                h5("This page provides (more recent) inter-provincial migration trends using
                    census data from Statistics South Africa. Select a province to observe the
                    in-migration and out-migration trends from 2001-2016. Several socio-demographic
                    statistics about the province will also be displayed."))#close column
  ) #close FluidRow
) #close fluidPage 
), #close about panel 

#-------------------------------------------------------------------------------
# 1. EXPLORE DATA 
#-------------------------------------------------------------------------------
tabPanel("1. Explore data", 
   fluidRow(
      column(width=4,
             h3("Select 2 variables"),
             selectInput("in.compare.var1", "Variable 1", c(varnames), selected="NULL"),
             br(),
             selectInput("in.compare.var2", "Variable 2", c(varnames), selected="NULL")
             ), #close column
      column(width=7, offset=1,
             h3("Examine the bivariate relationship"),
             plotOutput("plot1.1.1")
             ) # close column
   ),#close fluidRow   
   br(),
   br(),  
   tags$hr(),
   fluidRow(
        column(width=11, h3("Subsample the data", h5("or, just leave at default settings for full sample"))),
        column(width=4, sliderInput("in.restrict.age", "Age",
                                        min=1, max=99, value=c(1,99))), 
        column(width=4, sliderInput("in.restrict.moves", "Number of Moves", 
                                       min=1, max=22, value=c(1,22))), 
        column(width=4, selectInput("in.restrict.ed", "Years of Education ", c(0:14)))
   ) #close fluidrow
), #close 1. explore data tab  

#-------------------------------------------------------------------------------
# 2. VISUALIZE RESULTS
#-------------------------------------------------------------------------------
tabPanel("2. Examine results",
   tabsetPanel( 
     
     tabPanel("Survival Curves", 
              plotOutput("plot2.1"),
              
              hr(),
              
              fluidRow(
                column(8,
                       h4("Create a subsample"),
                       fixedRow(
                         column(4, sliderInput("in.restrict.age2.2", "Age at migration event", min=18, max=60, value=c(18,60))),
                         column(4, sliderInput("in.restrict.lifeyears2.2", "Total Life Years in File", min=1, max=71, value=c(1,71))) #close column
                       ) #close outer column 
                )# close fixed row
              )#close fluidRow
     ),#close last tabpanel 
     
      tabPanel("Regression Estimates",
         fluidRow(
            column(4,h3("Instructions"), 
                     h4("Hover over each point to see the regression estimate (presented in odds-ratios). Points above
                         the red line indicate the individual is more likely to move again relative to the reference category, 
                         and points below indicate he/she is less likely to migrate again. For instance, relative to those 
                         who experience an upward job transition, migrants who experience a downard transition are 1.8 times more likely to make another move.")), #close column 
            column(4, plotlyOutput("table2.2.1")), #close column
            column(4, plotlyOutput("table2.2.2")) #close column
         ), #close fluidRow
         tags$hr(),
         fluidRow(
           column(4, h3("Conclusions"),
                     h4(tags$ul(tags$li("Migrants whose first move results in working in white collar or semi-skilled jobs are less likely to migrate again, relative to the unemployed"), 
                           br(),
                           tags$li("Those who experience downward or neutral occupational transition more likely to migrate again"), 
                           br(),
                           tags$li("The longer the residency stay, the less likely an individual is to move on"), 
                           br(),
                           tags$li("The higher the number of prior moves made, the more likely the migrant is to move again")))), #close column 
           column(4, plotlyOutput("table2.2.3")), #close column
           column(4, plotlyOutput("table2.2.4")) #close column
         ) #close fluidRow
     ) #close tabPanel 
   )#close tabsetPanel
), #close panel 2. visualize results

#-------------------------------------------------------------------------------
# 3. VIEW CENSUS DATA
#-------------------------------------------------------------------------------
tabPanel("3. Census Data", 
         fluidPage(
           fluidRow(
             column(4, selectInput("provInput", h4("Select a Starting Province"), sort(unique(censusdata$fromprov))), 
                    textOutput("text1"),
                    br(),
                    textOutput("text2"),
                    br(),
                    textOutput("text3")),
             column(4,  br(), br(), h5(tags$b("EC:"), "Eastern Cape"), 
                                   h5(tags$b("WC:"), "Western Cape"),
                                   h5(tags$b("GP:"), "Gauteng"),
                                   h5(tags$b("NW:"), "Northwest"),
                                   h5(tags$b("MP:"), "Mpumalanga"),
                                   h5(tags$b("KZN:"), "Kwazaulu-Natal"),
                                   h5(tags$b("LP:"),  "Limpopo"),
                                   h5(tags$b("FC:"),  "Free state")),#close column
             column(4, plotOutput("map3.1", height="350px", width="400px"))
           ), #close fluidRow
           br(),
           fluidRow(
             column(4, h3("Out-migrations ('01-'05)"), plotOutput("plot3.1.0106", height="400px")), 
             column(4, h3("Out-migrations ('06-'10)"), plotOutput("plot3.1.0611", height="400px")), 
             column(4, h3("Out-migrations ('11-'16)"), plotOutput("plot3.1.1116", height="400px"))
           ),# close fluidRow
           tags$hr(),
           fluidRow(
             column(4, h3("In-migrations ('01-'05)"), plotOutput("plot3.2.0106", height="400px")), 
             column(4, h3("In-migrations ('06-'10)"), plotOutput("plot3.2.0611", height="400px")), 
             column(4, h3("In-migrations ('11-'16)"), plotOutput("plot3.3.1116", height="400px"))
           )# close fluidRow 
         )#close fluidPage
   )# close panel 3. census
) # close ui 

################################################################################
# Create the outputs to show in the app 
################################################################################
server <- function(input, output) {
#-------------------------------------------------------------------------------
# 0. ABOUT THIS APP 
#------------------------------------------------------------------------------- 
output$plot0 <- renderImage({
    list(src = "/Users/rebeccawang/Google Drive/Year 3/Sem1/stat computing/wang_final_shinyapp/map1b.gif",
         alt = "some text")
  }, deleteFile = FALSE) 

#-------------------------------------------------------------------------------
# 1. EXPLORE DATA 
#------------------------------------------------------------------------------- 
#plot 1.1.1
output$plot1.1.1 <- renderPlot({
  samhs.personlevel.f <- samhs.personlevel %>% filter(
    ageatsurvey >= input$in.restrict.age[1], 
    ageatsurvey <= input$in.restrict.age[2], 
    totmoves >= input$in.restrict.moves[1],
    totmoves <= input$in.restrict.moves[2],
    edu >= input$in.restrict.ed)
   
   select1<- samhs.personlevel.f[ ,input$in.compare.var1]
   select2<- samhs.personlevel.f[ ,input$in.compare.var2]
   
   ggplot(samhs.personlevel.f, aes(x=select1, y=select2))+geom_point()
   
}) #close plot1.1.1
  
#-------------------------------------------------------------------------------
# 2. VISUALIZE RESULTS
#-------------------------------------------------------------------------------
#plot 2.1
output$plot2.1 <- renderPlot({
  samhs.persyr.f2.2 <- samhs.persyr %>% filter(
    agewhenmoved1 >= input$in.restrict.age2.2[1],
    agewhenmoved1 <= input$in.restrict.age2.2[2],
    totlifeyrs >= input$in.restrict.lifeyears2.2[1],
    totlifeyrs <= input$in.restrict.lifeyears2.2[2]) 
  
  kpmcurve <- survfit(Surv(duration, event)~male, data=samhs.persyr.f2.2)
  ggsurv(kpmcurve)
}) #close plot2.1

# Estimate coefficients 
mylogit <- glm(event ~ endjob1_1 + endjob1_2 + endjob1_3 + endjob1_4 + neutralmove1 + downmove1 + dspell3 + dspell4 + dspell5 + dspell6 + dspell7 +
                 dspell8more + dur2 + dur3 +dur4 + dur5_6 + dur7_8 + dur9_10 + dur11up + lagage + male + primary + hs + degree + 
                 mrg_event + mrg_status + provcape + provkzn + provgaufree + urbanplace, data = samhs.persyr, family = "binomial")
p_values <- summary.glm((mylogit))$coefficient[,4]
Estimate <- coef(mylogit)
OR <- exp(coef(mylogit))
Var <- names(coef(mylogit)) 
logitresults1 <- data.frame(Var, Estimate, p_values, OR)
logitresults1 <- logitresults1 %>% mutate(signif = ifelse(p_values<0.10, "Yes", "No" ))

output$table2.2.1 <- renderPlotly({
  occupation <- logitresults1 %>% filter(Var=="endjob1_1" | Var=="endjob1_2" | Var=="endjob1_3" | Var=="endjob1_4")
  occupation <- occupation %>% mutate(VarName = ifelse(Var %in% "endjob1_1", "Up-white collar",
                                                ifelse(Var %in% "endjob1_2", "Low-white collar",
                                                ifelse(Var %in% "endjob1_3", "Semi-skilled", "unskilled"))))
  
  ggplot(occupation, aes(x=VarName, y=OR, color=signif))+scale_y_continuous(limits=c(0,2), breaks=c(0, 1, 2), labels=c("0", "1", "2"))+
    geom_point(size=4.5)+scale_color_manual(values=c("Yes"="#76f5f7", "No"="#bac9e2"))+
    geom_hline(yintercept = 1, colour="#990000", linetype="dashed")+
    theme(legend.position="none")+xlab("Occupation (Ref = unemployed)")
})

output$table2.2.2 <- renderPlotly({
  transition <- logitresults1 %>% filter(Var=="neutralmove1" | Var=="downmove1")
  transition <- transition %>% mutate(VarName = ifelse(Var %in% "neutralmove1", "Neutral", "Downward"))
  ggplot(transition, aes(x=VarName, y=OR, color=signif))+scale_y_continuous(limits=c(0,2), breaks=c(0, 1, 2), labels=c("0", "1", "2"))+
    geom_point(size=4.5)+scale_color_manual(values=c("Yes"="#76f5f7", "No"="#bac9e2"))+
    geom_hline(yintercept = 1, colour="#990000", linetype="dashed")+
    theme(legend.position="none")+xlab("Job Transition (Ref = Upward)")
})

output$table2.2.3 <- renderPlotly({
  resspell <- logitresults1 %>% filter(Var=="dspell3" | Var=="dspell4" | Var=="dspell5" | Var=="dspell6") %>% mutate(VarName = ifelse(Var %in% "dspell3", "Move:3",ifelse(Var %in% "dspell4", "Move:4", ifelse(Var %in% "dspell5", "Move:5", "Move:6+"))))
  ggplot(resspell, aes(x=VarName, y=OR, color=signif))+scale_y_continuous(limits=c(0,4), breaks=c(0, 1, 2, 3, 4), labels=c("0", "1", "2", "3", "4"))+
    geom_point(size=4.5)+scale_color_manual(values=c("Yes"="#76f5f7", "No"="#bac9e2"))+
    geom_hline(yintercept = 1, colour="#990000", linetype="dashed")+
    theme(legend.position="none")+xlab("Move Number (Ref = 2nd Move)")
})

output$table2.2.4 <- renderPlotly({
  duration <- logitresults1 %>% filter(Var=="dur2" | Var=="dur3" | Var=="dur4" | Var=="dur5") %>% mutate(VarName= ifelse(Var %in% "dur2", "2yrs", ifelse(Var %in% "dur3", "3yrs", ifelse(Var %in% "dur4", "4yrs", "5yrs+"))))
  ggplot(duration, aes(x=VarName, y=OR, color=signif))+scale_y_continuous(limits=c(0,2), breaks=c(0, 1, 2), labels=c("0", "1", "2"))+
    geom_point(size=4.5)+scale_color_manual(values=c("Yes"="#76f5f7", "No"="#bac9e2"))+
    geom_hline(yintercept = 1, colour="#990000", linetype="dashed")+
    theme(legend.position="none")+xlab("Length of stay (Ref = 1 year)")
})

#-------------------------------------------------------------------------------
# 3. VIEW CENSUS DATA
#-------------------------------------------------------------------------------

output$text1 <- renderText({ 
  amount <- provdata %>% filter(provdata$Province==input$provInput) %>% select(GDP)
  paste("Per capita GDP ($USD):", amount$GDP) 
})

output$text2 <- renderText({ 
  pop <- provdata %>% filter(provdata$Province==input$provInput) %>% select(Population)
  paste("Total Population:", pop$Population) 
})

output$text3 <- renderText({
  life <- provdata %>% filter(provdata$Province==input$provInput) %>% select(Life_Expectancy)
  paste("Average Life Expectancy:", life$Life_Expectancy)
})

output$map3.1 <- renderPlot({
  sa.shp <- readShapeSpatial("/Users/rebeccawang/Google Drive/Year 3/Sem1/stat computing/wang_final_shinyapp/ZAF_adm1.shp")
  #fortify the spatial polygons data frame
  sa.shp.f <- fortify(sa.shp, region = "NAME_1")
  #Turn variables into factors will use them to map onto colors
  sa.shp.f$id <-as.factor(sa.shp.f$id)

  #build base-layer map with provinces
  mapbase <- ggplot() + geom_polygon(data = sa.shp.f, aes(x = long, y = lat, group = group, fill=id, label=id),
                                     color = "white", size = 0.15) + 
    scale_fill_manual(name=NULL, values=c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", 
                                          "#0570b0", "#045a8d", "#023858", "#4682B4"))+coord_map()+scale_y_continuous(limits = c(-36, -20))+scale_x_continuous(limits=c(15, 35))+
    theme(legend.position="none", axis.title.x = element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank())+
    annotate("text", x=29.99971, y=-26.07099, label="MP", size=6)+annotate("text", x=29.66671, y=-23.43579, label="LP", size=6)+
    annotate("text", x=30.00007, y=-29.01407, label="KZN", size=6)+annotate("text", x=28.50000, y=-25.74972, label="GP", size=6)+
    annotate("text", x=25.00011, y=-27.07099, label="NW", size=7)+annotate("text", x=26.00001, y=-28.93472, label="FS", size=7)+
    annotate("text", x=21.74278, y=-33.56250, label="WC", size=7)+annotate("text", x=27.08611, y=-32.06250, label="EC", size=7)+
    annotate("text", x=21.04278, y=-30.00000, label="NC", size=7)
  mapbase
}) #close map3.1 



#out-migration plot 01-06   
output$plot3.1.0106 <- renderPlot({
  filtered0106 <- censusdata %>% filter(fromprov ==input$provInput, timeperiod=="2001-2006")
  maxval0106 <- max(filtered0106$Migrants, na.rm=TRUE)
  
  sa.bargraph.out0106 <- ggplot(data=filtered0106, aes(x=Destination, y=Migrants)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black", fill="#e5f5f9")+ 
    ylim(0, maxval0106)+ coord_flip() + xlab("Number of Migrants")+
    theme(legend.position="none", axis.title.x = element_text(face="bold", colour="#990000", size=20), 
          axis.title.y = element_blank(), axis.text.y=element_text(size=18))
  
  sa.bargraph.out0106
}) #close plot3.1.0106

#out-migration plot 0611 
output$plot3.1.0611 <- renderPlot({
  filtered0611 <- censusdata %>% filter(fromprov ==input$provInput, timeperiod=="2006-2011")
  maxval0611 <- max(filtered0611$Migrants, na.rm=TRUE)
  sa.bargraph.out0611 <- ggplot(data=filtered0611, aes(x=Destination, y=Migrants)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black", fill="#99d8c9")+ ylim(0, maxval0611)+ coord_flip() +
    theme(legend.position="none", axis.title.x = element_text(face="bold", colour="#990000", size=20), 
          axis.title.y = element_blank(), axis.text.y=element_text(size=18))

  sa.bargraph.out0611
}) #close plot3.1.0611

#out-migration plot 1116 
output$plot3.1.1116 <- renderPlot({
  filtered1116 <- censusdata %>% filter(fromprov ==input$provInput, timeperiod=="2011-2016")
  maxval1116 <- max(filtered1116$Migrants, na.rm=TRUE)
  
  sa.bargraph.out1116 <- ggplot(data=filtered1116, aes(x=Destination, y=Migrants)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black", fill="#2ca25f")+ 
    ylim(0, maxval1116)+coord_flip() +
    theme(legend.position="none", axis.title.x = element_text(face="bold", colour="#990000", size=20), 
          axis.title.y = element_blank(), axis.text.y=element_text(size=18))
  sa.bargraph.out1116
}) #close plot3.1.0611

#in-migration plot 01-06   
output$plot3.2.0106 <- renderPlot({
  filtered.in.0106 <- censusdata %>% filter(Destination ==input$provInput, timeperiod=="2001-2006")
  maxval0106 <- max(filtered.in.0106$Migrants, na.rm=TRUE)
  
  sa.bargraph.in.0106 <- ggplot(data=filtered.in.0106, aes(x=fromprov, y=Migrants)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black", fill="#e5f5f9")+ 
    ylim(0, maxval0106)+ coord_flip() + xlab("Number of Migrants")+
    theme(legend.position="none", axis.title.x = element_text(face="bold", colour="#990000", size=20), 
          axis.title.y = element_blank(), axis.text.y=element_text(size=18))
  sa.bargraph.in.0106
}) #close plot3.1.0106

#out-migration plot 0611 
output$plot3.2.0611 <- renderPlot({
  filtered0611 <- censusdata %>% filter(Destination ==input$provInput, timeperiod=="2006-2011")
  maxval0611 <- max(filtered0611$Migrants, na.rm=TRUE)
  sa.bargraph.out0611 <- ggplot(data=filtered0611, aes(x=fromprov, y=Migrants)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black", fill="#99d8c9")+ ylim(0, maxval0611)+ coord_flip() +
    theme(legend.position="none", axis.title.x = element_text(face="bold", colour="#990000", size=20), 
          axis.title.y = element_blank(), axis.text.y=element_text(size=18))
  
  sa.bargraph.out0611
}) #close plot3.2.0611

#out-migration plot 1116 
output$plot3.3.1116 <- renderPlot({
  filtered1116 <- censusdata %>% filter(Destination ==input$provInput, timeperiod=="2011-2016")
  maxval1116 <- max(filtered1116$Migrants, na.rm=TRUE)
  
  sa.bargraph.out1116 <- ggplot(data=filtered1116, aes(x=fromprov, y=Migrants)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black", fill="#2ca25f")+ 
    ylim(0, maxval1116)+coord_flip() +
    theme(legend.position="none", axis.title.x = element_text(face="bold", colour="#990000", size=20), 
          axis.title.y = element_blank(), axis.text.y=element_text(size=18))
  sa.bargraph.out1116
}) #close plot3.3.0611

} #close server 

shinyApp(ui = ui, server = server)


