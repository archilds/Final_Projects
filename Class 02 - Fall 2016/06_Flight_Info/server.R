library(shiny)
library(shinythemes)
#library("ggmap")
#library(maptools)
#library(maps)
#library(dplyr)
#library(stringr)
#library(ggplot2)
#library(gridExtra)
source("check_packages.R")
check_packages(c("ggmap","maptools","dplyr","stringr", "maps", "ggplot2", "gridExtra", "pander", "reshape2","shinythemes"))

#load all required data
data2015<-read.csv('2015_table6_withlon.csv',header=TRUE,stringsAsFactors=FALSE)
data2014<-read.csv('2014_table6_withlon.csv',header=TRUE,stringsAsFactors=FALSE)
data2013<-read.csv('2013_table6_withlon.csv',header=TRUE,stringsAsFactors=FALSE)
data2012<-read.csv('2012_table6_withlon.csv',header=TRUE,stringsAsFactors=FALSE)
data2011<-read.csv('2011_table6_withlon.csv',header=TRUE,stringsAsFactors=FALSE)
baggage<-read.csv('useful_baggage.csv',header=TRUE,stringsAsFactors=FALSE)

#=================================================================================================================================
#                                               Basic Function By Yifu Liu
#=================================================================================================================================
#function to get year data for selected year
get_year_data<-function(year){
    ydata<-c()
    for(j in 2011:2015){
        if(year==j){
            filename<-paste0(j,"_table6_withlon.csv")
            ydata<-read.csv(filename,header=TRUE,stringsAsFactors=FALSE)
            
        }
    }
    return(ydata)#return the year data for us
}
#=================================================================================================================================
#function to get quarter data for selected year
get_quart_data<-function(year,quart){
    qdata<-c()
    for(j in 2011:2015){
        if(year==j){
            filename<-paste0(j,"_table6_withlon.csv")
            ydata<-read.csv(filename,header=TRUE,stringsAsFactors=FALSE)
            for(i in 1:4){
                if(quart==i){
                    qdata<-ydata[ydata$quarter==i,]
                }
            }
        }
    }
    return(qdata)#return the quarter data for us
}
#=================================================================================================================================
#function to identify all departure city selections after year is selected
city_sel_after_yrs<-function(year){
    city1_list<-c()
    for(j in 2011:2015){
        if(year==j){
            filename<-paste0(j,"_table6_withlon.csv")
            #print(filename)
            ydata<-read.csv(filename,header=TRUE,stringsAsFactors=FALSE)
            city1_inuse<-ydata$city1
            city1_list<-unique(city1_inuse)
        }
    }
    return(city1_list)
}
#=================================================================================================================================

#function to get identify all possible arrivals for city2 if we selected departure for city1 in selected year
arrival_city_after_dcity<-function(year, dcity){
    arrival_city_list<-c()
    for(q in 2011:2015){
        if(year==q){
            filename<-paste0(q,"_table6_withlon.csv")
            ydata<-read.csv(filename,header=TRUE,stringsAsFactors=FALSE)
            acity<-unique(ydata[(ydata)$city1==dcity,]$city2)
            return(acity)
        }
    }
}


#=================================================================================================================================
#                                               Visulization Function By Wenyue Xing
#=================================================================================================================================
#visualization part:
#for departure city visualization:
city_vis_depart<-function(start_city, sel_year,sel_quarter){
    
    table.read<-get_quart_data(sel_year, sel_quarter) # get quarter data by year number and quarter number
    
    n<-length(table.read$fare)
    
    for(i in 1:n){
        if(table.read$fare[i]<=150){
            table.read$Price_Range[i]="Fare <$150"
        }
        if(table.read$fare[i]<=200&table.read$fare[i]>150) {
            table.read$Price_Range[i]="Fare $150~200"
        }
        if(table.read$fare[i]<=250&table.read$fare[i]>200) {
            table.read$Price_Range[i]="Fare $200~250"
        }
        if(table.read$fare[i]<=300&table.read$fare[i]>250) {
            table.read$Price_Range[i]="Fare $250~300"
        }
        if(table.read$fare[i]<=350&table.read$fare[i]>300) {
            table.read$Price_Range[i]="Fare $300~350"
        }
        if(table.read$fare[i]>350){
            table.read$Price_Range[i]="Fare $350~"
        }
    }
    
    table.select.1<-filter(table.read,table.read$city1==start_city)
    states_names <- map_data("state")
    
    # plot for the departure city
    p.d<-ggplot()
    p.d<-p.d + geom_polygon(data=states_names, aes(x=long, y=lat,group=group),colour="white", fill="black")
    p.d_title<-p.d+ggtitle(paste("Visualization of air fare from",start_city,"to other cities"))+xlab("Longtitude")+ylab("Latitude")
    p.d_boldtitle<-p.d_title+theme(plot.title = element_text(size = 25, face = "bold"))
    p.d_boldaxis<-p.d_boldtitle+theme(axis.title = element_text(size = 18, face = "bold"),axis.text=element_text(size=14))
    p.d_boldlegend<-p.d_boldaxis+theme(legend.text=element_text(size=15),legend.title=element_text(size=20))
    p.d_city<-p.d_boldlegend+geom_point( data=table.select.1, aes(x=lon2.long, y=lon2.lat), color="red",size=2)
    p.d_withpoint<-p.d_city+geom_point(data=table.select.1, aes(x=lon1.long, y=lon1.lat),color="blue",size=5)
    p.d_segment<-p.d_withpoint+geom_segment(data=table.select.1,aes(x=lon1.long,y=lon1.lat,xend=lon2.long,yend=lon2.lat,colour=Price_Range,fill=Price_Range),arrow=arrow(length=unit(0.3,"cm")))
    p.d_withtext<-p.d_segment+geom_text(data=table.select.1,aes(x=lon2.long, y=lon2.lat,label = city2),color="chocolate1",size=4.3)
    p.d_output<-p.d_withtext+theme(panel.background = element_rect(fill = "white"))
    
    
    return(p.d_output)
}
#=================================================================================================================================
#for departure city visualization filter:
city_vis_depart_filter<-function(start_city, sel_year,sel_quarter, price_range){
    if(price_range==1){
        price_range="Fare <$150"
    }else{
        if(price_range==2){
            price_range="Fare $150~200"
        }else{
            if(price_range==3){
                price_range="Fare $200~250"
            }else{
                if(price_range==4){
                    price_range="Fare $250~300"
                }else{
                    if(price_range==5){
                        price_range="Fare $300~350"
                    }else{
                        if(price_range==6){
                            price_range="Fare $350~"
                        }
                    }
                }
            }
        }
    }
    table.read<-get_quart_data(sel_year, sel_quarter) # get quarter data by year number and quarter number
    
    n<-length(table.read$fare)
    
    for(i in 1:n){
        if(table.read$fare[i]<=150){
            table.read$Price_Range[i]="Fare <$150"
        }
        if(table.read$fare[i]<=200&table.read$fare[i]>150) {
            table.read$Price_Range[i]="Fare $150~200"
        }
        if(table.read$fare[i]<=250&table.read$fare[i]>200) {
            table.read$Price_Range[i]="Fare $200~250"
        }
        if(table.read$fare[i]<=300&table.read$fare[i]>250) {
            table.read$Price_Range[i]="Fare $250~300"
        }
        if(table.read$fare[i]<=350&table.read$fare[i]>300) {
            table.read$Price_Range[i]="Fare $300~350"
        }
        if(table.read$fare[i]>350){
            table.read$Price_Range[i]="Fare $350~"
        }
    }
    
    table.select.1<-filter(table.read,table.read$city1==start_city,table.read$Price_Range==price_range)
    states_names <- map_data("state")
    
    # plot for the departure city
    p.d<-ggplot()
    p.d<-p.d + geom_polygon(data=states_names, aes(x=long, y=lat,group=group),colour="white", fill="black")
    p.d_title<-p.d+ggtitle(paste("Visualization of air fare from",start_city,"to other cities"))+xlab("Longtitude")+ylab("Latitude")
    p.d_boldtitle<-p.d_title+theme(plot.title = element_text(size = 25, face = "bold"))
    p.d_boldaxis<-p.d_boldtitle+theme(axis.title = element_text(size = 18, face = "bold"),axis.text=element_text(size=14))
    p.d_boldlegend<-p.d_boldaxis+theme(legend.text=element_text(size=15),legend.title=element_text(size=20))
    p.d_city<-p.d_boldlegend+geom_point( data=table.select.1, aes(x=lon2.long, y=lon2.lat), color="red",size=2)
    p.d_withpoint<-p.d_city+geom_point(data=table.select.1, aes(x=lon1.long, y=lon1.lat),color="blue",size=5)
    p.d_segment<-p.d_withpoint+geom_segment(data=table.select.1,aes(x=lon1.long,y=lon1.lat,xend=lon2.long,yend=lon2.lat,colour=Price_Range,fill=Price_Range),arrow=arrow(length=unit(0.3,"cm")))
    p.d_withtext<-p.d_segment+geom_text(data=table.select.1,aes(x=lon2.long, y=lon2.lat,label = city2),color="chocolate1",size=4.3)
    p.d_output<-p.d_withtext+theme(panel.background = element_rect(fill = "white"))
    
    
    return(p.d_output)
}

#=================================================================================================================================
#for arrival city visualization:
city_vis_arrive<-function(end_city, sel_year,sel_quarter){
    
    table.read<-get_quart_data(sel_year, sel_quarter) # get quarter data by year number and quarter number
    n<-length(table.read$fare)
    
    for(i in 1:n){
        if(table.read$fare[i]<=150){
            table.read$Price_Range[i]="Fare <$150"
        }
        if(table.read$fare[i]<=200&table.read$fare[i]>150) {
            table.read$Price_Range[i]="Fare $150~200"
        }
        if(table.read$fare[i]<=250&table.read$fare[i]>200) {
            table.read$Price_Range[i]="Fare $200~250"
        }
        if(table.read$fare[i]<=300&table.read$fare[i]>250) {
            table.read$Price_Range[i]="Fare $250~300"
        }
        if(table.read$fare[i]<=350&table.read$fare[i]>300) {
            table.read$Price_Range[i]="Fare $300~350"
        }
        if(table.read$fare[i]>350){
            table.read$Price_Range[i]="Fare $350~"
        }
    }
    
    table.select.2<-filter(table.read,table.read$city2==end_city)
    states_names <- map_data("state")
    
    #plot for arrival city
    p.a<-ggplot()
    p.a<-p.a + geom_polygon(data=states_names, aes(x=long, y=lat,group=group),colour="white", fill="black")
    p.a_title<-p.a+ggtitle(paste("Visualization of air fare from other cities to",end_city))+xlab("Longtitude")+ylab("Latitude")
    p.a_boldtitle<-p.a_title+theme(plot.title = element_text(size = 25, face = "bold"))
    p.a_boldaxis<-p.a_boldtitle+theme(axis.title = element_text(size = 18, face = "bold"),axis.text=element_text(size=14))
    p.a_boldlegend<-p.a_boldaxis+theme(legend.text=element_text(size=15),legend.title=element_text(size=20))
    p.a_city<-p.a_boldlegend+geom_point( data=table.select.2, aes(x=lon2.long, y=lon2.lat), color="red",size=5)
    p.a_withpoint<-p.a_city+geom_point(data=table.select.2, aes(x=lon1.long, y=lon1.lat),color="blue",size=2)+geom_segment(data=table.select.2,aes(x=lon1.long,y=lon1.lat,xend=lon2.long,yend=lon2.lat,color=Price_Range,fill=Price_Range), arrow=arrow(length=unit(0.3,"cm")))
    p.a_withtext<-p.a_withpoint+geom_text(data=table.select.2,aes(x=lon1.long, y=lon1.lat,label = city1),color="chocolate1",size=4.3)
    p.a_final<-p.a_withtext+theme(panel.background = element_rect(fill = "white"))
    return(p.a_final)
}

#=================================================================================================================================
#for arrival city visualization filter:
city_vis_arrive_filter<-function(end_city, sel_year,sel_quarter, price_range){
    if(price_range==1){
        price_range="Fare <$150"
    }else{
        if(price_range==2){
            price_range="Fare $150~200"
        }else{
            if(price_range==3){
                price_range="Fare $200~250"
            }else{
                if(price_range==4){
                    price_range="Fare $250~300"
                }else{
                    if(price_range==5){
                        price_range="Fare $300~350"
                    }else{
                        if(price_range==6){
                            price_range="Fare $350~"
                        }
                    }
                }
            }
        }
    }
    table.read<-get_quart_data(sel_year, sel_quarter) # get quarter data by year number and quarter number
    n<-length(table.read$fare)
    
    
    for(i in 1:n){
        if(table.read$fare[i]<=150){
            table.read$Price_Range[i]="Fare <$150"
        }
        if(table.read$fare[i]<=200&table.read$fare[i]>150) {
            table.read$Price_Range[i]="Fare $150~200"
        }
        if(table.read$fare[i]<=250&table.read$fare[i]>200) {
            table.read$Price_Range[i]="Fare $200~250"
        }
        if(table.read$fare[i]<=300&table.read$fare[i]>250) {
            table.read$Price_Range[i]="Fare $250~300"
        }
        if(table.read$fare[i]<=350&table.read$fare[i]>300) {
            table.read$Price_Range[i]="Fare $300~350"
        }
        if(table.read$fare[i]>350){
            table.read$Price_Range[i]="Fare $350~"
        }
    }
    
    table.select.2<-filter(table.read,table.read$city2==end_city,table.read$Price_Range==price_range)
    
    states_names <- map_data("state")
    
    #plot for arrival city
    p.a<-ggplot()
    p.a<-p.a + geom_polygon(data=states_names, aes(x=long, y=lat,group=group),colour="white", fill="black")
    p.a_title<-p.a+ggtitle(paste("Visualization of air fare from other cities to",end_city))+xlab("Longtitude")+ylab("Latitude")
    p.a_boldtitle<-p.a_title+theme(plot.title = element_text(size = 25, face = "bold"))
    p.a_boldaxis<-p.a_boldtitle+theme(axis.title = element_text(size = 18, face = "bold"),axis.text=element_text(size=14))
    p.a_boldlegend<-p.a_boldaxis+theme(legend.text=element_text(size=15),legend.title=element_text(size=20))
    p.a_city<-p.a_boldlegend+geom_point( data=table.select.2, aes(x=lon2.long, y=lon2.lat), color="red",size=5)
    p.a_withpoint<-p.a_city+geom_point(data=table.select.2, aes(x=lon1.long, y=lon1.lat),color="blue",size=2)+geom_segment(data=table.select.2,aes(x=lon1.long,y=lon1.lat,xend=lon2.long,yend=lon2.lat,color=Price_Range,fill=Price_Range), arrow=arrow(length=unit(0.3,"cm")))
    p.a_withtext<-p.a_withpoint+geom_text(data=table.select.2,aes(x=lon1.long, y=lon1.lat,label = city1),color="chocolate1",size=4.3)
    p.a_final<-p.a_withtext+theme(panel.background = element_rect(fill = "white"))
    return(p.a_final)
}


#=================================================================================================================================
#                                               Statistical analysis and baggage functions By Fang Hu
#=================================================================================================================================
#statistical analysis and baggage information part:
# For a given city in city1, like 'NYC', select all the cities that have flight from the
# given city, store the data in 'citydataframe'
######         Departure analysis      #####
statistical_departp1<-function(cityname, year, quarter, num_bars){
    quarterdata<-get_quart_data(year,quarter)
    citydata<-quarterdata %>% filter(str_detect(city1, cityname))
    citydataframe<-as.data.frame(citydata)
    
    
    # plot the graph of average fare versus destination cities (city2)
    
    titlenames_1<-paste('The average fares of airlines from ',cityname,' to other cities')
    p1<-ggplot(citydataframe, aes(x=reorder(city2, fare), y=fare)) +
    xlab('Destination') + ylab(' Overall average fare')  + ggtitle(titlenames_1) +
    geom_bar(stat='identity', fill="#FF9999", colour="black") +coord_flip() +
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    return(p1)
}
#=================================================================================================================================
statistical_departp2<-function(cityname, year, quarter,num2_bars){
    quarterdata<-get_quart_data(year,quarter)
    citydata<-quarterdata %>% filter(str_detect(city1, cityname))
    citydataframe<-as.data.frame(citydata)
    
    
    #  plot the graph of average passenger versus destination cities (city2)
    titlenames_2<-paste('The Passengers per day of airlines from ',cityname,' to other cities')
    p2<-ggplot(citydataframe, aes(x=reorder(city2, passengers), y=passengers)) +
    xlab('Destination') + ylab('Passengers per day')  + ggtitle(titlenames_2) +
    geom_bar(stat='identity', fill="red", colour="black") +coord_flip()+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    return(p2)
    
}
#=================================================================================================================================
statistical_departp3p4<-function(cityname,year,quarter){
    quarterdata<-get_quart_data(year,quarter)
    citydata<-quarterdata %>% filter(str_detect(city1, cityname))
    citydataframe<-as.data.frame(citydata)
    
    # draw the scatterplot of overall average fare versus city distance for different city2
    
    cor_fare_dis<-cor(citydataframe$nsmiles,citydataframe$fare)
    text0<-paste('Correlation between average fare and distance is ', round(cor_fare_dis,digits = 3))
    text0<-as.character(text0)
    x0_posi<- min(citydataframe$nsmiles)+ (max(citydataframe$nsmiles)-min(citydataframe$nsmiles))/2
    y0_posi<-max(citydataframe$fare)-
    (max(citydataframe$fare)-min(citydataframe$fare))/8
    p3<-ggplot(citydataframe,aes(nsmiles,fare)) + geom_point() + geom_smooth() +
    annotate("text", x=x0_posi, y=y0_posi, label= text0,size=8,colour='red') +
    xlab('distance') + ylab('Average fare')+
    ggtitle('The relationship between overall average fare and city distance')+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    #p3
    
    
    # draw the scatterplot of overall average fare versus Passengers per day for different city2
    
    cor_fare_pasger<-cor(citydataframe$passengers,citydataframe$fare)
    text00<-paste('Correlation between average fare and passengers is ', round(cor_fare_pasger,digits = 3))
    text00<-as.character(text00)
    x00_posi<- min(citydataframe$passengers)+ (max(citydataframe$passengers)-min(citydataframe$passengers))/2
    y00_posi<-max(citydataframe$fare)-
    (max(citydataframe$fare)-min(citydataframe$fare))/8
    p4<-ggplot(citydataframe,aes(passengers,fare)) + geom_point() +
    geom_smooth()+ annotate("text", x=x00_posi, y=y00_posi, label= text00,size=8,colour='red') +
    xlab('Passengers per day') + ylab('Average fare')+ggtitle('The relationship between overall average fare and passengers per day')+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    #p4
    
    return(grid.arrange(p3,p4,nrow=2))
}

#=================================================================================================================================
# summarise the numbers that each carrier be the largest carrier and be the lowest fare carrier for the given whole year
summa_depart<-function(year){
    table_data<-get_year_data(year)
    larg_carrier<-table_data %>% group_by(carrier_lg) %>% summarise(app_num=n())
    low_fare_carri<-table_data %>% group_by(carrier_low) %>% summarise(app_num2=n())
    
    # draw the graphs of numbers versus carrier
    
    colnames(larg_carrier)<-c('carrier_name','number_of_being_largest')
    colnames(low_fare_carri)<-c('carrier_name','number_of_being_lowest')
    
    carrier<-full_join(larg_carrier,low_fare_carri)
    
    carrier[is.na(carrier$number_of_being_largest),2]<-0
    carrier[is.na(carrier$number_of_being_lowest),2]<-0
    
    carrier_data<-melt(carrier,id.vars = 'carrier_name')
    ggplot(carrier_data, aes(carrier_name, value)) + xlab('Carrier') + ylab('Number') +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity",width = 0.8, colour="black") +
    scale_fill_discrete(name ="Types",breaks=c("number_of_being_largest", "number_of_being_lowest"),labels=c("Numbers of carrier \n being the top \n market share leader", "Numbers of carrier \n being the lowest \n average fare provider"))+
    ggtitle('Number of carrier being the top market share leaders \n (the lowest average fare provider) for each carrier')+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))+
    theme(legend.title = element_text(face="bold", size=15))+
    theme(legend.text = element_text(face="bold", size=13))
}
#=================================================================================================================================

######         Arrival analysis      #####

# For a given city in city1, like 'NYC', select all the cities that have flight from the
# given city, store the data in 'citydataframe'
statistical_arrivalp1<-function(cityname.2, year, quarter,num_bars_arr){
    quarterdata<-get_quart_data(year,quarter)
    citydata.2<-quarterdata %>% filter(str_detect(city2, cityname.2))
    citydataframe.2<-as.data.frame(citydata.2)
    
    
    
    # plot the graph of overall average fare versus destination cities (city2)
    
    titlenames_1.2<-paste('The average fares of airlines from other cities to',cityname.2)
    p1.2<-ggplot(citydataframe.2, aes(x=reorder(city1, fare), y=fare)) +
    xlab('Destination') + ylab(' Overall average fare')  + ggtitle(titlenames_1.2) +
    geom_bar(stat='identity', fill="#FF9999", colour="black") +coord_flip()+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    return(p1.2)
}
#=================================================================================================================================

statistical_arrivalp2<-function(cityname.2, year, quarter,num2_bars_arr){
    
    quarterdata<-get_quart_data(year,quarter)
    citydata.2<-quarterdata %>% filter(str_detect(city2, cityname.2))
    citydataframe.2<-as.data.frame(citydata.2)
    
    
    #  plot the graph of Passengers versus destination cities (city2)
    titlenames_2.2<-paste('The Passengers per day of airlines from other cities to ',cityname.2)
    p2.2<-ggplot(citydataframe.2, aes(x=reorder(city1, passengers), y=passengers)) +
    xlab('Destination') + ylab('Passengers per day')  + ggtitle(titlenames_2.2) +
    geom_bar(stat='identity', fill="red", colour="black") +coord_flip()+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    return(p2.2)
}
#=================================================================================================================================
# draw the scatterplot of overall average fare versus city distance for different city1
statistical_arrivalp3p4<-function(cityname.2, year, quarter){
    
    quarterdata<-get_quart_data(year,quarter)
    citydata.2<-quarterdata %>% filter(str_detect(city2, cityname.2))
    citydataframe.2<-as.data.frame(citydata.2)
    
    cor_fare_dis.2<-cor(citydataframe.2$nsmiles,citydataframe.2$fare)
    text1<-paste('Correlation between average fare and distance is ', round(cor_fare_dis.2,digits = 3))
    text1<-as.character(text1)
    x_posi<- min(citydataframe.2$nsmiles)+ (max(citydataframe.2$nsmiles)-min(citydataframe.2$nsmiles))/2
    y_posi<-max(citydataframe.2$fare)-
    (max(citydataframe.2$fare)-min(citydataframe.2$fare))/8
    p3.2<-ggplot(citydataframe.2,aes(nsmiles,fare)) + geom_point() + geom_smooth() +
    annotate("text", x=x_posi, y=y_posi, label= text1,size=8,colour='red') +
    xlab('distance') + ylab('Average fare')+
    ggtitle('The relationship between overall average fare and city distance')+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    #p3.2
    
    # draw the scatterplot of overall average fare versus Passengers per day for different city1
    cor_fare_pasger.2<-cor(citydataframe.2$passengers,citydataframe.2$fare)
    text11<-paste('Correlation between average fare and passengers is ', round(cor_fare_pasger.2,digits = 3))
    text11<-as.character(text11)
    x1_posi<- min(citydataframe.2$passengers)+ (max(citydataframe.2$passengers)-min(citydataframe.2$passengers))/2
    y1_posi<-max(citydataframe.2$fare)-
    (max(citydataframe.2$fare)-min(citydataframe.2$fare))/8
    p4.2<-ggplot(citydataframe.2,aes(passengers,fare)) + geom_point() +
    geom_smooth()+
    annotate("text", x=x1_posi, y=y1_posi, label= text11,size=8,colour='red') +
    xlab('Passengers per day') + ylab('Average fare')+
    ggtitle('The relationship between overall average fare and passengers per day')+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))
    #p4.2
    
    return(grid.arrange(p3.2,p4.2,nrow=2))
}
#=================================================================================================================================
# summarise the numbers that each carrier be the largest carrier and be the lowest fare carrier for the given whole year
summa_arrival<-function(year){
    table6<-get_year_data(year)
    larg_carrier.2<-table6 %>% group_by(carrier_lg) %>% summarise(app_num=n())
    low_fare_carri.2<-table6 %>% group_by(carrier_low) %>% summarise(app_num2=n())
    
    # draw the graphs of numbers versus carrier
    
    colnames(larg_carrier.2)<-c('carrier_name','number_of_being_largest')
    colnames(low_fare_carri.2)<-c('carrier_name','number_of_being_lowest')
    
    carrier.2<-full_join(larg_carrier.2,low_fare_carri.2)
    
    carrier.2[is.na(carrier.2$number_of_being_largest),2]<-0
    carrier.2[is.na(carrier.2$number_of_being_lowest),2]<-0
    
    carrier_2<-melt(carrier.2,id.vars = 'carrier_name')
    j<-ggplot(carrier_2, aes(carrier_name, value)) + xlab('Carrier') + ylab('Number') + geom_bar(aes(fill = variable), position = "dodge", stat="identity",width = 0.8, colour="black") +
    scale_fill_discrete(name ="Types",breaks=c("number_of_being_largest", "number_of_being_lowest"),labels=c("Numbers of carrier \n being the top \n market share leader", "Numbers of carrier \n being the lowest \n average fare provider"))+
    ggtitle('Number of carrier being the top market share leaders \n (the lowest average fare provider) for each carrier')+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15))+
    theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))+
    theme(legend.title = element_text(face="bold", size=15))+
    theme(legend.text = element_text(face="bold", size=13))
    return(j)
}
#=================================================================================================================================
#Quarterly time trend analysis in a given year
quart_time_trend_analysis<-function(dcity, acity, year){ #first graph in first subtab
    
    city1_name<-dcity
    city2_name<-acity
    table6_2015<-get_year_data(year)
    
    # select the data in a whole year for the given two cities
    selec_data<-table6_2015 %>% filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    
    # plot the data in a whole year for the given two cities
    
    selec_data_fare<-selec_data %>% select(quarter, fare, fare_lg, fare_low)
    fare_min<-min(selec_data_fare$fare,selec_data_fare$fare_lg,selec_data_fare$fare_low)
    fare_max<-max(selec_data_fare$fare,selec_data_fare$fare_lg,selec_data_fare$fare_low)
    fare_low_boud<-floor(fare_min/100)*100
    fare_upper_boud<-ceiling(fare_max/15)*15
    
    selec_fare<-melt(selec_data_fare, id.vars = 'quarter')
    # output
    quarterly_plot<- ggplot(selec_fare, aes(quarter, value)) + ylab('Average Fare') +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity",width = 0.8) +
    coord_cartesian(ylim=c(fare_low_boud,fare_upper_boud)) +
    ggtitle('Quarterly time trend analysis in a given year') +
    scale_fill_discrete(name ="Three types \n of average fare",breaks=c("fare", "fare_lg","fare_low"),labels=c("For all carrier", "For largest carrier", "For lowest carrier")) +
    theme(plot.title = element_text( face="bold", size=25, hjust=0))+
    theme(axis.title = element_text(face="bold", size=18))+
    theme(legend.title = element_text(face="bold", size=15))+
    theme(legend.text = element_text(face="bold", size=13))
    
    
    return(quarterly_plot)
}


quart_time_trend_analysis_2<-function(dcity, acity, year){#first table in first subtab
    
    city1_name<-dcity
    city2_name<-acity
    table6_2015<-get_year_data(year)
    
    # select the data in a whole year for the given two cities
    selec_data<-table6_2015 %>% filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    
    # give the carrier names of the largest carrier and lowest carrier
    carrier_name<-selec_data %>% select(Year,quarter,largest_carrier=carrier_lg,lowest_carrier=carrier_low)
    # output the 'carrier_name' as a table with the title 'The carrier names of the largest carrier and lowest carrier'
    
    return(carrier_name)
}

quart_time_trend_analysis_3<-function(dcity, acity, year){#second table in first subtab
    
    city1_name<-dcity
    city2_name<-acity
    table6_2015<-get_year_data(year)
    
    # select the data in a whole year for the given two cities
    selec_data<-table6_2015 %>% filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    
    # give the baggage information of largest carrier and lowest carrier,
    # 'baggage' is the baggage information table loaded at first
    
    name_carrier_uniq<-unique(c(selec_data$carrier_lg,selec_data$carrier_low))
    remain<-ceiling(nrow(baggage)/length(name_carrier_uniq))*length(name_carrier_uniq)
    
    remain2<-(nrow(baggage)+1):remain
    baggage2<-baggage
    baggage2[remain2,]<-NA
    baggage_inf<-filter(baggage2,IATA_code %in% name_carrier_uniq)
    baggage_inf<-baggage_inf[,-1]  
    
    
    # output the 'baggage_inf' as a table with the title 'The baggage information of largest carrier and lowest carrier'
    return(baggage_inf)
}
#=================================================================================================================================
#Time trend analysis in selected quarter across years
quarter_time_trend_across<-function(dcity, acity, quart){# frist graph in second subtab
    city1_name<-dcity
    city2_name<-acity
    quart<-as.integer(quart)
    table6_2015<-data2015;table6_2014<-data2014;table6_2013<-data2013;table6_2012<-data2012;table6_2011<-data2011;
    # select the data of a certain quarter for 2011-2015 for the given two cities
    select_horiz_data_15<-table6_2015 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_14<-table6_2014 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_13<-table6_2013 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_12<-table6_2012 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_11<-table6_2011 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data<-rbind(select_horiz_data_11,select_horiz_data_12,select_horiz_data_13,select_horiz_data_14,select_horiz_data_15)
    select_horiz_fare<-select_horiz_data %>% select(Year, fare, fare_lg,fare_low)
    
    # plot the data in a whole year for the given two cities
    
    fare2_min<-min(select_horiz_fare$fare,select_horiz_fare$fare_lg,select_horiz_fare$fare_low)
    fare2_max<-max(select_horiz_fare$fare,select_horiz_fare$fare_lg,select_horiz_fare$fare_low)
    fare2_low_boud<-floor(fare2_min/100)*100
    fare2_upper_boud<-ceiling(fare2_max/25)*25
    
    select_fare_melt<-melt(select_horiz_fare, id.vars = 'Year')
    # following is output
    timetrend_plot<-ggplot(select_fare_melt, aes(Year, value)) + ylab('Average Fare') +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity",width = 0.8) +
    coord_cartesian(ylim=c(fare2_low_boud,fare2_upper_boud)) +
    scale_fill_discrete(name ="Three types \n of average fare",breaks=c("fare", "fare_lg","fare_low"),labels=c("For all carrier", "For largest carrier", "For lowest carrier"))+
    ggtitle('Time trend analysis in selected quarter across years')+
    theme(plot.title = element_text( face="bold", size=25, hjust=0))+
    theme(axis.title = element_text(face="bold", size=18))+
    theme(legend.title = element_text(face="bold", size=15))+
    theme(legend.text = element_text(face="bold", size=13))
    
    return(timetrend_plot)
}


quarter_time_trend_across_2<-function(dcity, acity, quart){#first table in second subtab
    city1_name<-dcity
    city2_name<-acity
    quart<-as.integer(quart)
    table6_2015<-data2015;table6_2014<-data2014;table6_2013<-data2013;table6_2012<-data2012;table6_2011<-data2011;
    # select the data of a certain quarter for 2011-2015 for the given two cities
    select_horiz_data_15<-table6_2015 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_14<-table6_2014 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_13<-table6_2013 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_12<-table6_2012 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_11<-table6_2011 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>%
    filter(str_detect(city2, city2_name))
    
    select_horiz_data<-rbind(select_horiz_data_11,select_horiz_data_12,select_horiz_data_13,select_horiz_data_14,select_horiz_data_15)
    select_horiz_fare<-select_horiz_data %>% select(Year, fare, fare_lg,fare_low)
    
    # give the carrier names of the largest carrier and lowest carrier
    carrier_name_2<-select_horiz_data %>% select(Year,quarter,largest_carrier=carrier_lg,lowest_carrier=carrier_low)
    # output the 'carrier_name_2' as a table with the title 'The carrier names of the largest carrier and lowest carrier'
    return(carrier_name_2)
}


quarter_time_trend_across_3<-function(dcity, acity, quart){#second table in second subtab
    city1_name<-dcity
    city2_name<-acity
    quart<-as.integer(quart)
    table6_2015<-data2015;table6_2014<-data2014;table6_2013<-data2013;table6_2012<-data2012;table6_2011<-data2011;
    # select the data of a certain quarter for 2011-2015 for the given two cities
    select_horiz_data_15<-table6_2015 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>% 
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_14<-table6_2014 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>% 
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_13<-table6_2013 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>% 
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_12<-table6_2012 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>% 
    filter(str_detect(city2, city2_name))
    
    select_horiz_data_11<-table6_2011 %>% filter(quarter==quart) %>%
    filter(str_detect(city1, city1_name)) %>% 
    filter(str_detect(city2, city2_name))
    
    select_horiz_data<-rbind(select_horiz_data_11,select_horiz_data_12,select_horiz_data_13,select_horiz_data_14,select_horiz_data_15)
    select_horiz_fare<-select_horiz_data %>% select(Year, fare, fare_lg,fare_low)
    
    name2_carrier_uniq<-unique(c(select_horiz_data$carrier_lg, select_horiz_data$carrier_low))
    remain<-ceiling(nrow(baggage)/length(name2_carrier_uniq))*length(name2_carrier_uniq)
    
    remain2<-(nrow(baggage)+1):remain
    baggage2<-baggage
    baggage2[remain2,]<-NA
    baggage2_inf<-filter(baggage2,IATA_code %in% name2_carrier_uniq)
    baggage2_inf<-baggage2_inf[,-1]  
    return(baggage2_inf)
}

#=================================================================================================================================

#The Below Server Code By Yifu Liu

#=================================================================================================================================
###renderUI for departure city and arrival city part
shinyServer(function(input, output){
  #load_data()
  #input for city_sel_after_yrs
  city_selection<-reactive({
    csay<-city_sel_after_yrs(input$year
    )
    paste(csay)
  })
  #link output for UI departure city selection

  output$city_list_dep1 <- renderUI({
    selectInput(inputId ="dcity", label = h5("Departure City"), choices=city_selection(), selected = "New York City, NY")
  })

  
  #input for all arrive city if departure city selected by function: arrival_city_after_dcity
  acity_list_selection<-reactive({
    csay<-arrival_city_after_dcity(input$year, input$dcity
    )
    paste(csay)
  })
  #link output for UI arrival city selection

  output$city_list_arr1 <- renderUI({
    selectInput(inputId ="acity", label = h5("Arrival City"), choices=acity_list_selection(), selected = "Boston, MA")
  })
 
  #=================================================================================================================================
  
  #input for statistical analysis departure part for p1
  formula_stat_depart_city1<-function(){
    stat_depart_city_output1<-statistical_departp1(input$dcity, input$year, input$quarter, input$num1
                                              )
  plot(stat_depart_city_output1)
  }
  #download above plot
  output$download_formula_stat_depart_city1<- downloadHandler(
    filename = "p1.png",
    content = function(file) {
      png(file)
      print(formula_stat_depart_city1())
      dev.off()
    }) 
  #input for statistical analysis departure part for p2
  formula_stat_depart_city2<-function(){
    stat_depart_city_output2<-statistical_departp2(input$dcity, input$year, input$quarter, input$num2
    )
    plot(stat_depart_city_output2)
  }
  #download above plot
  output$download_formula_stat_depart_city2<- downloadHandler(
    filename = "p2.png",
    content = function(file) {
      png(file)
      print(formula_stat_depart_city2())
      dev.off()
    }) 
  #input for statistical analysis departure part for p3p4
  formula_stat_depart_city34<-function(){
    stat_depart_city_output34<-statistical_departp3p4(input$dcity, input$year, input$quarter
    )
    plot(stat_depart_city_output34)
  }
  #download above plot
  output$download_formula_stat_depart_city34<- downloadHandler(
    filename = "p3p4.png",
    content = function(file) {
      png(file)
      print(formula_stat_depart_city34())
      dev.off()
    }) 
  
  #input for summarise the numbers that each carrier be the largest carrier and be the lowest fare carrier for the given whole year
  formula_summary<-function(){
    sum_out<-summa_depart(input$year
    )
    plot(sum_out)
  }
  #download above plot
  output$download_formula_summary<- downloadHandler(
    filename = "summary.png",
    content = function(file) {
      png(file)
      print(formula_summary())
      dev.off()
    }) 
  #input for statistical analysis arrival part for p1
  formula_stat_arrival_city1<-function(){
    stat_arrival_city_output1<-statistical_arrivalp1(input$acity, input$year, input$quarter,input$num3
    )
    plot(stat_arrival_city_output1)
  }
  #download above plot
  output$download_formula_stat_arrival_city1<- downloadHandler(
    filename = "p1.png",
    content = function(file) {
      png(file)
      print(formula_stat_arrival_city1())
      dev.off()
    }) 
  #input for statistical analysis arrival part for p2
  formula_stat_arrival_city2<-function(){
    stat_arrival_city_output2<-statistical_arrivalp2(input$acity, input$year, input$quarter, input$num4
    )
    plot(stat_arrival_city_output2)
  }
  #download above plot
  output$download_formula_stat_arrival_city2<- downloadHandler(
    filename = "p2.png",
    content = function(file) {
      png(file)
      print(formula_stat_arrival_city2())
      dev.off()
    }) 
  #input for statistical analysis arrival part for p3p4
  formula_stat_arrival_city34<-function(){
    stat_arrival_city_output34<-statistical_arrivalp3p4(input$acity, input$year, input$quarter
    )
    plot(stat_arrival_city_output34)
  }
  #download above plot
  output$download_formula_stat_arrival_city34<- downloadHandler(
    filename = "p3p4.png",
    content = function(file) {
      png(file)
      print(formula_stat_arrival_city34())
      dev.off()
    }) 
  #input for summarise the numbers that each carrier be the largest carrier and be the lowest fare carrier for the given whole year
  formula_summary_arrival<-function(){
    sum_out<-summa_arrival(input$year
    )
    plot(sum_out)
  }
  #download above plot
  output$downloadformula_summary_arrival<- downloadHandler(
    filename = "summary.png",
    content = function(file) {
      png(file)
      print(formula_summary_arrival())
      dev.off()
    }) 
  

  #input for departure city_map_visulization
  formula_city_depart<-function(){
    city_output<-city_vis_depart(input$dcity, input$year,input$quarter
                                  )
    plot(city_output)
    }
  #download above plot
  output$downloadPlot_map_depart<- downloadHandler(
    filename = "map_depart.png",
    content = function(file) {
      png(file)
      print(formula_city_depart())
      dev.off()
    }) 
  #input for departure city_map_visulization by price filter
  formula_city_depart_filter<-function(){
    city_output<-city_vis_depart_filter(input$dcity, input$year,input$quarter, input$price
    )
    plot(city_output)
  }
  #download above plot
  output$downloadPlot_map_depart_filter<- downloadHandler(
    filename = "map_depart_filter.png",
    content = function(file) {
      png(file)
      print(formula_city_depart_filter())
      dev.off()
    }) 
  #input for arrival city_map_visulization
  formula_city_arrival<-function(){
    city_output<-city_vis_arrive(input$acity, input$year,input$quarter
    )
    plot(city_output)
  }
  #download above plot
  output$downloadPlot_map_arr<- downloadHandler(
    filename = "map_arrival.png",
    content = function(file) {
      png(file)
      print(formula_city_arrival())
      dev.off()
    }) 
  #input for arrival city_map_visulization by price filter
  formula_city_arrival_filter<-function(){
    city_output<-city_vis_arrive_filter(input$acity, input$year,input$quarter, input$price1
    )
    plot(city_output)
  }
  #download above plot
  output$downloadPlot_map_arr_filter <- downloadHandler(
    filename = "map_arr_filter.png",
    content = function(file) {
      png(file)
      print(formula_city_arrival_filter())
      dev.off()
    }) 

  #input for Quarterly time trend analysis in a given year
  quarter_time_trend<-function(){
    cc<-quart_time_trend_analysis(input$dcity, input$acity, input$year
    )
    plot(cc)
  }
  #download above plot
  output$downloadPlot <- downloadHandler(
    filename = "YourPlot.png",
    content = function(file) {
      png(file)
      print(quarter_time_trend())
      dev.off()
    }) 
  #input for Quarterly time trend analysis2
  quarter_time_trend2<-function(){
    cc<-quart_time_trend_analysis_2(input$dcity, input$acity, input$year
    )
  }
  #download above table
  output$downloadtable1 <- downloadHandler(
    filename = "Yourtable1.csv",
    content = function(file) {
      write.csv(quarter_time_trend2(), file)
    }) 
  #input for Quarterly time trend analysis3
  quarter_time_trend3<-function(){
    cc<-quart_time_trend_analysis_3(input$dcity, input$acity, input$year
    )
  }
  #download above table
  output$downloadtable2 <- downloadHandler(
    filename = "Yourtable2.csv",
    content = function(file) {
      write.csv(quarter_time_trend3(), file)
    }) 
  
  #input for Time trend analysis in selected quarter across years
  q_time_across<-function(){
    csay<-quarter_time_trend_across(input$dcity,input$acity,input$quarter
    )
    plot(csay)
  }
  #download above plot
  output$downloadPlot11 <- downloadHandler(
    filename = "YourPlot.png",
    content = function(file) {
      png(file)
      print(q_time_across())
      dev.off()
    }) 
  #input for Time trend analysis in selected quarter across2
  q_time_across2<-function(){
    csay<- quarter_time_trend_across_2(input$dcity,input$acity,input$quarter
    )
  }
  #download above table
  output$downloadtable11 <- downloadHandler(
    filename = "Yourtable1.csv",
    content = function(file) {
      write.csv(q_time_across2(), file)
    }) 
  #input for Time trend analysis in selected quarter across3
  q_time_across3<-function(){
    csay<-quarter_time_trend_across_3(input$dcity,input$acity,input$quarter
    )
  }
  #download above table
  output$downloadtable22 <- downloadHandler(
    filename = "Yourtable2.csv",
    content = function(file) {
      write.csv(q_time_across3(), file)
    }) 
 
  #input for Raw Table
  raw_table<-reactive({
    csay<-get_year_data(input$year
    )
  })
  
  #link output for statistical analysis departure part p1
  output$stat_depart_city_out1 <- renderPlot({
    formula_stat_depart_city1()
  })
  
  #link output for statistical analysis departure part p2
  output$stat_depart_city_out2 <- renderPlot({
    formula_stat_depart_city2()
  })
  #link output for statistical analysis departure part p3p4
  output$stat_depart_city_out34 <- renderPlot({
    formula_stat_depart_city34()
  })
  #link output for summarise the numbers that each carrier be the largest carrier and be the lowest fare carrier for the given whole year
  output$formula_summary_out_put <- renderPlot({
    formula_summary()
  })
  
  #link output for statistical analysis arrival part p1
  output$stat_arrival_city_out1 <- renderPlot({
    formula_stat_arrival_city1()
  })
  #link output for statistical analysis arrival part p2
  output$stat_arrival_city_out2 <- renderPlot({
    formula_stat_arrival_city2()
  })
  #link output for statistical analysis arrival part p3p4
  output$stat_arrival_city_out34 <- renderPlot({
    formula_stat_arrival_city34()
  })
  #link output for summarise the numbers that each carrier be the largest carrier and be the lowest fare carrier for the given whole year
  output$formula_summary_arrival_out_put <- renderPlot({
    formula_summary_arrival()
  })
  
  #link output for depart_city_map_visulization  
  output$depart_city_out <- renderPlot({
    formula_city_depart()
  })
  #link output for depart_city_map_visulization by price range filter
  output$depart_city_out_price <- renderPlot({
    formula_city_depart_filter()
  })
  
  #link output for arrival_city_map_visulization  
  output$arrival_city_out <- renderPlot({
    formula_city_arrival()
  })
  
  #link output for arrival_city_map_visulization by price range filter
  output$arrival_city_out_price <- renderPlot({
    formula_city_arrival_filter()
  })
  
  #link output for Quarterly time trend analysis in a given year  
  output$q_time_trend_given_year <- renderPlot({
    quarter_time_trend()
  })
  #link output for Quarterly time trend analysis2
  output$q_time_trend_given_year2 <- renderTable({
    quarter_time_trend2()
  })
  #link output for Quarterly time trend analysis3
  output$q_time_trend_given_year3 <- renderTable({
    quarter_time_trend3()
  })

  #link output for Time trend analysis in selected quarter across years 
  output$qtime_across_out <- renderPlot({
    q_time_across()
  })
  #link output for Time trend analysis in selected quarter across2 
  output$qtime_across_out2 <- renderTable({
    q_time_across2()
  })
  #link output for Time trend analysis in selected quarter across3
  output$qtime_across_out3 <- renderTable({
    q_time_across3()
  })

  #link output for part of Raw Table
  output$raw_table_out <- renderTable({
    j<-raw_table()
    j[c(1:100),c(-1,-2,-6,-7,-19,-20)]
  })
  #link output for part of MetaData
  output$MetaData <- renderTable({
    read.csv('MetaData.csv',header=TRUE,stringsAsFactors=FALSE)
  })
  
  
}

)

