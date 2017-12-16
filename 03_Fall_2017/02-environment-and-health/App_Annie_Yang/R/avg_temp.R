#' @title Average Temperatures Trend
#'
#' @description This function helps you analyse global as well as countries average temperatures trend during a specified time period. 
#'     You can use it to do comparison between countries.
#' @param data A data.frame. Use GlobalTemperatures or GlobalLandTemperaturesByCountry dataset to do analysis.
#' @param type A vector giving which type of trend you want to plot. 1 indicates yearly trend. 2 indicates monthly trend. c(1,2) indicates both yearly and monthly average temperature trend.
#' @param year A vector. Time period during which you want to analyse how the temperature changed.
#' @param month A vector. Time period during which you want to analyse how the temperature changed.
#' @param country A vector of countries' name. Compare how the temperature changed in these countries during a specified time period.
#' @param con Display confidence interval around smooth? (TRUE for confidence interval. FALSE by default)
#' @examples
#' avg_temp(data=GlobalLandTemperaturesByCountry, type=c(1,2),
#' year=c(2000:2015), month=c(1:12), 
#' country = c("Afghanistan","Andorra"),con="F")
#' 
#' @export

# Load GlobalTemperatures or GlobalLandTemperaturesByCountry dataset before using this function

avg_temp <- function(data, type, year, month, country, con = "FALSE"){
  
  if(missing(country)){ # when data=GlobalTemperatures, the country argument is missing
    glb_avg_temp <- data %>% 
      mutate(Month=as.numeric(format(data$dt,"%m")), # Create new column month (decimal number)
             Month.String=format(data$dt,"%B"), # Create string month (full name)
             Year=as.numeric(format(data$dt,"%Y")))%>% # Create new column year (4 digit)
      select(dt, Month, Month.String, Year, LandAverageTemperature, LandAverageTemperatureUncertainty)%>%
      na.omit() # Remove missing values
    
    
    avg_temp_year<-glb_avg_temp %>% 
      filter(Year %in% year)%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(AverageTemp=mean(LandAverageTemperature)) %>% # Calculate yearly average temperature
      ungroup()%>%
      ggplot(aes(x = Year, y = AverageTemp)) +
      geom_smooth(method="loess", se=con)+
      labs(title = "Global Yearly Average Temperatures",
           x="Year",
           y="Average Temperature")
    
    avg_temp_month<-glb_avg_temp %>%
      filter(Year %in% year & Month %in% month)%>%
      ggplot(aes(x=dt, y=LandAverageTemperature,colour=reorder(Month.String,-LandAverageTemperature,mean)))+
      # Sort month from highest temperature to lowest temperature
      geom_smooth(method="loess",se=con)+
      labs(title="Global Average Temperatures By Month",
           x="Year",
           y="Average Temperature",
           colour="Month")
    
    if(identical(type,c(1,2))){
      ggpubr::ggarrange(avg_temp_year,avg_temp_month,ncol=2,nrow=1)
    } else if(type==1) {
      avg_temp_year
    } else {
      avg_temp_month
    }
  } else {
    glb_avg_temp <- data %>% 
      mutate(Month=as.numeric(format(data$dt,"%m")),
             Month.String=format(data$dt,"%B"),
             Year=as.numeric(format(data$dt,"%Y")))%>% # Create new columns month and year
      select(dt, Month, Month.String, Year, Country, AverageTemperature, AverageTemperatureUncertainty)%>%
      na.omit() %>% # Remove missing values
      filter(Country %in% country)
    
    avg_temp_year <- glb_avg_temp %>% 
      filter(Year %in% year)%>%
      dplyr::group_by(Year, Country) %>%
      dplyr::summarise(AverageTemp=mean(AverageTemperature)) %>% # Calculate yearly average temperature by country
      ungroup()%>%
      ggplot(aes(x = Year, y = AverageTemp,colour=Country)) +
      geom_smooth(method="loess", se=con)+
      labs(title = "Yearly Average Temperatures By Country",
           x="Year",
           y="Average Temperature")
    
    avg_temp_month <- glb_avg_temp %>%
      filter(Year %in% year & Month %in% month)%>%
      ggplot(aes(x=dt, y=AverageTemperature,colour=reorder(Month.String,-AverageTemperature,mean)))+
      geom_smooth(method="loess",se=con)+
      facet_wrap(~Country,scales="free")+
      labs(title="Monthly Average Temperatures By Country",
           x="Year",
           y="Average Temperature",
           colour="Month")
    
    if(identical(type,c(1,2))){
      ggpubr::ggarrange(avg_temp_year,avg_temp_month)
    } else if(type==1) {
      avg_temp_year
    } else {
      avg_temp_month
    }
  }
}