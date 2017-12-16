#' @title Plot Temperature Geographic Maps by States in USA
#'
#' @description This function plots temperature geographic maps for States in USA in specific year
#' @param data A data.frame. The defalt dataset is GlobalLandTemperaturesByState.
#' @param year numeric
#' @examples
#' temp_state(year=2012)
#' 
#' @export

# Loading Packages
# library(choroplethr)
# library(choroplethrMaps)
# library(plotly)
# library(countrycode)

temp_state<-function(data=GlobalLandTemperaturesByState,year){
  
map <- data %>%
  mutate(Month=as.numeric(format(data$dt,"%m")), # Create new column month (decimal number)
         Month.String=format(data$dt,"%B"), # Create string month (full name)
         Year=as.numeric(format(data$dt,"%Y"))) %>% # Create new column year (4 digit)
  na.omit() %>% filter(Country=="United States")

map$State <- as.character(map$State)  
map$State[map$State=="Georgia (State)"] <- "Georgia" # Changing Georgia (State)
map$State<- as.factor(map$State)                    

#' select columns of interest
map_select <- map %>% 
  select(Year,AverageTemperature,State) %>%
  dplyr::group_by(Year, State) %>%
  dplyr::summarise(value=mean(AverageTemperature))

#Data frame must have a column named region (all lower case) and another one value.
colnames(map_select)[2]<- "region"
map_select$region<-tolower(map_select$region)

map_state<-map_select %>%
  filter(Year==year)

map_state<-map_state[,2:3]

print(state_choropleth(map_state,
                       title = paste("Land Temperature",year," "), 
                       num_colors = 8,
                       legend = "Degrees"),reference_map=TRUE)
}

#' @title Plot Temperature Geographic Maps by Country
#'
#' @description This function plots temperature geographic maps for countries in specific year. You can use this
#'     function to get a temperature geographic map showing the temperature change from the start year to end year.
#' @param data A data.frame. The defalt dataset is GlobalLandTemperaturesByCountry.
#' @param year A numeric. You can get temperature geographic maps for countries in this year.
#' @param start A numeric. The start year you want to do temperature comparison.
#' @param end A numeric. Then end year you want to do temperature comparison.
#' @param diff A character. If diff=="TRUE", you will get a temperature geographic map showing the temperature
#'     change from the start year to end year. (Default value is "FALSE")
#' @examples
#' temp_country(year=2012)
#' temp_country(start=1990,end=2000,diff="TRUE")

temp_country<-function(data=GlobalLandTemperaturesByCountry, year, start, end, diff="FALSE"){
  # light grey boundaries
  l <- list(color = toRGB("grey"), width = 0.5)
  
  # specify map projection/options
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
 
  map_country <- data %>%
    mutate(Month=as.numeric(format(data$dt,"%m")), # Create new column month (decimal number)
           Month.String=format(data$dt,"%B"), # Create string month (full name)
           Year=as.numeric(format(data$dt,"%Y"))) %>% # Create new column year (4 digit)
    na.omit()%>%
    select(Year,AverageTemperature,Country) %>%
    dplyr::group_by(Year, Country) %>%
    dplyr::summarise(AvgTemp=mean(AverageTemperature))
  
  code<-countrycode(map_country$Country,'country.name', 'iso3c') # Converts long country name into country codes
  
  map_country$CODE<-code # Create new column in map_country named "CODE"
  
  if(diff=="FALSE"){
  temp<-map_country%>%filter(Year==year)
  
  map_temp <- plot_geo(temp) %>%
    add_trace(
      z = ~AvgTemp, color = ~AvgTemp, colors = 'Reds',
      text = ~Country, locations = ~CODE, marker = list(line = l)
    ) %>%
    colorbar(title = 'Temperature') %>%
    layout(
      title = paste(year,"Temperature Map",sep=" "),
      geo = g
    )
  
  map_temp
  } else if (diff=="TRUE") {
    
    temp_diff<-map_country %>% 
      filter(Year==start | Year==end) %>% 
      tidyr::spread(Year, AvgTemp)
    
    temp_diff$Difference<-unlist(temp_diff[,4]-temp_diff[,3]) # Calculate temperature variation from start year to end year
    
    map_temp <- plot_geo(temp_diff) %>%
      add_trace(
        z = ~Difference, color = ~Difference, colors = 'Reds',
        text = ~Country, locations = ~CODE, marker = list(line = l)
      ) %>%
      colorbar(title = 'Temperature Variation') %>%
      layout(
        title = paste(start,"-",end,"Temperature Variation Map", sep=" "),
        geo = g
      )
    print(map_temp)
  }
  
}
