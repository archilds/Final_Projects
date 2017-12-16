# To run app, you don't have to run this R script. This R script can tell you how to get the aqi_pollution
# dataset, which is needed for plot the Pollutant AQI trend.

source("R/tidy_data.R")
load(file = "data/pollution_us.rda")

aqi_pollutant<-tidy_data(pollution_us,'Date Local',na="T")%>%
  dplyr::group_by(year, month, city) %>%
  dplyr::summarise(
    AverageNo2=mean(no2_aqi,na.rm=T),
    AverageO3=mean(o3_aqi,na.rm=T),
    AverageSo2=mean(so2_aqi,na.rm=T),
    AverageCo=mean(co_aqi,na.rm=T)
            )


aqi_pollutant$dt<- zoo::as.yearmon(paste(aqi_pollutant$year, aqi_pollutant$month, sep = "-")) 
save(aqi_pollutant, file = "data/aqi_pollutant.rda")

