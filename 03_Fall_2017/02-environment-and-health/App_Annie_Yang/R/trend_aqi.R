#' @title Plot AQI trend
#'
#' @description This function plots NO2, O3, SO2 and AQI trend in US city since 2000
#' @param data A data.frame
#' @param city US city name
#' @seealso
#' @examples
#' trend_aqi(pollution_us, "Phoenix")
#' 
#' @export

trend_aqi <- function (data=pollution_us, city) {
  table<-data %>%
    select ("City", "Date Local",ends_with("AQI")) %>%
    filter (City==city )
  
  yyyymm <- paste(format(as.POSIXlt(table$'Date Local'), format = "%y-%m"), "01", sep = "-")
  
  AQI_df <- table %>% select (ends_with("AQI"))
  
  monthly_mean<-as.data.frame(apply(AQI_df, 2, function(x) tapply(x, yyyymm, mean,na.rm=TRUE)))
  
  start_year <- as.numeric(paste0("20",substr(rownames(monthly_mean)[1], 1,2)))
  start_month <- as.numeric(substr(rownames(monthly_mean)[1], 4,5))
  
  monthly_mean <- ts(monthly_mean, start = c(start_year, start_month), frequency = 12)
  
  plot(monthly_mean,main=paste(city,"Monthly AQI Trend",sep=" "))
}