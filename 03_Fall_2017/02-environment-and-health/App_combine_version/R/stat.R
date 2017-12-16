#' @title Calculate summary statistics for AQI
#'
#' @description This function helps you calculate summary statistics for AQI by CBSA.
#' @param data A data.frame
#' @param cbsa CBSA in US
#' @seealso
#' @ruturn
#' @examples stat_func(cbsa="Ames, IA")
#' 
#' @export
#' Loading packages
#' library(dplyr)
#' library(Rmisc)


stat_func<-function(data=aqi,cbsa){
  data_stat<-data%>%
    filter(CBSA==cbsa)
  stat_table <- as.array(summary(data_stat$`Median AQI`))
  stat_df <- as.data.frame(stat_table)
  colnames(stat_df) <- c("Stat", "Value")
  Confi_Interval <- CI(data_stat$`Median AQI`, ci = 0.95) #Caculate the confidence interval of Median AQI
  lower <- data.frame('CI lower',Confi_Interval[['lower']])
  upper <- data.frame('CI upper',Confi_Interval[['upper']])
  names(lower) <- c("Stat", "Value")
  names(upper) <- c("Stat", "Value")
  stat_df <- rbind(stat_df,lower, upper)
  return(stat_df)
}

