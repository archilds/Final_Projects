#' @title Compare AQI category percentage by CBSA
#'
#' @description There are six categories of AQI. Each category corresponds to a different level of health concern. This function
#'     plots percentage of each category by CBSA. It can be used to compare AQI among different CBSAs.
#' @param data A data.frame. The default dataset is annual_aqi.
#' @param cbsa A vector of CBSA. You can input the CBSAs you want to compare.
#' @param category A character vector of health concern category by AQI. There are six categories.
#'     Input category from "Good", "Moderate", "UnhealthyForSensitiveGroup", "Unhealthy", "VeryUnhealthy" and "Hazardous".
#'     Default value includes all six categories.
#' @param year A numeric vector.
#' @param plot A character. The value is either "line" or "bar".
#'     Default value is "line". You can use it to decide which kind of plot you want to get.
#' @examples
#' aqi_healthconcern(annual_aqi,c("Aberdeen, SD","Adrian, MI"),
#' category =c("Good","Moderate","Unhealthy"),year=c(2000,2008,2015),plot="bar")
#'
#' @export

aqi_healthconcern<-function(data=annual_aqi,cbsa,
                            category=c("Good", "Moderate", "UnhealthyForSensitiveGroup", "Unhealthy", "VeryUnhealthy","Hazardous"),
                            year=c(2000:2015),plot="line"){


  healthconcern <- data%>%
    mutate(
      Good = as.numeric(data$'Good Days')/as.numeric(data$'Days with AQI'),
      Moderate = as.numeric(data$'Moderate Days')/as.numeric(data$'Days with AQI'),
      UnhealthyForSensitiveGroup = as.numeric(data$'Unhealthy for Sensitive Groups Days')/as.numeric(data$'Days with AQI'),
      Unhealthy = as.numeric(data$'Unhealthy Days')/as.numeric(data$'Days with AQI'),
      VeryUnhealthy = as.numeric(data$'Very Unhealthy Days')/as.numeric(data$'Days with AQI'),
      Hazardous = as.numeric(data$'Hazardous Days')/as.numeric(data$'Days with AQI')
    )%>%
    select(CBSA,Year,category)%>%
    filter(CBSA %in% cbsa & Year %in% year) # Calculate the percentage of each AQI category.

  if(plot=="line"){
    healthconcern %>%
      tidyr::gather("id", "Percentage", (ncol(healthconcern)-length(category)+1):ncol(healthconcern)) %>%
      ggplot(., aes(Year, Percentage, color=CBSA))+
      geom_smooth(method = "loess", se=FALSE)+
      facet_wrap(~id,scales="free_y")+
      theme_bw()
  } else{
    healthconcern %>%
      tidyr::gather("id", "Percentage", (ncol(healthconcern)-length(category)+1):ncol(healthconcern)) %>%
      ggplot(., aes(Year, Percentage))+
      geom_bar(stat = "identity",aes(fill = CBSA), position = "dodge")+
      facet_wrap(~id,scales="free_y")+
      theme_bw()
  }

}
