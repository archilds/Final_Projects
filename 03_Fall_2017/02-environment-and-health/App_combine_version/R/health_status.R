#' @title Check your health status
#'
#' @description This function helps you to evaluate your health status
#' @param data a data.frame
#' @param cbsa cbsa in US
#' @param year A numeric vector
#' @param smoke whether people smoke or not, equal to -1 if you smoke, else is 1. Default is 1
#' @param exercise whether people has certain exercise every day, equal to -1 if you don't, else is 1. Default is 1
#' @param gene whether people has certain gene may cause disease happen,equal to -1 for bad gene or 1 for good gene. Default is 1
#' @seealso
#' @return
#' @examples
#'
#' @export



health_status<-function(data=annual_aqi, cbsa, year, smoke, exercise, gene){
  env_status=0
  health_data <- data%>%filter(CBSA %in% cbsa & Year %in% year)
  names(health_data)[13] <- "MedianAQI"
  if(mean(health_data$MedianAQI)>=0&mean(health_data$MedianAQI)<=50){
    env_status = env_status + 2
  } else if(mean(health_data$MedianAQI)>50&mean(health_data$MedianAQI)<=100){
    env_status = env_status + 1
  } else if(mean(health_data$MedianAQI)>100&mean(health_data$MedianAQI)<=150){
    env_status = env_status
  } else if(mean(health_data$MedianAQI)>150&mean(health_data$MedianAQI)<=200){
    env_status = env_status - 1
  } else if(mean(health_data$MedianAQI)>200&mean(health_data$MedianAQI)<=300){
    env_status = env_status - 2
  } else{
    env_status = env_status - 3
  }
  if(smoke == "Yes"){
    smoke = -1
  } else{
    smoke = 1
  }
  if(exercise == "Yes"){
    exercise = 1
  } else{
    exercise = -1
  }
  if(gene == "Yes"){
    gene = -1
  } else{
    gene = 1
  }
  status = env_status + smoke + exercise + gene
  if(status>=4&status<=6){
    print('Healthy!Keep going!:)')
  } else if(status>=-2&status<=3){
    print('Sub Healthy.Maybe you can do more to improve!;)')
  } else{
    print('Not healthy.:( You should be careful of your health!')
  }
}
