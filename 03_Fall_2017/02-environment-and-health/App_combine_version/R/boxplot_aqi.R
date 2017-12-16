#' @title Make the Boxplot of AQI
#'
#' @description This function helps you make boxplots of AQI in the year you are interested. And also mark the point of the CBSA you are interested.
#'     So that you can know the AQI of the CBSA is relatively high or low compared to other geographic areas. You can make comparison of AQI in
#'     different years by the notch. if two boxes' notches do not overlap there is ‘strong evidence’ (95% confidence) their medians differ.
#' @param data A data.frame. The default dataset is annual_aqi.
#' @param year A vector, years in which you want to display the distribution of AQI.
#' @param cbsa A CBSA in US.
#' @examples
#' boxplot_aqi(annual_aqi,c("2000","2005"),"Albany, GA")
#' 
#' @export



boxplot_aqi<-function(data=annual_aqi,year, cbsa){
  
names(data)[13] <- "MedianAQI"

interested_cbsa<-data%>%filter(CBSA==cbsa & Year %in% year)

box_data<-data%>%filter(Year %in% year)

ggplot(box_data,aes(x=as.factor(Year), y=MedianAQI, fill=as.factor(Year)))+
  geom_boxplot(alpha=0.2,
               # Does the midian of AQI differ? 
               # if two boxes' notches do not overlap there is ‘strong evidence’ (95% confidence) their medians differ.
               notch=TRUE,
               notchwidth = 0.5,
               
               outlier.colour="blue",
               outlier.fill="blue",
               outlier.size=2
               )+
  geom_point(data=interested_cbsa,
             aes(x=as.factor(Year), y=MedianAQI),
             color="red", size=2)+
  labs(x= "Year")+
  theme(legend.position="none")
}