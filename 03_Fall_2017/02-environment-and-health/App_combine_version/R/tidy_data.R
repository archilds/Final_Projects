#' @title Tidy Environment Dataset
#'
#' @description This function helps you tidy environment dataset, deal with the dates information, converts dates
#'     to Date class and create new column "Month" and "Year" to help you analyse the data.
#' @param data a data.frame
#' @param date a character. It is the column name of the data and this column containing dates information.
#' @param na a character, either "F" or "T". The default value is "F". If the na=="F", it will remove rows with missing values.
#' @return If na = "F", it will return a data frame without NA. The column names are with only lowercase letters, with "_" as a separator.
#' @examples
#' tidy_data(pollution_us, "Date Local")
#' 
#' @export

tidy_data <- function(data,date,na="F") {
  
  colnames(data)[which(names(data) == date)] <- "date" # change the name of column containing dates to "date"
  
  if(class(data$date)=="character") {
    date<-as.Date(data$date) # Convert dates encoded as characters to Date class
  } else if (is.numeric(data$date)){
    janitor::excel_numeric_to_date(data$date) # Convert dates encoded as serial numbers to Date class
  }
  
  clean_data <- data %>% mutate(Month=as.numeric(format(data$date,"%m")), # Create new column month (decimal number)
                  Year=as.numeric(format(data$date,"%Y"))) # Create new column year (4 digit)
  if(na=="F"){
  clean_data<-na.omit(clean_data) # Remove rows with missing values
  }
  
  clean_data<-janitor::clean_names(clean_data) # Returns a data.frame and names with only lowercase letters, with "_" as a separator
  
  return(clean_data)
}