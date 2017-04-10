
appendStateToDf <- function(df){
  newFrame = df[!is.na(df$lat),]
  source("find_append.R")
  StatesList = latlong2state(data.frame(x = c(newFrame$lng), y = c(newFrame$lat)))
  StatesList = StatesList[!is.na(StatesList)]
  
  tweetFreq = summary(as.factor(StatesList))
  listedStates = c("alabama",              "alaska",               "arizona",             
                   "arkansas"  ,           "california",           "colorado",            
                   "connecticut",          "delaware",             "district of columbia",
                   "florida",              "georgia",              "hawaii",              
                   "idaho" ,               "illinois",             "indiana" ,            
                   "iowa" ,                "kansas",               "kentucky",            
                   "louisiana" ,           "maine" ,               "maryland",            
                   "massachusetts",        "michigan",             "minnesota",           
                   "mississippi",          "missouri" ,            "montana",             
                   "nebraska",             "nevada"   ,            "new hampshire"  ,     
                   "new jersey"  ,         "new mexico",           "new york" ,           
                   "north carolina" ,      "north dakota",         "ohio",                
                   "oklahoma" ,            "oregon",               "pennsylvania",       
                   "rhode island"  ,       "south carolina",       "south dakota",        
                   "tennessee"  ,          "texas" ,               "utah",                
                   "vermont",              "virginia",             "washington",          
                   "west virginia" ,       "wisconsin",            "wyoming" )
  listedStates
  Statesdf = data.frame(state =listedStates, Freq = rep(0,1,length(listedStates)) )
  #Statesdf= as.data.frame(tweetFreq )
  tweetFreq.df <-as.data.frame(tweetFreq)
  Statesdf[is.element(listedStates, rownames(tweetFreq.df)),2]= tweetFreq.df[,1]
  
  return(Statesdf)
}

latlong2state <- function(dataframe) {
  
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert dataframe to a SpatialPoints object 
  pointsSP <- SpatialPoints(dataframe, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # find where the coordinates are in the state
  indices <- over(pointsSP, states_sp)
  
  # Return the state names
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

addColToData <- function(df1, df_data, variable2){
  # if (length(variable_data) == 1){
  #   variable_data <- rep(0,1,2)
  #   variable_data[1] <-variable_data
  #   variable_data[2] <-variable_data
  # }
#  df1[variable_data[2]] =tolower(df_data[variable_data[1]])
  df1[variable2] <- df_data[variable2]
  return(df1)
}


