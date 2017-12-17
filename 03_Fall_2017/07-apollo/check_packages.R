check_packages = function(names){
  for(name in names){
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org",dependencies=TRUE) #if package not installed, install the package
    
    library(name, character.only=TRUE)
  }
}
# Checks to see if required packages are already installed.
check_packages(c("shiny","ggplot2","scales","dplyr","plotly","janitor"))  #check these packages

# install git packages r-framingham
devtools::install_github("PHP2560-Statistical-Programming-R/r-framingham", dependencies=TRUE)

# install git packages: shinycssloaders
devtools::install_github('andrewsali/shinycssloaders', dependencies=TRUE)

# install git packages: ggradar
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)