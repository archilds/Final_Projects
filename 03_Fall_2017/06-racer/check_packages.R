## The "racecar package has to be installed in the repository below
## https://github.com/PHP2560-Statistical-Programming-R/r-package-racecar
### Please:
### 1. download this repository
### 2. run the code "install.packages("racecar", type = "source", repos = NULL)"
### 3. go back to this shiny-app-racer repository and run app.R

pkgs <- c("shiny", "shinythemes", "plotly", "tidyverse")

pkgTest <- function(pkgs){ 
  for (i in pkgs){
    if (!require(i,character.only = TRUE)){
      install.packages(i,dep=TRUE, repos = NULL, type = "source")
    }
  }
}

## Just in case that the code above does not work for these two packages
## If that happens, you can run the code below seperately
install.packages("shinythemes")
install.packages("tidyverse")

