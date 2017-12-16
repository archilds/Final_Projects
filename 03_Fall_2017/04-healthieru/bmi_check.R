source("check_packages.R")

# Function of BMI Diagnosis (Used for health.analysis function)
bmi.diagnosis<-function(bmi){
  if (bmi < 18.5) {
    Diagnosis <- "Underweight" #underweight if bmi less than 18.5
  } else if (bmi >= 18.5 && bmi < 25) {
    Diagnosis <- "Normal Weight" #normal bmi if between 18.5 and 25
  } else if (bmi >= 25 && bmi < 30) {
    Diagnosis <- "Overweight" #overweight bmi if between 25 and 30 
  } else if (bmi >= 30) { #obese if bmi over 30
    Diagnosis <- "Obese"
  }
  return(Diagnosis)
}

# Function for computing BMI (Used for graph and table creation in output)
health.analysis <- function(height,weight, target.weight, unit=input$metric_sys) {    # Input height (in) and weight (lb) into function
  
  if (unit=="Standard"){
    BMI <- weight / height^2 * 703                     # Converts height (in) and weight (lb) into BMI
    target.BMI <- target.weight / height^2 * 703       # Converts height (in) and target weight (lb) into BMI
  } else if (unit=="Metric"){
    BMI <- (weight*2.205) / (height*0.3937)^2 * 703    # Converts height (in) and weight (lb) into BMI
    target.BMI <- target.weight*2.205 / (height*0.3937)^2 * 703       # Converts height (in) and target weight (lb) into BMI
  }
  
  # Classifies Diagnosis based on BMI 
  Diagnosis=bmi.diagnosis(BMI)
  target.diagnosis=bmi.diagnosis(target.BMI)
  
  output<-list(BMI=BMI,Diagnosis=Diagnosis, Target.BMI=target.BMI, Target.Diagnosis=target.diagnosis)       # Creates a list so function can return two variables
  return(output)      # Function outputs variable BMI and Diagnosis
}
