### Add description as well as the break down of work to be done by group. 


# HealthierU
authors: Julia Mullokandova, Isaac Zhao, Carol Shum

## Outline of R Package
1. Function health.analysis takes height and weight in either standard or metric units and outputs corresponding BMI and diagnosis of BMI. We will write an equation for BMI. No web scraping will be needed.

2. BMI statistics is compared to the rest of the U.S. population and print out the diagnosis of underweight, normal, or obese. We will use ggplot for graphing. Stratified data for mean and standard error of mean for BMI based on gender and adult age (20+) was retrieved from: https://www.cdc.gov/nchs/data/series/sr_03/sr03_039.pdf

3. Develop a personalized exercise regimen based off of your current weight, your target weight, and the period from now until the target date. We will incorporate the intensity of the exercise and the number of hours desired to exercise weekly. This will in turn ouput a list of activities. Data was webscraped from: http://www.nutristrategy.com/caloriesburned.htm


## Description
### How Does This Work
HealthierU is a reference guide to weight loss. It recommends possible activities for users to try given their gender, weight, target weight and commitment. The activies are pulled from http://www.nutristrategy.com/caloriesburned.htm, a website that lists activities and calories burned by weight for each exercise. After uers enter their gender, height, weight, goal weight and commitment, HealtierU will determine whether they should enter a weight loss program based on their BMI. If they are qualified for a weight loss program then suggest the right exercises for them. 

### To Use
User will select gender, height, weight, desired weight, and commitment. The program will use those data points to anaylze whether the user is fit for a weight loss program using BMI formula. If the user is underweight, HealthierU will allow the user to know a weight loss program is not recommended. If the user is not underweight, then HealthierU will move on the further analysis. 

User's desired weight and commitment are then analyzed if s/he is not in the underweight category, defined by BMI. If the user is listing unrealistic goals such as losing too many lbs/kg over a short period of time, HealthierU will determine that it is an unsafe plan and no activities fit that the criteria. Likewise, if the user has a realistic approach to weight loss, HealthierU will suggest programs for user to try that will help the user to reach his/her weight goal. 

### Types of User Options
Users will enter the following data points: <br />
  -Gender <br />
  -Height <br />
  -Weight <br />
  -Target Weight <br />
  -Commitment 


## Breakdown of Work
Each member of the group is responsible for writing one of the three functions and assisting the other group members in editing the others. 
Debugging, optimizing, and editing code was done collectively and collaboratively among all group members.
