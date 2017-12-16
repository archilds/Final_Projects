source("bmi_check.R")

#Webscraping
#Save URL into a variable
url.1<-"http://www.nutristrategy.com/caloriesburned.htm"
#Use getURL to download the URL
urldata.1<-getURL(url.1)
#read the table from the website 
data_table.1<-readHTMLTable(urldata.1, which=1)
#print out data table
table.paste<-function(){
  old=data.frame(data_table.1[[1]][13:261])
  for (i in 2:5){
    old=cbind(old,data.frame(data_table.1[[i]][13:261])) #append the tables into one table
  }
  return(old)
}
data_table.1=table.paste()

#gather lame exercises
lame_exercises<-c(3:4,7:9,11,13,22,24,26:28,33,35,39,41,43,45,48:56,60:64,66,68:74,81:82,84:85,89,93:100,104:105,108,116,118,120:121,124,126,128:129,134:136,138:139,141:169,171:178,184:194,197:204,208:210,214,216:249)

#create column names 
colnames(data_table.1)<-c("Activity","p130","p155","p180","p205")

#take out lame exercises
data_table.1<-data_table.1[-lame_exercises, ]

#melt columns into one column except Activity column, similar to proc transpose in SAS
data_table.2<-melt(data_table.1, id="Activity")
data_table.2<-data_table.2 %>% 
  mutate_at("variable", funs(as.numeric(substr(., 2, 4)))) %>% #truncate 'p130' to 130, 'p155' to 155, etc.
  mutate(metric=variable*.454)%>% #create metric variable
  arrange(Activity)

mods.standard <- dlply(data_table.2, .(Activity), lm, formula=as.numeric(as.character(value))~as.numeric(variable)-1) #regression function for standard measurement
mods.metric <- dlply(data_table.2, .(Activity), lm, formula=as.numeric(as.character(value))~as.numeric(metric)-1) #regression function for metric measurement
coefs.standard = ldply(mods.standard, coef) #gather standard measurement coefficients
coefs.metric= ldply(mods.metric, coef) #gather metric measure coefficients 

reg_table= inner_join(data_table.1, coefs.standard) %>% inner_join(.,coefs.metric, by="Activity") #join the cofficients back to the original table
names(reg_table)[6:7]<-c("standard", "metric") #rename column names