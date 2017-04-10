setwd("/Users/masahiro/Documents/Brown Courses/Term 7/PHP2560/Project8/PHP2560FinallApp3")
load("trump.Rdata")
dataFrame <- c
names <- names(c)
classes<-sapply(c,class)

# Loop to Generate Histograms
for(name in names[classes == 'numeric'])
{
  dev.new()
  hist(c[,name], xlab=name, main="Histogram") # subset with [] not $
}


# Loop to Generate Barplots
for(name in names[classes == 'numeric'])
{
  dev.new()
  barplot(c[,name], xlab=name, main="Barplot") # subset with [] not $
}

#Generate Pie Chart for Sentiment Score
pie(table(c$score), main="Pie Chart of Score")

#Generate Pie Chart for Sentiment
pie(table(c$sentiment), main="Pie Chart")



#Regressions

