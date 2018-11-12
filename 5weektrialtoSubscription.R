# Scripts for EDA of codeSpark 5-week window Trial to Subscription data and updated churn model
# Author: Evan Rushton 
# Date: 10/16/18
# Attribution: NBD/Pareto (https://github.com/blendo-app)

library(ggplot2)
library(corrplot)
library(randomForest)
library(sqldf)
library(glmnet)
library(caret)

# Import the csv file 
df <- read.table("./Data/Churn_5-week_windows_Past_3_months.csv", fill = TRUE, header = TRUE, sep = ",", na.strings = c("NA","")) # 64813 23

# ===== Cleaning =====
# Convert variable types
df$
  
  
  
# Check NA
sapply(df, function(y) sum(length(which(is.na(y)))))

# ===== Visualize Vars =====
#Histograms and tables to check distributions of vars
par(mfrow = c(3, 2)) 
hist(df$time_spent / 60, main="Time Spent (mins)") # raw in seconds
hist(df$time_spent[which(df$time_spent < 21600)] / 60, main="Minutes Played (Under 360 mins)")
hist(df$value.minutes_played[which(df$value.minutes_played < 100)], main="Minutes Played (Under 100 mins)")
hist(df$value.sub_Duration, main="Subscription Duration (days)")
hist(df$value.sub_Duration[which(df$value.sub_Duration < 150)], main="Subscription Duration (Under 150 days)", breaks=seq(0,150,by=30))
hist(df$value.sub_Duration[which(df$value.sub_Duration < 70)], main="Subscription Duration (Under 70 days)", breaks=seq(0,70,by=14))
hist(df$value.play_game)
hist(df$value.completed_puzzle)
hist(log(df$value.completed_puzzle))
table(df$value.account_Status)
table(df$value.age)


# Correlation plot for numeric variables
df.n<-sapply(df,class)=='numeric' | sapply(df,class)=='integer' # numeric columns
dfNum<-df[,df.n] # subset numeric
corr<-cor(dfNum)
corrplot(corr, type = "lower", tl.pos = "ld")
corr[,1]


# Logistic Models
md1 = glm(canceled ~ ., data=df, family=binomial(link="logit"))
md2 = glm(canceled ~ canceled + time_spent + , data=df, family=binomial(link="logit"))
summary(md1)
