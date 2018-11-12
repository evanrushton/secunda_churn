
library(tidyverse)
library(corrplot)
library(randomForest)
library(sqldf)
library(glmnet)
library(caret)

data1718 <- read_csv("./Data/ChurnSeg_Past_6_months_.csv") # 53203
# Converting value.sub_Start and value.sub_End to Date (exclude time)
data1718$value.sub_End <- as.Date(data1718$value.sub_End, "%Y-%m-%d");
data1718$value.sub_Start <- as.Date(data1718$value.sub_Start, format="%Y-%m-%d")
data1718 %>% 
  filter(value.sub_Start >= "2017-08-15") # 31288


data1718 %>% 
  arrange(value.sub_Start)

# Duration of subscription
data1718$value.sub_End[which(data1718$value.account_Status=="Active Subscriber")] <- "2018-08-06" # Make blank end dates the current date
data1718$value.sub_Duration <- as.integer(data1718$value.sub_End - data1718$value.sub_Start) # Days





# ===== Data Cleaning =====
# Check NAs
sapply(data1718, function(y) sum(length(which(is.na(y))))) # these are for blanks that have been converted to NA
data1718$value.age[which(is.na(data1718$value.age))] <- '?' # Merge blank age fields with '?'
levels(data1718$value.gender) <- c("female", "male", "?") # Create '?' level for gender
data1718$value.gender[which(is.na(data1718$value.gender))] <- '?' # Make NA values unknown gender
levels(data1718$value.country) <- c(levels(data1718$value.country), "?") # Create '?' level for country
data1718$value.country[which(is.na(data1718$value.country))] <- '?' # Make NA values unknown gender



# Gifted Accounts
data1718[which(format(data1718$value.sub_End, "%Y")==2118),] # Active Gifted are incorrectly labeled churn=1
table(format(data1718$value.sub_End[which(format(data1718$value.sub_End, "%Y")>2100)], "%Y")) # Remove "Gifted" accounts should solve this
data1718 <- data1718[which(!data1718$value.account_Status %in% c("Active Gifted", "Expired Gifted")),]
data1718$value.account_Status[which(is.na(data1718$value.sub_End))]

# Remove Outliers (Initially 64446 24)
dim(data1718[which(data1718$value.minutes_played > 10000), ] ) # check variables/thresholds

data1718 <- data1718[which(data1718$value.sub_Duration < 1000), ] # Removes NA durations (61881 24)
data1718 <- data1718[which(data1718$value.minutes_played < 10000), ] # Playtime over 167 hours (61550 24)
data1718 <- data1718[which(data1718$value.sub_Duration > 0), ] # Negative subscription times?? (61368 24) (These 59 users re-subscribed?)

# Expired subscribers who aren't labeled churn=1?
dim(data1718[which(data1718$Churn. == 0 & data1718$value.account_Status == "Expired Subscriber"), ]) # 1287 removed in prior cleanup steps
data1718$value.account_Status <- droplevels(data1718$value.account_Status)

sapply(data1718, function(y) sum(length(which(is.na(y))))) # No more NA values
summary(data1718)



data1718 %>% 
  ggplot(aes(value.minutes_played))+
  geom_histogram()+
  scale_x_log10()
