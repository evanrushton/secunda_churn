# Scripts for EDA of codeSpark Segmentation data and initial churn model
# Author: Evan Rushton 
# Date: 9/26/18
# Attribution: RF Model from Eleni Markou (https://github.com/blendo-app)

library(ggplot2)
library(corrplot)
library(randomForest)

# Import the csv file 
df <- read.table("./Data/Segmentation_Report.csv", fill = TRUE, header = TRUE, sep = ",", na.strings = c("NA","")) # 64813 23

# ===== Data Cleaning =====
# Check NAs
sapply(df, function(y) sum(length(which(is.na(y))))) # these are for blanks that have been converted to NA
df$value.age[which(is.na(df$value.age))] <- '?' # Merge blank age fields with '?'
levels(df$value.gender) <- c("female", "male", "?") # Create '?' level for gender
df$value.gender[which(is.na(df$value.gender))] <- '?' # Make NA values unknown gender
levels(df$value.country) <- c(levels(df$value.country), "?") # Create '?' level for country
df$value.country[which(is.na(df$value.country))] <- '?' # Make NA values unknown gender

# Converting value.sub_Start and value.sub_End to Date (exclude time)
df$value.sub_End <- as.Date(df$value.sub_End, "%Y-%m-%d");
df$value.sub_Start <- as.Date(df$value.sub_Start, format="%Y-%m-%d")

# Duration of subscription
df$value.sub_End[which(df$value.account_Status=="Active Subscriber")] <- "2018-08-06" # Make blank end dates the current date
df$value.sub_Duration <- as.integer(df$value.sub_End - df$value.sub_Start) / (365/12) # convert days to months

# Gifted Accounts
df[which(format(df$value.sub_End, "%Y")==2118),] # Active Gifted are incorrectly labeled churn=1
table(format(df$value.sub_End[which(format(df$value.sub_End, "%Y")>2100)], "%Y")) # Remove "Gifted" accounts should solve this
df <- df[which(!df$value.account_Status %in% c("Active Gifted", "Expired Gifted")),]
df$value.account_Status[which(is.na(df$value.sub_End))]

# Remove Outliers (Initially 64446 24)
dim(df[which(df$value.play_game > 10000), ] ) # check variables/thresholds

df <- df[which(df$value.sub_Duration < 1000), ] # Subscriptions over 83 months and NA durations (61881 24)
df <- df[which(df$value.minutes_played < 10000), ] # Playtime over 167 hours (61550 24)
df <- df[which(df$value.sub_Duration > 0), ] # Negative subscription times?? (61368 24)
# Expired subscribers who aren't labeled churn=1?
dim(df[which(df$Churn. == 0 & df$value.account_Status == "Expired Subscriber"), ]) # 1287 removed in prior cleanup steps
df$value.account_Status <- droplevels(df$value.account_Status)

sapply(df, function(y) sum(length(which(is.na(y))))) # No more NA values
summary(df)

# ===== Visualize Vars =====
#Histograms and tables to check distributions of vars
hist(df$value.minutes_played)
hist(df$value.sub_Duration)
hist(df$value.play_game)
hist(df$value.completed_puzzle)
hist(log(df$value.completed_puzzle))
table(df$value.account_Status)
table(df$value.age)

# plot churn versus some of the more straight-forward vars
df$Churn. <- as.factor(df$Churn.)
#Pairwise scatterplots
ggplot(df, aes(x= Churn., y = value.sub_Duration))+
  geom_jitter(alpha=0.5, aes(color=Churn.), position = position_jitter(width = 0.1))+coord_flip()

ggplot(df,aes(x= value.age )) + geom_bar(aes(fill=Churn.))

ggplot(df,aes(x= value.sub_Duration, y= value.minutes_played)) + geom_point(aes(color=Churn.))

# ===== Feature Engineering =====
# Ratio of minutes played : length of subscription
df$ratio.min_played_to_duration <- df$value.minutes_played / df$value.sub_Duration

#log-transform poor distributions (minutes_played, app_open, sub_duration
#df$value.log_minutes_played <- log(df$value.minutes_played)
#df$value.log_app_open <- log(df$value.app_open)
#df$value.log_sub_duration <- log(df$value.sub_Duration)

# Remove key, account_status (churn depends on this), sub_start/end, age (too many ?), country (too many categories)
df <- df[, -c(1,3,4,5,7,9)]
df <- df[, c(2:3,1,4:ncol(df))]

# ===== Model Testing =====
# Define train and test sets
smp_size <- floor(0.75 * nrow(df)) # 75% train
set.seed(28)
train_ind <- sample(seq_len(nrow(df)), size = smp_size) # random indeces 
train <-df[train_ind, ] 
test <- df[-train_ind, ]

# Correlation plot for numeric variables
train.n<-sapply(train,class)=='numeric' | sapply(train,class)=='integer'
trainNum<-train[,train.n]
corr<-cor(trainNum)
corrplot(corr, type = "lower", tl.pos = "ld")

#Modeling with random forest
#Random forest can handle only numeric variables and factors
train.n<-sapply(train,class)!='character'
trainF<-train[,train.n]

set.seed(28)
#model construction
randomForestModel <- randomForest(Churn. ~.,
                                  data=trainF, 
                                  importance=TRUE, 
                                  ntree=2000)

#variable importance plot
varImpPlot(randomForestModel)

#making predictions
prediction <- predict(randomForestModel, test)

#evaluating the model
confMatrix<-table(prediction,test$churn)
A<-confMatrix[4]
B<-confMatrix[2]
C<-confMatrix[3]
D<-confMatrix[1]

accuracy<- (A+D)/(A+B+C+D)
accuracy
specificity<-D/(B+D)
specificity
sensitivity<-A/(A+C)
sensitivity
precision<-A/(A+B)
precision
F1<-2*precision*sensitivity/(precision+sensitivity)
F1

#rerun random forest. This time only with the 5 most important variables
set.seed(42)

#constructint
randomForestModel <- randomForest(as.factor(churn) ~last_changed+stats_avg_open_rate+
                                    member_rating+totalopens+list_id,
                                  data=trainF, 
                                  importance=TRUE, proximity=TRUE,
                                  ntree=2000)
varImpPlot(randomForestModel)

#predicting
prediction <- predict(randomForestModel, test)

#evaluating
confMatrix<-table(prediction,test$churn)

D<-confMatrix[1]
C<-confMatrix[3]
A<-confMatrix[4]
B<-confMatrix[2]
accuracy<- (A+D)/(A+B+C+D)
accuracy
specificity<-D/(B+D)
specificity
sensitivity<-A/(A+C)
sensitivity
precision<-A/(A+B)
precision
F1<-2*precision*sensitivity/(precision+sensitivity)
F1

auc(test$churn, prediction)


