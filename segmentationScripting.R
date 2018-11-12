# Scripts for EDA of codeSpark Segmentation data and initial churn model
# Author: Evan Rushton 
# Date: 9/26/18
# Attribution: RF Model from Eleni Markou (https://github.com/blendo-app)

library(ggplot2)
library(corrplot)
library(randomForest)
library(sqldf)
library(glmnet)
library(caret)

# Import the csv file 
df <- read.table("./Data/Segmentation_Report.csv", fill = TRUE, header = TRUE, sep = ",", na.strings = c("NA","")) # 64813 23

# Correlation plot for numeric variables
df.n<-sapply(df,class)=='numeric' | sapply(df,class)=='integer' # numeric columns
dfNum<-df[,df.n] # subset numeric
corr<-cor(dfNum)
corrplot(corr, type = "lower", tl.pos = "ld")
corr[ 1:16, 2]

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
df$value.sub_Duration <- as.integer(df$value.sub_End - df$value.sub_Start) # Days

# Gifted Accounts
df[which(format(df$value.sub_End, "%Y")==2118),] # Active Gifted are incorrectly labeled churn=1
table(format(df$value.sub_End[which(format(df$value.sub_End, "%Y")>2100)], "%Y")) # Remove "Gifted" accounts should solve this
df <- df[which(!df$value.account_Status %in% c("Active Gifted", "Expired Gifted")),]
df$value.account_Status[which(is.na(df$value.sub_End))]

# Remove Outliers (Initially 64446 24)
dim(df[which(df$value.minutes_played > 10000), ] ) # check variables/thresholds

df <- df[which(df$value.sub_Duration < 1000), ] # Removes NA durations (61881 24)
df <- df[which(df$value.minutes_played < 10000), ] # Playtime over 167 hours (61550 24)
df <- df[which(df$value.sub_Duration > 0), ] # Negative subscription times?? (61368 24) (These 59 users re-subscribed?)

# Expired subscribers who aren't labeled churn=1?
dim(df[which(df$Churn. == 0 & df$value.account_Status == "Expired Subscriber"), ]) # 1287 removed in prior cleanup steps
df$value.account_Status <- droplevels(df$value.account_Status)

sapply(df, function(y) sum(length(which(is.na(y))))) # No more NA values
summary(df)

# ===== Visualize Vars =====
#Histograms and tables to check distributions of vars
par(mfrow = c(3, 2)) 
hist(df$value.minutes_played, main="Minutes Played")
hist(df$value.minutes_played[which(df$value.minutes_played < 1000)], main="Minutes Played (Under 1000 mins)")
hist(df$value.minutes_played[which(df$value.minutes_played < 100)], main="Minutes Played (Under 100 mins)")
hist(df$value.sub_Duration, main="Subscription Duration (days)")
hist(df$value.sub_Duration[which(df$value.sub_Duration < 150)], main="Subscription Duration (Under 150 days)", breaks=seq(0,150,by=30))
hist(df$value.sub_Duration[which(df$value.sub_Duration < 70)], main="Subscription Duration (Under 70 days)", breaks=seq(0,70,by=14))
hist(df$value.play_game)
hist(df$value.completed_puzzle)
hist(log(df$value.completed_puzzle))
table(df$value.account_Status)
table(df$value.age)

# plot churn versus some of the more straight-forward vars
#Pairwise plots
ggplot(df, aes(x= as.factor(Churn.), y = value.sub_Duration))+
  geom_jitter(alpha=0.5, aes(color=as.factor(Churn.)), position = position_jitter(width = 0.1))+coord_flip()

ggplot(df,aes(x= value.age )) + geom_bar(aes(fill=as.factor(Churn.)))
ggplot(df,aes(x= value.gender )) + geom_bar(aes(fill=as.factor(Churn.)))

ggplot(df,aes(x= value.sub_Duration, y= value.minutes_played)) + geom_point(aes(color=as.factor(Churn.)))

boxplot(value.sub_Duration~Churn.,data=df, main="Subscription Duration, Churn or No Churn", 
        xlab="Churn", ylab="Subscription Duration (Days)")

# ===== Feature Engineering =====
# Ratio of minutes played : length of subscription
df$ratio.min_played_to_duration <- df$value.minutes_played / df$value.sub_Duration

ggplot(df, aes(x= as.factor(Churn.), y = ratio.min_played_to_duration))+
  geom_jitter(alpha=0.5, aes(color=as.factor(Churn.)), position = position_jitter(width = 0.1))+coord_flip()

#log-transform poor distributions (minutes_played, app_open, sub_duration
#df$value.log_minutes_played <- log(df$value.minutes_played)
#df$value.log_app_open <- log(df$value.app_open)
#df$value.log_sub_duration <- log(df$value.sub_Duration)

# Remove key, account_status (churn depends on this), sub_start/end, age (too many ?), country (too many categories)
df <- df[, -c(1,3,4,5,7,9)]
df <- df[, c(2:3,1,4:ncol(df))]

# Define train and test sets
smp_size <- floor(0.75 * nrow(df)) # 75% train
set.seed(28)
train_ind <- sample(seq_len(nrow(df)), size = smp_size) # random indeces 
train <-df[train_ind, ] 
test <- df[-train_ind, ]

# Correlation plot for numeric variables
train.n<-sapply(train,class)=='numeric' | sapply(train,class)=='integer' #subset numeric
trainNum<-train[,train.n]
corr<-cor(trainNum)
corrplot(corr, type = "lower", tl.pos = "ld")

# ===== RF Model Testing =====
#Modeling with random forest
#Random forest can handle only numeric variables and factors
train.n<-sapply(train,class)!='character' #subset factor and numeric
trainF<-train[,train.n]

set.seed(28)
# Model construction
randomForestModel <- randomForest(Churn. ~.,
                                  data=trainF, 
                                  importance=TRUE, 
                                  ntree=2000)

# Variable importance plot
varImpPlot(randomForestModel)

# Predictions
prediction <- predict(randomForestModel, test)

# Evaluate the model
confMatrix<-table(prediction,test$Churn.)
A<-confMatrix[4] # true positives
B<-confMatrix[2] # false negatives
C<-confMatrix[3] # false positives
D<-confMatrix[1] # true negatives

accuracy<- (A+D)/(A+B+C+D) # true / total
accuracy
specificity<-D/(B+D) # true negatives / total negatives
specificity
sensitivity<-A/(A+C) # true positives / total positives
sensitivity
precision<-A/(A+B) # true positives / predicted positives
precision
F1<-2*precision*sensitivity/(precision+sensitivity)
F1

# Rerun random forest. This time only with the 5 most important variables
set.seed(28)

randomForestModel <- randomForest(Churn. ~value.sub_Duration+value.temple+
                                    ratio.min_played_to_duration+value.completed_puzzle+value.minutes_played,
                                  data=trainF, 
                                  importance=TRUE, proximity=TRUE,
                                  ntree=2000)
varImpPlot(randomForestModel)

# Predictions
prediction <- predict(randomForestModel, test)

# Evaluate
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



# ===== Lasso Logistic Regression =====
#sub_Duration and churn
table(df$Churn.)
df_150 <- df[df$value.sub_Duration<150,]
table(df_150$Churn.)
ggplot(df_150, aes(x=value.sub_Duration, colour = as.factor(Churn.))) +
  geom_density() +
  scale_x_continuous(name="Days subscribed (N = 39365)", breaks=c(seq(0,150,by=30)), limits= c(0, 150)) +
  guides(fill=FALSE)

xtabs(~ Churn. + value.gender, data=df)

#lasso
df<-data.matrix(trainNum)
lasso<-glmnet(as.matrix(trainNum[,-1]),trainNum$Churn.)
plot.glmnet(lasso, label=T)

crossVal<-cv.glmnet(as.matrix(trainNum[,-1]),trainNum$Churn.) # 10-fold cross validation ensures the model is not over fitted
coefs<- coef(crossVal, s="lambda.1se")
coefs

#logistic regression
model0<- glm(as.factor(Churn.)~value.sub_Duration+value.completed_puzzle+value.edit_game+value.minigame+value.temple+value.snoopy+value.complete_tutorial, family=binomial(link='logit'), data=train)
summary(model0)

 # Predictions
prediction <- predict(model0, test[,-1], type="response")

# Evaluate the model
confMatrix<-table(prediction,test$Churn.)
A<-confMatrix[4] # true positives
B<-confMatrix[2] # false negatives
C<-confMatrix[3] # false positives
D<-confMatrix[1] # true negatives

accuracy<- (A+D)/(A+B+C+D) # true / total
accuracy
specificity<-D/(B+D) # true negatives / total negatives
specificity
sensitivity<-A/(A+C) # true positives / total positives
sensitivity
precision<-A/(A+B) # true positives / predicted positives
precision
F1<-2*precision*sensitivity/(precision+sensitivity)
F1



#decision tree
DataTreeM <- rpart(status~., data=train[,-13])
varImp(DataTreeM)
plot(DataTreeM)
text(DataTreeM)
predictionTreeM <- predict(DataTreeM, test)