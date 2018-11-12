# codeSpark NBD/Pareto 
# revised from emarkou on github https://github.com/blendo-app/NBD-Pareto-churn-model

######################
#prerequisite: Load Data in variable 'data' as an event log of the form:
# <email_address> | <open action timestamp>
#------------------------------------------
# aaaaa@gg.com   | 2013-03-02
# bbbbb@hh.com  |  2011-02-04
#####################

library(reshape)
library(BTYD)
library(ggplot2)
library(plyr)
library(lubridate)
library(magrittr)

# Import the csv file  
data <- read.table("./Data/NBDpareto.csv", fill = TRUE, header = TRUE, sep = ",", na.strings = c("NA","")) %>% # 605298 2
  arrange(uid, open_time)

# Order by user and time
data <- data[order(data$uid, data$open_time),]

# Convert time in ms to dates by day
data$open_time <- format(as.POSIXct(data$open_time / 1000, origin = "1970-01-01", tz = "America/Los_Angeles"), "%Y%m%d")
data$open_time <- as.Date(data$open_time, "%Y%m%d")

#assign ids to users  
users <- unique(data$uid) %>% 
  as.data.frame() %>% 
  mutate(id = seq.int(length(unique(data$uid))))
names(users)[1] <- "uid"
data <- merge(data, users, by = "uid")

# NA Values
sapply(data, function(y) sum(length(which(is.na(y))))) # 1752 values all between 8-23 and 8-28 (10% of that week. Perhaps an error that week?)
# I will remove, but probably should impute with some sort RF
data <- data[-which(is.na(data$uid)),] # 603546      3

names(data)[2] <- "date"

count(data$id)
count(data$date)
# Clean outliers
which(count(data$id)["freq"]>10000) # some user has many AppOpen events
count1 <- count(data$id)[-34060,]
data1 <- data[which(data$id != 34060),] # remove this power user
summary(count1)
summary(count(data1$date))
ggplot(count1,aes(x=x, y=freq))+
  geom_point(colour="black")+
  ylab("AppOpen Frequency")+
  xlab("User id")+
  theme_minimal()

ggplot(count(data1$date),aes(x=x, y=freq))+
  geom_point(colour="black")+
  ylab("AppOpen Frequency")+
  xlab("Date")+
  geom_smooth(method = 'lm' )+
  theme_minimal()

elog <- data[, c("id", "date")]
names(elog)[1] <- "cust"
write.csv(elog, "./Data/elog_No-NA_Raw.csv")
elogM <- dc.MergeTransactionsOnSameDate(elog)
write.csv(elogM, "./Data/elog_No-NA_Merge.csv")

# ===== Load from csv =====
elog <- read.csv("./Data/elog_No-NA_Merge.csv", row.names = 1)
elog <- read.csv("./Data/elog_No-NA_Raw.csv", row.names = 1)
head(elog)
summary(elog)  # no NAs


# exploring data
purchaseFreq <- ddply(elog, .(cust), summarize, 
                      daysBetween = as.numeric(diff(date)))

ggplot(purchaseFreq,aes(x=daysBetween))+
  geom_histogram(fill="orange")+
  xlab("Time between opens (days)")+
  theme_minimal()

elog$date<-as.POSIXct(elog$date)
# splitting data
(end.of.cal.period <- min(elog$date)+(max(elog$date)-min(elog$date))/2)
data2 <- dc.ElogToCbsCbt(elog, per="day", 
                        T.cal=end.of.cal.period,
                        statistic = "freq") 
cal2.cbs <- as.matrix(data2[[1]][[1]])

data3 <- dc.ElogToCbsCbt(elog, per="day", 
                        T.cal=end.of.cal.period,
                        T.tot=max(elog$date),
                        statistic = "freq") 
cal2.cbs <- as.matrix(data3[[1]][[1]])
str(cal2.cbs)

#Parameter estimation

(params2 <- pnbd.EstimateParameters(cal2.cbs))
(LL <- pnbd.cbs.LL(params2, cal2.cbs))
# it is a good idea to make a few more estimates to see if they converge
p.matrix <- c(params2, LL)
for (i in 1:5) {
  params2 <- pnbd.EstimateParameters(cal2.cbs, params2)
  LL <- pnbd.cbs.LL(params2, cal2.cbs)
  p.matrix.row <- c(params2, LL)
  p.matrix <- rbind(p.matrix, p.matrix.row)
}

p.matrix
(params2 <- p.matrix[dim(p.matrix)[1],1:4])

param.names <- c("r", "alpha", "s", "beta")

LL <- pnbd.cbs.LL(params2, cal2.cbs)
#contour plots
dc.PlotLogLikelihoodContours(pnbd.cbs.LL, params2, cal.cbs = cal2.cbs , n.divs = 5,
                             num.contour.lines = 7, zoom.percent = 0.3,
                             allow.neg.params = FALSE, param.names = param.names)
#heterogeneity of open
pnbd.PlotTransactionRateHeterogeneity(params2, lim = NULL)
#heterogeneity of drop out
pnbd.PlotDropoutRateHeterogeneity(params2)

#individual predicitions - 2 (or 60 days) month period - new customer
pnbd.Expectation(params2, t = 60) # 5.462855 days with AppOpen events

#individual predictions - 2 month period - existing customer
cal2.cbs["42",]
x <- cal2.cbs["2003", "x"]         
t.x <- cal2.cbs["2003", "t.x"]     
T.cal <- cal2.cbs["2003", "T.cal"]
pnbd.ConditionalExpectedTransactions(params2, T.star = 60, 
                                     x, t.x, T.cal)

#probabilities of customers being alive

x          
t.x        
#end of calibration
T.cal <- 61.5
pnbd.PAlive(params2, x, t.x, T.cal)
params3 <- pnbd.EstimateParameters(cal2.cbs)
p.alives <- pnbd.PAlive(params3, cal2.cbs[,"x"], cal2.cbs[,"t.x"], cal2.cbs[,"T.cal"])
ggplot(as.data.frame(p.alives),aes(x=p.alives))+
  geom_histogram(colour="grey",fill="orange")+
  ylab("Number of Customers")+
  xlab("Probability Customer is 'Live'")+
  theme_minimal()
pnbd.PlotFrequencyInCalibration(params2, cal2.cbs, 
                                censor=10, title="Model vs. Reality during Calibration")
#assess model in holdout period
x.star   <- data3[[2]][[2]][,1]
cal2.cbs <- cbind(cal2.cbs, x.star)
str(cal2.cbs)

holdoutdates <- attributes(data3[[2]][[1]])[[2]][[2]]
holdoutlength <- round(as.numeric(max(as.Date(holdoutdates))-
                                    min(as.Date(holdoutdates)))/7)

T.star <- holdoutlength
censor <- 7 
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params2, T.star,
                                                    cal2.cbs, x.star, censor)
