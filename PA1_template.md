Load data
=========

if (!file.exists(‘activity.csv’)) {

unzip(‘./repdata\_data\_activity.zip’, exdir = ‘.’); }

data \<- read.csv(‘activity.csv’);

What is mean total number of steps taken per day?
=================================================

Histogram of total number of steps taken per day
================================================

steps\_by\_day \<- aggregate(steps \~ date, data, sum)
hist(steps\_by\_day\$steps, main = paste(“Total Steps Each Day”),
col=“green”,xlab=“Number of Steps”)

Mean and median number of steps taken each day
==============================================

rmean \<- mean(steps\_by\_day\$steps) rmean

rmedian \<- median(steps\_by\_day\$steps) rmedian

Time series plot of the average number of steps taken
=====================================================

steps\_by\_interval \<- aggregate(steps \~ interval, data, mean)
plot(steps\_by\_interval\\(interval,steps\_by\_interval\\)steps,
type=“l”, xlab=“Interval”, ylab=“Number of Steps”,main=“Average Number
of Steps per Day by Interval”)

The 5-minute interval that, on average, contains the maximum number of steps
============================================================================

max\_interval \<-
steps\_by\_interval[which.max(steps\_by\_interval\$steps),1]
max\_interval

Code to describe and show a strategy for imputing missing data
==============================================================

NATotal \<- sum(!complete.cases(data)) NATotal

StepsAverage \<- aggregate(steps \~ interval, data = data, FUN = mean)
fillNA \<- numeric() for (i in 1:nrow(data)) { obs \<- data[i, ] if
(is.na(obs\\(steps)) { steps \<- subset(StepsAverage, interval ==
obs\\)interval)\\(steps } else { steps \<- obs\\)steps } fillNA \<-
c(fillNA, steps) }

new\_activity \<- data new\_activity\$steps \<- fillNA

Histogram of the total number of steps taken each day after missing values are imputed
======================================================================================

StepsTotalUnion \<- aggregate(steps \~ date, data = new\_activity, sum,
na.rm = TRUE) hist(StepsTotalUnion\\(steps, main = paste("Total Steps
Each Day"), col="red", xlab="Number of Steps") \#Create Histogram to
show difference. hist(steps\_by\_day\\)steps, main = paste(“Total Steps
Each Day”), col=“blue”, xlab=“Number of Steps”, add=T)
legend(“topright”, c(“Imputed”, “Non-imputed”), col=c(“red”, “blue”),
lwd=10)

\#average number of steps taken per 5-minute interval across weekdays
and weekends

rmeantotal \<- mean(StepsTotalUnion\$steps) rmeantotal

rmediantotal \<- median(StepsTotalUnion\$steps) rmediantotal

rmediandiff \<- rmediantotal - rmedian rmediandiff

rmeandiff \<- rmeantotal - rmean rmeandiff

\#plot comparing the average number of steps taken per 5-minute interval
across weekdays and weekends

weekdays \<- c(“Monday”, “Tuesday”, “Wednesday”, “Thursday”, “Friday”)
new\_activity\\(dow =
as.factor(ifelse(is.element(weekdays(as.Date(new\_activity\\)date)),weekdays),
“Weekday”, “Weekend”)) StepsTotalUnion \<- aggregate(steps \~ interval +
dow, new\_activity, mean) library(lattice)
xyplot(StepsTotalUnion\\(steps \~
StepsTotalUnion\\)interval|StepsTotalUnion\$dow, main=“Average Steps per
Day by Interval”,xlab=“Interval”, ylab=“Steps”,layout=c(1,2), type=“l”)
