---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Load libraries
library(data.table)
library(ggplot2)


## Loading and preprocessing the data
unzip("activity.zip")
df = read.csv("activity.csv")
df$date = as.Date(df$date)
DT <- data.table(df)


## Histogram of the total number of steps taken each day
totalSteps = aggregate(steps ~ date, df, sum)
hist(totalSteps$steps, main = "Histogram of steps", xlab = "Steps", ylab = "Count of Days")


## Mean and median number of steps taken each day
DT[, list(mean   = mean(steps),
              median = as.double(median(steps))),
         by = date]


## What is the average daily activity pattern?
i = aggregate(steps ~ interval, df, mean)
plot(i$interval, i$steps, type="l",
     main="Average number of steps taken per day", 
     xlab="Interval", ylab="Average number of steps", 
     lwd=3, col="blue")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
iMax = i[order(-i$steps),]
iMax[1,]

## Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing = is.na(df$steps)
sum(missing)

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Use mean for that 5-minute interval
meanCol <- rep(i$steps, 61)

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
dfNew = df
dfNew$steps[missing] = meanCol[missing]

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalStepsNew = aggregate(steps ~ date, dfNew, sum)
# before imput
hist(totalSteps$steps, main = "Histogram of steps", xlab = "Steps", ylab = "Count of Days")
# after impute
hist(totalStepsNew$steps, main = "Histogram of steps", xlab = "Steps", ylab = "Count of Days")

# Mean and median number of steps taken each day
DTNew <- data.table(dfNew)
DTNew[, list(mean   = mean(steps),
              median = as.double(median(steps))),
         by = date]
# The impact of imputing missing data: number and mean of steps have gone up for those days that had NA or missing data.

## Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
dfNew$wDay <- factor((weekdays(dfNew$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

# sum interval steps and wday
meanStepsWeekend = aggregate(steps ~ interval, dfNew[dfNew$wDay == "weekend",], mean)
meanStepsWeekday = aggregate(steps ~ interval, dfNew[dfNew$wDay == "weekday",], mean)

par(mfcol=c(2,1))
# plot weekend
plot(meanStepsWeekend$interval, meanStepsWeekend$steps, type="l",
     main="Average number of steps taken per day", 
     xlab="Interval", ylab="Average number of steps", 
     lwd=3, col="blue")

# plot weekday
plot(meanStepsWeekday$interval, meanStepsWeekday$steps, type="l",
    main="Average number of steps taken per day", 
    xlab="Interval", ylab="Average number of steps", 
    lwd=3, col="blue")
