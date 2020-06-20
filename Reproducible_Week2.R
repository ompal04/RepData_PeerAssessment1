## Load the data
unzip(zipfile="activity.zip")
activity <- read.csv("activity.csv")

library(lubridate)
    
## Determine day of the week and categorize as weekend or weekday
activity$date <- ymd(activity$date)
activity$weekend <- as.factor(ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday","weekend","weekday"))
activity$dayofweek <- as.factor(weekdays(activity$date))

library(dplyr)
library(ggplot2)

## Generate histogram for total number of steps taken for each day
stepsByDay <- activity %>% group_by(date) %>% summarise(stepsperday = sum(steps,na.rm = TRUE))
png(filename = "P1.png")
qplot(stepsperday,data=stepsByDay,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
dev.off()

## Calculate mean and median steps taken for each day
meanstepsperday <- stepsByDay %>% summarise(average = mean(stepsperday,na.rm = TRUE),median=median(stepsperday,na.rm = TRUE))
meanstepsperday

## Plot average daily activity pattern
interval_average <- activity %>% group_by(interval) %>% summarise(average = mean(steps,na.rm = TRUE))
png(filename = "P2.png")
qplot(interval,average,data=interval_average,geom="line",xlab = "5-minute intervals",ylab = "Average steps taken across all days")
dev.off()

## Calculate interval which has max number of steps on average
interval_average[which.max(interval_average$average),]

## Handling missing values
## Dataset with no NAs
activity_no_NA <- activity[which(!is.na(activity$steps)),]
## Mean steps for each interval
interval_only <- activity_no_NA %>% group_by(interval) %>% summarise(average=mean(steps))
## Average to integer
interval_only$average <- as.integer(interval_only$average)
## Dataset where steps have NAs
activity_na <- activity[which(is.na(activity$steps)),]
## Replace NAs with average steps
activity_na$steps <- ifelse(activity_na$interval==interval_only$interval,interval_only$average)
## Binding data w/o NAs and data that had NAs 
activity_impute <- rbind(activity_no_NA,activity_na)
## Calculate number of missing values
nrow(activity_na)

## Plot steps taken per day after missing values handled
stepsByDay_impute <- activity_impute %>% group_by(date) %>% summarise(stepsperday = sum(steps))
png(filename = "P3.png")
qplot(stepsperday,data=stepsByDay_impute,na.rm=TRUE,binwidth=500,xlab='Total steps per day', ylab='Frequency using binwith 500',main = 'Histogram of the total number of steps taken each day')
dev.off()
## Mean and median after missing handling
totalstepsperday_impute <- activity_impute %>% group_by(date) %>% summarise(stepsperday = sum(steps))
mean_n_median <- totalstepsperday_impute %>% summarise(average=mean(stepsperday),median=median(stepsperday))
mean_n_median

## Comparing patterns for weekdays and weekends
meansteps <- activity_impute %>% group_by(interval,weekend) %>%   summarise(average = mean(steps))
png(filename = "P4.png")
qplot(interval,average,data=meansteps,geom="line",facets=weekend~.,xlab="5-minute interval",ylab="average number of steps",main="Average steps pattern between Weekday and Weekend")
dev.off()

