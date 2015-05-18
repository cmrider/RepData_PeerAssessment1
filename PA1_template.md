---
title: "PA1_template"
author: "Chelsea Rider"
date: "Sunday, May 17, 2015"
output: html_document
---
## Load and Preprocess the Data:

```r
activity<-read.csv("C:/Users/Chelsea/Desktop/repdata_data_activity/activity.csv")

activity$date<-as.POSIXct(activity$date, format="%Y-%m-%d")

activity<-data.frame(date=activity$date, 
                     weekday=tolower(weekdays(activity$date)), 
                     steps=activity$steps, 
                     interval=activity$interval)

activity<- cbind(activity, 
                 daytype=ifelse(activity$weekday == "saturday" | 
                                activity$weekday == "sunday", "weekend", 
                                "weekday"))

activity <- data.frame(date=activity$date, 
                       weekday=activity$weekday, 
                       daytype=activity$daytype, 
                       interval=activity$interval,
                       steps=activity$steps)
```
## What is the mean total number of steps taken per day?

Calculate the total number of stpes taken per day.

```r
daily <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(daily)<-c("date", "total")
# "total" is the total number of steps taken per day
```

Make a histogram of the total number of steps taken each day.

```r
hist(daily$total, main="Histogram of Total Number of Steps per Day", 
     xlab="Daily Step Total", 
     ylab="Frequency of Totals", ylim=c(0, 30))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(daily$total)
```

```
## [1] 9354.23
```

```r
# [1] 9354.23
median(daily$total)
```

```
## [1] 10395
```

```r
# [1] 10395
```

## What is the average daily activity pattern?

Make a time-series plot of the 5-minute interval and the average number of steps taken

```r
pattern<-aggregate(activity$steps,
      by=list(activity$interval),
      FUN=mean,
      na.rm=TRUE)
names(pattern)<-c("interval", "mean")
plot(pattern$interval, pattern$mean,
     type="l",
     main="Time-Series Plot of the Average Number of Steps Taken by Interval",
     xlab="Interval in 5-minute Increments",
     ylab="Average No. of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Which 5-minute interval contains the maximum number of steps?

```r
max<-which(pattern$mean == max(pattern$mean))
interval<-pattern[max, 1]
print(interval)
```

```
## [1] 835
```

```r
# [1] 835
```

## Inputing missing values.

Calculate and report the total number of missing values in the dataset.

```r
na<-sum(is.na(activity$steps))
print(na)
```

```
## [1] 2304
```

```r
# [1] 2304
```

Fill in the missing values and create a new dataset with missing data filled in

```r
na_obvs<-which(is.na(activity$steps))

mean_obvs<-rep(mean(activity$steps, na.rm=TRUE), times=length(na_obvs))

activity[na_obvs, "steps"]<-mean_obvs
```

Make a histogram of the total number of steps taken each day

```r
sum<-aggregate(activity$steps, by=list(activity$date), FUN=sum)
names(sum)<-c("date", "total")

hist(sum$total, 
     xlab="Total Number of Steps", 
     ylab="Frequency of Days",
     ylim=c(0,40),
     main="Histogram of the Total Number of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Calculate and report the mean and median total number of steps taken each day

```r
mean(sum$total)
```

```
## [1] 10766.19
```

```r
# [1] 10766.19
median(sum$total)
```

```
## [1] 10766.19
```

```r
# [1] 10766.19
```

## Are there differences in activity data between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend"

```r
activity$date<-as.POSIXct(activity$date, format="%Y-%m-%d")

activity <- data.frame(date=activity$date, 
                       weekday=tolower(weekdays(activity$date)), 
                       steps=activity$steps, 
                       interval=activity$interval)

activity <- cbind(activity, 
                  daytype=ifelse(activity$weekday == "saturday" | 
                                 activity$weekday == "sunday", "weekend", 
                                 "weekday"))

activity <- data.frame(date=activity$date, 
                       weekday=activity$weekday, 
                       daytype=activity$daytype, 
                       interval=activity$interval,
                       steps=activity$steps)

mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                       activity$weekday, activity$interval), mean)

names(mean_data)<-c("daytype", "weekday", "interval", "mean")
```

Make a panel plot containg a time-series plot of the 5-minute intervals and average number of steps taken, averaged across all weekdays or weekend days.

```r
library(lattice)

xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
