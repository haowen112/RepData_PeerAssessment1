---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip", "activity.csv")
measurements <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
* histogram of total number of steps taken per day

```r
steps_per_day <- aggregate(steps~date, measurements, sum)
hist(steps_per_day$steps, main = "Steps taken per day", xlab="Steps", ylab = "days")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

* Calculate mean and median number of steps taken per day

```r
mean_steps_per_day <- mean(steps_per_day$steps)
median_steps_per_day <- median(steps_per_day$steps)
```

The mean total number of steps taken per day is 1.0766189\times 10^{4}. \
The median total number of steps taken per day is 10765.

## What is the average daily activity pattern?

* Time series plot

```r
average_daily_activity <- aggregate(steps~interval, measurements, mean)
plot(steps~interval, average_daily_activity, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

* Calculate the maximum number of steps on average across all the days in 5 minutes interval

```r
max_5_minute_interval_steps <- average_daily_activity[which.max(average_daily_activity$steps), ]$interval
```

The 5 minutes interval with maximum number of steps across all the days is 835.

## Imputing missing values
* Calculate total number of missing values

```r
total_missing <- sum(is.na(measurements))
```

Total number of missing values is 2304

* Fill in the missing values with mean for that 5-minute interval in a new data set 

```r
find_mean_steps <- function(interval){
  average_daily_activity[average_daily_activity$interval == interval, ]$steps
}

new_measurements <- measurements
for(i in 1:nrow(new_measurements)){
  if(is.na(new_measurements[i,]$steps)){
    new_measurements[i,]$steps <- find_mean_steps(new_measurements[i,]$interval)
  }
}
```

* Histogram of new measurement with NA value filled by mean steps in 5-minute interval

```r
new_steps_per_day <- aggregate(steps~date, new_measurements, sum)
hist(new_steps_per_day$steps, main = "Steps taken per day with NA value filled", xlab="Steps", ylab = "days")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

* Calculate mean and median number of steps taken per day

```r
new_mean_steps_per_day <- mean(new_steps_per_day$steps)
new_median_steps_per_day <- median(new_steps_per_day$steps)
```

The new mean total number of steps taken per day is 1.0766189\times 10^{4}. \
The new median total number of steps taken per day is 1.0766189\times 10^{4}.

The impact of of imputing missing data is seen in the increased median and also increased total number of steps in days.

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable with two levels - "weekday" and "weekend"

```r
new_measurements$day_type <- weekdays(as.Date(new_measurements$date))
new_measurements$day_type[new_measurements$day_type %in% c("Saturday", "Sunday")] <- "Weekend"
new_measurements$day_type[new_measurements$day_type != "Weekend"] <- "Weekday"

new_measurements$day_type <- factor(new_measurements$day_type, levels = c("Weekday", "Weekend"))

new_average_daily_activity <- aggregate(steps~interval+day_type, new_measurements, mean)
library(lattice)
xyplot(steps~interval|factor(day_type), new_average_daily_activity, aspect=1/2,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


