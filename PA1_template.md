---
title: "Reproducible Research: Peer Assessment 1"
author: "Nicolás E. Kuzminski"
date: "26 de octubre de 2018"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              "activity.zip", method="curl")

unzip("activity.zip", overwrite = TRUE)

activity <- read.csv("activity.csv", colClasses = c("integer","Date","integer"))

str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```



## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsday <- aggregate(steps ~ date, data = activity, sum)
head(stepsday)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
hist(stepsday$steps)
```

![](PA1_template_files/figure-html/stepsnumber-1.png)<!-- -->

```r
mean(stepsday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsday$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgstepsinterv <- aggregate(steps ~ interval, data = activity, mean)
head(avgstepsinterv)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(avgstepsinterv$interval, avgstepsinterv$steps, type="l")
```

![](PA1_template_files/figure-html/dailypattern-1.png)<!-- -->

```r
avgstepsinterv[which.max(avgstepsinterv$steps), "interval"]
```

```
## [1] 835
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
nas <- which( is.na(activity$steps) )
length(nas)
```

```
## [1] 2304
```

```r
newactivity <- activity
for (i in nas) {
  newactivity[i, "steps"] <- avgstepsinterv[avgstepsinterv$interval == activity[i, "interval"], "steps"]
} 

newstepsday <- aggregate(steps ~ date, data = newactivity, sum)
hist(newstepsday$steps)
```

![](PA1_template_files/figure-html/missingvalues-1.png)<!-- -->

```r
mean(newstepsday$steps)
```

```
## [1] 10766.19
```

```r
median(newstepsday$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
newactivity$daytype <- factor(
  ifelse(weekdays(newactivity$date) %in% c("saturday","sunday","sábado","domingo"), "weekend", "weekday"))

par(mfrow=c(2,1),mar=c(4,4,0,4))
xr <- range(newactivity$interval) 
yr <- range(avgstepsinterv$steps)

avgstepswend <- aggregate(steps ~ interval, data = newactivity[newactivity$daytype == "weekend",], mean)
with(avgstepswend, plot(interval, steps, type="l", xlim=xr, ylim=yr))
legend("topleft", "weekend")

avgstepswday <- aggregate(steps ~ interval, data = newactivity[newactivity$daytype == "weekday",], mean)
with(avgstepswday, plot(interval, steps, type="l", xlim=xr, ylim=yr))      
legend("topleft", "weekday")
```

![](PA1_template_files/figure-html/wdayswend-1.png)<!-- -->
