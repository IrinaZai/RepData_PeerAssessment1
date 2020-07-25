---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE, error = FALSE, warning = FALSE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```


## Loading and preprocessing the data

```r
unzip("activity.zip")
activitydata <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.  

1. Calculate the total number of steps taken per day  

```r
total_steps <- activitydata %>%
                        group_by(date) %>%
                        summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  

```r
steps_plot <- ggplot(total_steps, aes(x = steps))
steps_plot + geom_histogram(fill = "coral", binwidth = 2000) +
             labs(x = "Steps", y = "Frequency", title = "Steps per day") +
             theme_bw()
```

![](PA1_template_files/figure-html/hist_steps_per_day-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day  

```r
options(scipen = 6) 
mean_steps <- round(mean(total_steps$steps, na.rm = TRUE), 2)
median_steps <- round(median(total_steps$steps, na.rm = TRUE), 2)
```
Mean steps per day is equal to 10766.19. 
Median steps per day is equal to 10765.


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l"") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_interval <- activitydata %>%
                  group_by(interval) %>%
                  summarise(steps = mean(steps, na.rm = TRUE)) %>%
                  rename(steps_avg = steps)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with (steps_interval, {
        plot(interval, steps_avg, 
             type = "l",
             xlab = "Time interval",
             ylab = "Average number of steps",
             main = "The average number of steps taken in each 5-minute interval")
})
```

![](PA1_template_files/figure-html/agg_steps_interval-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- steps_interval[which.max(steps_interval$steps_avg), 1]
```
Interval 835 contains the maximum number of steps on average across all the days in the dataset.


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```
Number of missing values is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I am going to fill the missing values with the average steps per interval that were estimated in the previous task. I am going to merge the two datasets, and then replace missing values with the average steps per interval, and then will delete the column with average steps.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activitydata_rmna <- merge(activitydata, steps_interval, by = "interval")
activitydata_rmna$steps <- with(activitydata_rmna, ifelse(is.na(steps), steps_avg, steps))
activitydata_rmna$steps_avg <- NULL
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps_rmna <- activitydata_rmna %>%
                        group_by(date) %>%
                        summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
steps_plot_rmna <- ggplot(total_steps, aes(x = steps))
steps_plot_rmna + geom_histogram(fill = "navy", binwidth = 2000) +
                  labs(x = "Steps", y = "Frequency", title = "Steps per day") +
                  theme_bw()
```

![](PA1_template_files/figure-html/hist_rmna-1.png)<!-- -->


```r
mean_steps_rmna <- round(mean(total_steps_rmna$steps, na.rm = TRUE), 2)
median_steps_rmna <- round(median(total_steps_rmna$steps, na.rm = TRUE), 2)
```
Mean steps per day is equal to 10766.19 after imputting missing data. It is equal to the mean steps before imputing the missing data.  
Median steps per day is equal to 10766.19 after imputting missing data. It is greater than the median steps before imputing the missing data.  


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activitydata_rmna$date <- as.POSIXct(activitydata_rmna$date, format = "%Y-%m-%d")
activitydata_rmna$wday <- weekdays(activitydata_rmna$date)
activitydata_rmna[grepl("Monday|Tuesday|Wednesday|Thursday|Friday", activitydata_rmna$wday), "Day"] <- "Weekday"
activitydata_rmna[grepl("Saturday|Sunday", activitydata_rmna$wday), "Day"] <- "Weekend"
activitydata_rmna$Day <- as.factor(activitydata_rmna$Day)
activitydata_rmna$wday <- NULL
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
steps_interval_wday <- activitydata_rmna %>%
                        group_by(Day, interval) %>%
                        summarise(steps = mean(steps))
```

```
## `summarise()` regrouping output by 'Day' (override with `.groups` argument)
```

```r
plot_steps_wday <- ggplot(steps_interval_wday, aes(x = interval, y = steps, col = Day))
plot_steps_wday + geom_line() +
             facet_grid(Day ~ .) +
             labs(x = "Time interval", y = "Average number of steps",
                  title = "The average number of steps taken in each 5-minute interval") +
             theme_bw()
```

![](PA1_template_files/figure-html/weekend_weekday_plots-1.png)<!-- -->
