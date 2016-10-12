# Reproducible Research
Nishant Sisingi  
October 5, 2016  



#Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Loading and preprocessing the data
Load Packages

```r
library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)
```

### 1.Code for reading in the dataset and/or processing the data
Read data
Convert date to POSIXt

```r
##Read Data
activity_data <- read.csv("activity.csv")
activity_data <- as_tibble(activity_data)
activity_data$date <- ymd(activity_data$date)
activity_data
```

```
## # A tibble: 17,568 × 3
##    steps       date interval
##    <int>     <date>    <int>
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

#What is mean total number of steps taken per day?

Calculate the total number of steps taken per day.


```r
daily_steps <- activity_data %>% group_by(date) %>% summarise(tot = sum(steps,na.rm=T))
daily_steps
```

```
## # A tibble: 61 × 2
##          date   tot
##        <date> <int>
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## # ... with 51 more rows
```

Make a histogram of the total number of steps taken each day.

### 2.Histogram of the total number of steps taken each day

```r
ggplot(daily_steps) + geom_histogram(aes(x=tot)) + ggtitle("Total steps taken each day")
```

![](Reproducible_Research_I_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day.

### 3a.Mean and median number of steps taken each day
Mean

```r
mean(daily_steps$tot)
```

```
## [1] 9354.23
```

Median

```r
median(daily_steps$tot)
```

```
## [1] 10395
```

#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
interval_steps <- activity_data %>% group_by(interval) %>% summarise(avg = mean(steps,na.rm=T))
interval_steps
```

```
## # A tibble: 288 × 2
##    interval       avg
##       <int>     <dbl>
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## # ... with 278 more rows
```

### 4.Time series plot of the average number of steps taken

```r
ggplot(interval_steps) + geom_line(aes(x=interval,y=avg)) + ggtitle("Avg steps taken each day")
```

![](Reproducible_Research_I_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?.

### 5.The 5-minute interval that, on average, contains the maximum number of steps

```r
interval_steps %>% filter(avg == max(avg))
```

```
## # A tibble: 1 × 2
##   interval      avg
##      <int>    <dbl>
## 1      835 206.1698
```
The highest count of steps is 206 in interval 835

#Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

### 6.Code to describe and show a strategy for imputing missing data
Simple technique - Imputing missing data with zeroes. 
The activity_data is subset to obtain the positions for which the NA's exist and overwrite them with zeroes.

```r
new <- activity_data
new$steps[is.na(new$steps)]<-0
```

```r
new_total_steps <- new %>% group_by(date) %>% summarise(tot = sum(steps))
new_total_steps
```

```
## # A tibble: 61 × 2
##          date   tot
##        <date> <dbl>
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## # ... with 51 more rows
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### 7.Histogram of the total number of steps taken each day after missing values are imputed   

```r
ggplot(new_total_steps) + geom_histogram(aes(x=tot)) + ggtitle("Total no. of steps taken each day")
```

![](Reproducible_Research_I_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

### 3b.Mean and median number of steps taken each day
Mean

```r
mean(new_total_steps$tot)
```

```
## [1] 9354.23
```

Median

```r
median(new_total_steps$tot)
```

```
## [1] 10395
```
The mean and the median remain unchanged at 9354 and 10395 respectively.

#Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_pattern <- activity_data %>% mutate(wtype = ifelse(wday(date)==1 | wday(date)==7,"weekend","weekday"))
activity_pattern
```

```
## # A tibble: 17,568 × 4
##    steps       date interval   wtype
##    <int>     <date>    <int>   <chr>
## 1     NA 2012-10-01        0 weekday
## 2     NA 2012-10-01        5 weekday
## 3     NA 2012-10-01       10 weekday
## 4     NA 2012-10-01       15 weekday
## 5     NA 2012-10-01       20 weekday
## 6     NA 2012-10-01       25 weekday
## 7     NA 2012-10-01       30 weekday
## 8     NA 2012-10-01       35 weekday
## 9     NA 2012-10-01       40 weekday
## 10    NA 2012-10-01       45 weekday
## # ... with 17,558 more rows
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

### 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activity_pattern %>% group_by(interval,wtype) %>% summarise(avg = mean(steps,na.rm=T)) %>%
ggplot(aes(x=interval,y=avg)) + geom_line()+ geom_smooth() + facet_wrap(~wtype,ncol = 1,nrow = 2) + ggtitle("Weekday vs Weekend Activity Comparisions")
```

![](Reproducible_Research_I_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

From the two plots it seems that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays.

### 9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report - YES





























