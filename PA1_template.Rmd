---
title: "Reproducible Research"
author: "Nishant Sisingi"
date: "October 5, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Loading and preprocessing the data
Load Packages
```{r, message=FALSE}
library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)
```

### 1.Code for reading in the dataset and/or processing the data
Read data
Convert date to POSIXt
```{r}
##Read Data
activity_data <- read.csv("activity.csv")
activity_data <- as_tibble(activity_data)
activity_data$date <- ymd(activity_data$date)
activity_data
```

#What is mean total number of steps taken per day?

Calculate the total number of steps taken per day.

```{r}
daily_steps <- activity_data %>% group_by(date) %>% summarise(tot = sum(steps,na.rm=T))
daily_steps
```

Make a histogram of the total number of steps taken each day.

### 2.Histogram of the total number of steps taken each day
```{r, message=FALSE}
ggplot(daily_steps) + geom_histogram(aes(x=tot)) + ggtitle("Total steps taken each day")
```

Calculate and report the mean and median of the total number of steps taken per day.

### 3a.Mean and median number of steps taken each day
Mean
```{r}
mean(daily_steps$tot)
```

Median
```{r}
median(daily_steps$tot)
```

#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r}
interval_steps <- activity_data %>% group_by(interval) %>% summarise(avg = mean(steps,na.rm=T))
interval_steps
```

### 4.Time series plot of the average number of steps taken
```{r}
ggplot(interval_steps) + geom_line(aes(x=interval,y=avg)) + ggtitle("Avg steps taken each day")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?.

### 5.The 5-minute interval that, on average, contains the maximum number of steps
```{r}
interval_steps %>% filter(avg == max(avg))
```
The highest count of steps is 206 in interval 835

#Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).
```{r}
sum(is.na(activity_data$steps))
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

### 6.Code to describe and show a strategy for imputing missing data
Simple technique - Imputing missing data with zeroes. 
The activity_data is subset to obtain the positions for which the NA's exist and overwrite them with zeroes.
```{r}
new <- activity_data
new$steps[is.na(new$steps)]<-0
```
```{r}
new_total_steps <- new %>% group_by(date) %>% summarise(tot = sum(steps))
new_total_steps
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### 7.Histogram of the total number of steps taken each day after missing values are imputed   
```{r, message=FALSE}
ggplot(new_total_steps) + geom_histogram(aes(x=tot)) + ggtitle("Total no. of steps taken each day")
```

### 3b.Mean and median number of steps taken each day
Mean
```{r}
mean(new_total_steps$tot)
```

Median
```{r}
median(new_total_steps$tot)
```
The mean and the median remain unchanged at 9354 and 10395 respectively.

#Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
activity_pattern <- activity_data %>% mutate(wtype = ifelse(wday(date)==1 | wday(date)==7,"weekend","weekday"))
activity_pattern
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

### 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
activity_pattern %>% group_by(interval,wtype) %>% summarise(avg = mean(steps,na.rm=T)) %>%
ggplot(aes(x=interval,y=avg)) + geom_line()+ geom_smooth() + facet_wrap(~wtype,ncol = 1,nrow = 2) + ggtitle("Weekday vs Weekend Activity Comparisions")
```

From the two plots it seems that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays.

### 9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report - YES





























