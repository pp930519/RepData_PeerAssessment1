---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---


## Loading and preprocessing the data

```r
unzip("activity.zip", exdir = "activity Dataset")
files <- list.files("activity Dataset", full.names = TRUE)
dat <- read.csv("activity Dataset/activity.csv",header = TRUE)
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
date <- character()
steps_sum <- integer()
count <- 288
for (i in 0:60) {
    date[i+1] <- as.character(dat$date[i*count + 1])
    steps_sum[i+1] <- sum(dat$steps[(i*count + 1):(i*count + 288)], na.rm = TRUE)
}
dat1 <- data.frame(date = date, steps_sum = steps_sum)
ggplot(dat1, aes(x = steps_sum)) + geom_histogram() + labs(x =  "total number of steps taken per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/Q1-1.png)<!-- -->

```r
summary(dat1$steps_sum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

## What is the average daily activity pattern?

```r
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
interval <- vector()
steps_ave <- vector()
for (i in 1:length(unique(dat$interval))) {
    interval[i] <- as.character(dat$interval[i])
    steps_ave[i] <- as.integer(mean(filter(dat, dat$interval == unique(dat$interval)[i])[,1], na.rm = TRUE))
}
dat2 <- data.frame(interval = interval, steps_ave = steps_ave)
ggplot(dat2, aes(x = as.integer(as.character(interval)), y = steps_ave)) + geom_line(col = "blue")
```

![](PA1_template_files/figure-html/Q2-1.png)<!-- -->

On average across all the days in the dataset, interval at 835 contains the maximum number of steps

## Imputing missing values
The total number of missing values in the dataset is

```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```

```r
library(ggplot2)
steps_ave <-integer()
for (i in 1:length(dat1$steps_sum)) {
     steps_ave[i] <- dat1[,2][i]/61
}
dat1 <- mutate(dat1, steps_ave = as.integer(steps_ave))
dat_new <- dat
for (i in 1:length(dat_new$steps)) {
    if (is.na(dat_new[i,1]) == TRUE) {
        dat_new[i,1] <- filter(dat1, as.character(dat1[,1]) == as.character(dat_new[i,2]))[,3]
    } 
}
date <- character()
steps_sum <- integer()
count <- 288
for (i in 0:60) {
    date[i+1] <- as.character(dat_new$date[i*count + 1])
    steps_sum[i+1] <- sum(dat_new$steps[(i*count + 1):(i*count + 288)], na.rm = TRUE)
}
dat_new1 <- data.frame(date = date, steps_sum = steps_sum)
ggplot(dat_new1, aes(x = steps_sum)) + geom_histogram() + labs(x =  "total number of steps taken per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/buildnewdataset-1.png)<!-- -->

```r
summary(dat_new1$steps_sum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
library(dplyr)
weekdays <- vector()
for (i in 1:length(dat_new$date)) {
     if (weekdays(as.Date(dat_new$date[i])) == "Saturday" | weekdays(as.Date(dat_new$date[i])) == "Sunday") {
         weekdays[i] <- "weekend"
     } else {
         weekdays[i] <- "weekdays"
     }
}
dat_new <- mutate(dat_new, weekdays = weekdays)
dat3 <- aggregate(steps ~ interval + weekdays, dat_new, mean)
g <- ggplot(dat3, aes(x = interval, y =  steps, color = weekdays))
g + geom_line() + facet_grid(weekdays ~ .) + labs(y = "Number of steps")
```

![](PA1_template_files/figure-html/Q4answer-1.png)<!-- -->
