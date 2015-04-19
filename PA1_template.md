output: html_document
knit: my_render

# Reproducible Research: Peer Assessment 1   

## Data   
The data is from a personal activity monitoring device such as a Fitbit, Nike Fuelband, or Jawbone Up. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Dataset   
Dataset: [Activity monitoring data [Zip file, 52K]](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data      

### 1. Load the data      

```r
## rm(list=ls())
## setwd("/Users/rogerfischer/datasciencecoursera/repdata/RepData_PeerAssessment1")
activity <- unzip("activity.zip")
activity <- read.csv(activity, stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

### 2. Process/transform the data into a format suitable for the analysis     
Date should be of class date, not character

```r
class(activity$date)
```

```
## [1] "character"
```

```r
activity$date <- strptime(activity$date, "%Y-%m-%d")
class(activity$date)
```

```
## [1] "POSIXlt" "POSIXt"
```

```r
order_by_date <- order(activity$date)
activity <- activity[order_by_date,]
```


## What is mean total number of steps taken per day?    
We ignore the missing values in the dataset.

### 1. Calculate the total number of steps taken per day    

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity$date <- as.character(activity$date)
by_day <- group_by(activity, date)
steps_by_day <- summarise(by_day, steps = sum(steps))

# View(steps_by_day)
head(steps_by_day)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
tail(steps_by_day)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
## 1 2012-11-25 11834
## 2 2012-11-26 11162
## 3 2012-11-27 13646
## 4 2012-11-28 10183
## 5 2012-11-29  7047
## 6 2012-11-30    NA
```

### 2. Make a histogram of the total number of steps taken each day   

```r
steps_only <- as.matrix(steps_by_day[, 2])
hist(steps_only)
```

![plot of chunk histogram](figure/histogram-1.png) 

### 3. Calculate and report the mean and median of the total number of steps    taken per day

```r
mean_steps <- apply(steps_by_day[, 2], 2 , mean, na.rm=TRUE)
mean_steps
```

```
##    steps 
## 10766.19
```

```r
median_steps <- apply(steps_by_day[, 2], 2 , median, na.rm=TRUE)
median_steps
```

```
## steps 
## 10765
```

## What is the average daily activity pattern?   

### 1. Make a time series plot    
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

### 2. Maximum number of steps interval    
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


## Imputing missing values   



## Are there differences in activity patterns between weekdays and weekends?   
