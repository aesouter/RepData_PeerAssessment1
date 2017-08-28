# Reproducible Research Assignment 1
Alana Souter  
26 August 2017  

# Reproducible Research Assignment 1

## Prepare Environment


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.1
```

```r
library(knitr)
opts_chunk$set(echo=TRUE, results='hold')
```

## Loading and Preprocessing the Data

1. Load the Data


```r
Data <- read.csv('activity.csv', header=TRUE, sep=",",colClasses=c("numeric","character","numeric"))
```

2. Tidying the Data


```r
Data$date<-as.Date(Data$date,format="%Y-%m-%d")
Data$interval<-as.factor(Data$interval)
str(Data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is the mean total number of steps taken per day?


```r
StepsPerDay<-aggregate(steps~date,Data,sum)
colnames(StepsPerDay)<-c("date","steps")
head(StepsPerDay)
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

1. Make a histogram of the total number of steps taken each day


```r
ggplot(StepsPerDay, aes(x=steps))+
  geom_histogram(fill="blue",binwidth=1000)+
  labs(title="Steps Taken per Day", x="Number of Steps per Day", y= "Number of times in day")+ theme_bw()
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->

2. Calculate the mean and median total number of steps taken per day


```r
MeanSteps<-mean(StepsPerDay$steps, na.rm=TRUE)
MedianSteps<-median(StepsPerDay$steps, na.rm=TRUE)
```

## What is the average daily activity pattern?

```r
AvgStepsPerInterval<-aggregate(Data$steps,
                               by=list(interval=Data$interval),
                               FUN=mean, na.rm=TRUE)
AvgStepsPerInterval$interval<-as.integer(levels(AvgStepsPerInterval$interval)[AvgStepsPerInterval$interval])
colnames(AvgStepsPerInterval)<-c("interval","steps")
```

1. Make a time series plot


```r
ggplot(AvgStepsPerInterval,aes(x=interval,y=steps))+
  geom_line(color="red",size=1)+
  labs(title="Average Daily Activity Pattern", x="Interval",y="Number of Steps")+
  theme_bw()
```

![](PA1_template_files/figure-html/plot1-1.png)<!-- -->

2. Which 5 minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
MaximumInterval<-AvgStepsPerInterval[which.max(AvgStepsPerInterval$steps),]
```

## Inputting missing values

1. Calculate the total number of missing values in the dataset


```r
MissingValues<-sum(is.na(Data$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset & 3. Create new dataset that is equal to the original dataset but with the missing data filled in.


```r
NAfill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- Data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

Datafill <- data.frame(  
        steps = NAfill(Data, AvgStepsPerInterval),  
        date = Data$date,  
        interval = Data$interval)
str(Datafill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

4. Make a histogram of the total number of steps taken each day


```r
StepsPerDayFilled<-aggregate(steps~date,Datafill,sum)
colnames(StepsPerDayFilled)<-c("date","steps")
ggplot(StepsPerDayFilled, aes(x=steps))+
  geom_histogram(fill="blue",binwidth=1000)+
  labs(title="Steps Taken per Day", x="Number of Steps per Day", y= "Number of times in a Day")+theme_bw()
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day.


```r
StepsPerDayFillMean<-mean(StepsPerDayFilled$steps,na.rm=TRUE)
StepsPerDayFillMedian<-median(StepsPerDayFilled$steps,na.rm=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?


```r
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) # weekdays
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(Datafill)
```

2. Make a panel plot containing a time series plot


```r
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="red") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->


