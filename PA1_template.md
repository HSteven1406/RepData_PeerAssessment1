---
title: "Reproducible Research"
author: "Steven Hanssens"
date: "28/04/2021"
output: 
  html_document:
    keep_md: true
---




## Loading and pre processing data


```r
dataRaw <- read.csv(unz('activity.zip','activity.csv'))
data <- dataRaw
data$date <- as.Date(data$date)
data <- data[complete.cases(data),]
```

## What is mean total number of steps taken per day?

To know the mean total, first, we calculate the number the total number of steps per day and plot it in a histogram.


```r
dfSum <- data %>% group_by(date) %>% summarise(SumSteps= sum(steps,na.rm=T)) 

hist(dfSum$SumSteps, main = 'Histogram of the total number of steps taken each day',xlab='Number of steps taken each day', col='red')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Now, we calculate the mean and the median of the total number of steps taken per day.


```r
DayMean <- mean(dfSum$SumSteps)
print(DayMean)
```

```
## [1] 10766.19
```

```r
DayMedian <- median(dfSum$SumSteps)
print(DayMedian)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Here is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
IntervalDf <- data %>% group_by(interval) %>% summarise(Meansteps = mean(steps))
plot(IntervalDf$interval,IntervalDf$Meansteps,type="l",xlab = 'Interval',ylab='Average number of steps',
     main = 'Average number of steps taken per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

And we store the interval with the maximum number of steps averaged across all days.


```r
IntervalMax <- IntervalDf[IntervalDf$Meansteps==max(IntervalDf$Meansteps),1]
print(IntervalMax)
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

## Imputing missing values
To deal with missing values, we will, first, calculate the number of missing values in the dataset. 


```r
NAsNumber <- sum(is.na(dataRaw$steps))
print(NAsNumber)
```

```
## [1] 2304
```

Now, we filled the missing values in by using the mean number of steps taken for that 5-minute interval.


```r
dataCompleted <- dataRaw
dataCompleted$date <- as.Date(dataCompleted$date)
Temp <- merge(dataCompleted,IntervalDf)
Temp2 <- arrange(Temp,date)
Temp2[is.na(Temp2$steps),2] <- Temp2[is.na(Temp2$steps),4]
dataCompleted<- Temp2[,-4]
```

We can draw our histogram of number of steps per day per month with the new data and recalculate means and medians of steps taken per day.


```r
dfSumCompl <- dataCompleted %>% group_by(date) %>% summarise(SumSteps= sum(steps,na.rm=T)) 

hist(dfSumCompl$SumSteps, main = 'Histogram of the total number of steps taken each day',xlab='Number of steps taken each day', col='red')
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
DayMeanCompl <- mean(dfSumCompl$SumSteps)
print(DayMeanCompl)
```

```
## [1] 10766.19
```

```r
DayMedianCompl <- median(dfSumCompl$SumSteps)
print(DayMedianCompl)
```

```
## [1] 10766.19
```

Remember the first values of DayMean and DayMedian To see how theses values differ from each other, let's substract one each other. 


```r
print(DayMean)
```

```
## [1] 10766.19
```

```r
print(DayMedian)
```

```
## [1] 10765
```

As we can see, mean didn't change but imputing missing values changed median slightly

## Are there differences in activity patterns between weekdays and weekends?
Let's make a factor to indicate wether a given date correspond to a weekday or weekend day.


```r
dataCompleted$weekdays <- as.factor(ifelse(weekdays(dataCompleted$date)%in% c('samedi','dimanche'),'weekend','weekday'))
```

Now, we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
IntervalDfCompl <- dataCompleted %>% group_by(interval, weekdays) %>% summarise(Meansteps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(IntervalDfCompl, aes(x=interval,y=Meansteps)) + geom_line()+facet_grid(weekdays~.) + labs(x='5-minute interval',y='Average number of steps taken')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

As seen on the plots, there are differences between weekday and weekend. Weekday, activity begins sooner and is more stable throughout the day than weekend.


