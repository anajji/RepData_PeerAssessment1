Loading the library

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Loading and preprocessing the data
==================================

Load the data and convert the date as date format.

``` r
data<-read.csv("activity.csv")
data$date<-as.Date(data$date)
```

What is mean total number of steps taken per day?
=================================================

Make a histogram of the total number of steps taken each day and calculate the mean and median value.

``` r
summaryStep<- data %>% group_by(date) %>% summarise(totStep=sum(steps,na.rm=T))
a<-ggplot(data = summaryStep,aes(x = totStep))
smean<-mean(summaryStep$totStep)
smedian<-median(summaryStep$totStep)
a+geom_histogram(bins=20,fill="blue")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

The mean of the total number of steps taken per day is 9354.2295082 and the median value is 10395

What is the average daily activity pattern?
===========================================

Make a time series plot of the 5-minute intervaland the average number of steps taken, averaged across all days.

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
summaryInterval<- data %>% group_by(interval) %>% summarise(meanStep=mean(steps,na.rm=T))
b<-ggplot(data = summaryInterval,aes(interval,meanStep))
maxInterval <- summaryInterval[summaryInterval$meanStep==max(summaryInterval$meanStep),1]
b+geom_line()
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

The 5-minute interval containing the maximum number of steps is 835.

Imputing missing values
=======================

``` r
Navalue<-sum(!complete.cases(data))
```

There is 2304 missing value in the dataset. I replace the NA value by the mean value of the given interval.

``` r
data2 <- transform(data, steps = ifelse(is.na(data$steps), summaryInterval$meanStep[match(data$interval, summaryInterval$interval)], data$steps))
```

We can look at the change from this imputation.

``` r
summaryStep2<- data2 %>% group_by(date) %>% summarise(totStep=sum(steps))
smean2<-mean(summaryStep2$totStep)
smedian2<-median(summaryStep2$totStep)
diffmean<-smean2-smean
diffmedian <- smedian2-smedian
hist(summaryStep2$totStep,col=rgb(1,0,0,0.5),xlab="Number of Steps",breaks=20)
hist(summaryStep$totStep,col=rgb(0,0,1,0.5),xlab="Number of Steps",add=T,breaks=20)
legend("topright", c("Imputed","Non-imputed"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)),lwd=5)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

The mean of the total number of steps taken per day for the imputed data is 1.076618910^{4} and the median value is 1.076618910^{4}

As expected the imputation increase the mean value by 1411.959171 and the meadian value by 371.1886792

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

Based on the date we can deduce if it is a day of the week or if it is during the weekend.

``` r
data2$weekday<-weekdays(data2$date)
data2 <- transform(data2, weekday = ifelse(data2$weekday %in% c("samedi","dimanche"), "weekend", "weekday"))

summaryIntervalWeekday<- data2%>% filter(weekday=="weekday") %>% group_by(interval) %>% summarise(meanStep=mean(steps))

summaryIntervalWeekend<- data2%>% filter(weekday=="weekend") %>% group_by(interval) %>% summarise(meanStep=mean(steps))

par(mfrow=c(1,2))

plot(summaryIntervalWeekday$interval,summaryIntervalWeekday$meanStep,type="l",xlab="interval",ylab="Number of Steps (Weekday)")
plot(summaryIntervalWeekend$interval,summaryIntervalWeekend$meanStep,type="l",xlab="interval",ylab="Number of Steps (Weekend)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

The plots show that there is on average an higher activity during the weekend.
