---
title: "Assignment1"
author: "Hugo Escobar"
date: "15 February, 2015"
---

# Loading and preprocesing the data


```r
srcData<-read.csv("activity.csv")
```

# What is mean total number of steps taken per day?

1. histogram of the total number of steps (frequency) taken each day

```r
stepsPDay <- aggregate(steps ~ as.Date(date), srcData, sum, na.rm=TRUE)
hist(stepsPDay$steps, col=c(20))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2. Report of **mean** and **median**
*Mean of total number of steps taken/day: 1.0766189 &times; 10<sup>4</sup>
*Median of total num of steps taken/day: 10765
```

# What is the average daily activity pattern?

1. Make a time series plot

```r
library(ggplot2)
```

```
## Loading required package: reshape
## Loading required package: plyr
## 
## Attaching package: 'reshape'
## 
## The following object(s) are masked from 'package:plyr':
## 
##     rename, round_any
## 
## Loading required package: grid
## Loading required package: proto
```

```r
dailyAvg<-aggregate(steps ~ interval, data=srcData, mean, na.rm=TRUE)
ggplot(data=dailyAvg,aes(x=interval,y=steps))+geom_line()+scale_x_discrete(breaks=seq(0,2400, by=100))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
int5min<-aggregate(steps ~ interval, data=srcData, mean, na.rm=TRUE)
maxNSteps <- int5min[int5min$steps %in% max(int5min$steps), c('interval')] 
gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", maxNSteps, perl=TRUE)
```

```
## [1] "8:35"
```

# Imputing missing values

1. Total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(srcData))
```

```
## [1] 2304
```

2. Strategy for filling in all of the missing values in the dataset
Here I decided to use the mean of steps per interval to fill up those slots
with NA values in the steps column.

For more info on [Imputation](http://en.wikipedia.org/wiki/Imputation_(statistics)

3. New dataset that is equal to the original dataset but with the missing data filled in

```r
stepPIntrv <- aggregate(steps ~ interval, srcData, mean, na.rm=TRUE)

# Probably not the best way to impute missing values. There are several
# packages that can help with this task but none of them could be installed
# on my system. This solucion has no external dependencies as it has
# been written in standard R

newData <- srcData

for (i in 1:nrow(newData)) {
  if (is.na(newData[i,c('steps')])) {
		interval <- newData[i, c('interval')]
		newData[i, c('steps')] <- stepPIntrv[which(stepPIntrv$interval %in% interval), c('steps')]
	}
}
```

4. Histogram of the total number of steps taken each day; Report the mean and median total number of steps taken per day. 

```r
stepsPDay2 <- aggregate(steps ~ as.Date(date), newData, sum, na.rm=TRUE)
hist(stepsPDay2$steps, col=c(20))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Although the shape of the histogram seems to be the same, the values in the y axis,
especially in the 10000-15000 range are greater than the values observed in the first
part of the assignment.

# Are there differences in activity patterns between weekdays and weekends?

1. New factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
newData$weekday <- ifelse(
  				weekdays(as.Date(newData[,c('date')])) %in% c('Saturday','Sunday'),
					'weekend', 'weekday'
				)
newData$weekday <- factor(newData$weekday)
```

2. Panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(grid)
library(gridExtra)

stepsWdays <- subset(newData, weekday %in% 'weekday')
stepsWdays <- aggregate(steps ~ interval, data=stepsWdays, FUN=mean)
pweekday <- qplot(interval, steps, data=stepsWdays, geom="line") + scale_x_discrete(breaks=seq(0, 2400, by=100))

stepsWends <- subset(newData, weekday %in% 'weekend')
stepsWends <- aggregate(steps ~ interval, data=stepsWends, FUN=mean)
pweekend <- qplot(interval, steps, data=stepsWends, geom = "line") +scale_x_discrete(breaks=seq(0, 2400, by=100))

grid.arrange(pweekday, pweekend, nrow = 2, main = "Diff in activity patterns: wday vs wkend")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 


