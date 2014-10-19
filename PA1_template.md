# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
We set the appropriate working directory then load the data into a data frame.


```r
# Reset environment
rm(list=ls());
setwd("C:\\Users\\Robin\\RepData_PeerAssessment1");
df = read.csv("activity.csv");
```

## What is mean total number of steps taken per day?

We melt and recast data for total number of steps taken each studied day, then print an histogram in a png file.


```r
library(reshape2);
melted_dailyData = melt(df,id="date",measure=c("steps"), na.rm = TRUE);
df_dailyData = dcast(melted_dailyData, date ~ variable,fun.aggregate=sum);

#png(file = "figure\\plot1.png", width = 480, height= 480);  ## plot to a PNG file
hist(df_dailyData$steps,main = "Histogram of daily steps", ylab = "Frequency", xlab = "Number of daily steps",col = "blue");
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
#dev.off();
dailyStepMean = mean(df_dailyData$steps,na.rm = TRUE);
dailyStepMedian = median(df_dailyData$steps, na.rm= TRUE);
```

The total number of steps taken per day mean is 1.0766 &times; 10<sup>4</sup> and median is 10765.

## What is the average daily activity pattern?
We make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
melted_intervalMeanData = melt(df,id="interval",measure = "steps", na.rm= TRUE);
df_intervalMeanData = dcast(melted_intervalMeanData,interval ~ variable, fun.aggregate=mean);

#png(file = "figure\\plot2.png", width = 480, height= 480);  ## plot to a PNG file
plot(df_intervalMeanData$interval,df_intervalMeanData$steps, type = "l", main = "Mean steps by interval", xlab = "interval", ylab = "Number of steps");
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
#dev.off();
maxStepsInterval = df_intervalMeanData$interval[which.max(df_intervalMeanData$steps)];
```

The 5-minute interval that contains the maximum number of steps on average across all the days in the dataset is the interval number 835. 

## Imputing missing values



```r
countNA = sum(is.na(df$steps));
```

There are 2304 missing values in the dataset. We decide to create a new dataset by filling missing values with the 5-minute interval mean.


```r
filledDf = df;
filledDf$steps[is.na(df$steps)] = df_intervalMeanData$steps[is.na(df$steps)];
```

Then we make a histogram of the total number of steps taken each day.


```r
melted_filledDailyData = melt(filledDf,id="date",measure=c("steps"), na.rm = TRUE);
df_filledDailyData = dcast(melted_filledDailyData, date ~ variable,fun.aggregate=sum);
#png(file = "figure\\plot3.png", width = 480, height= 480);  ## plot to a PNG file
hist(df_filledDailyData$steps,main = "Histogram of filled daily steps", ylab = "Frequency", xlab = "Number of daily steps",col = "blue");
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

```r
#dev.off();
filledDailyStepMean = mean(df_filledDailyData$steps,na.rm = TRUE);
filledDailyStepMedian = median(df_filledDailyData$steps, na.rm= TRUE);
```

The total number of steps taken per day mean is 1.0766 &times; 10<sup>4</sup> and median is 1.0766 &times; 10<sup>4</sup>. These values do not significantly differ form the previous mean and median values. Our filling strategy has low impact on the distribution of the values.

## Are there differences in activity patterns between weekdays and weekends?

We add a new factor variable to the filled dataset with two levels: "weekday" and "weekendday", then make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).



```r
# Set system locale to get english time labels
Sys.setlocale(locale="English_United States.1252");
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
filledDf$date = as.Date(filledDf$date);
filledDf$weekday = weekdays(filledDf$date);

weekdaysList = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday");
weekenddaysList = c("Saturday", "Sunday");

filledDf$weekday[filledDf$weekday %in% weekdaysList] = "weekday";
filledDf$weekday[filledDf$weekday %in% weekenddaysList] = "weekend";
filledDf$weekday = as.factor(filledDf$weekday);

melted_intervalMeanFilledData = melt(filledDf,id=c("interval","weekday"),measure = "steps", na.rm= TRUE);
df_intervalMeanFilledData = dcast(melted_intervalMeanFilledData,interval + weekday ~ variable, fun.aggregate=mean);

library(lattice);

#png(file = "figure\\plot4.png", width = 480*2, height= 480);  ## plot to a PNG file
xyplot(steps ~ interval | weekday, data = df_intervalMeanFilledData, type = "l", layout = c(2, 1));
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 

```r
#dev.off();
```
