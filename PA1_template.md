---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Exploring personal movement data

## Loading and preprocessing the data
First, let's load the data and preprocess it a little bit:

```r
# loading the data
exfile <- unzip("./activity.zip");
# treating DATE as factors
activity <- read.csv(exfile, stringsAsFactors = FALSE);

# formatting date
activity$date <- as.Date(activity$date);
```

## What is mean total number of steps taken per day?
To address this problem, we need to calculate the total number of steps
taken per day:

```r
steps_day <- as.vector(tapply(activity$steps, activity$date, sum));
```
Let's make a histogram to view this distribution:

```r
hist(steps_day);
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Now we can calculate the **mean** and **median** total number of steps
taken per day:

```r
mean(steps_day, na.rm = TRUE);
```

```
## [1] 10766.19
```

```r
median(steps_day, na.rm = TRUE);
```

```
## [1] 10765
```

## What is the average daily activity pattern?
We can go ahead and calculate the mean of steps per interval:

```r
library(plyr);
isteps <- ddply(activity, "interval", summarize,
                steps = mean(steps, na.rm = TRUE));
```

Here's the time series plot:

```r
plot(isteps$interval, isteps$steps, type = 'l');
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Now let's find out which 5-minute interval contains the maximum number
of steps:

```r
idx <- which.max(isteps$steps);
isteps[idx, "interval"];
```

```
## [1] 835
```

## Imputing missing values
The total number of missing values in the dataset can be calculated using
the following R code:

```r
missing <- !complete.cases(activity);
sum(missing);
```

```
## [1] 2304
```
Let's try fill in the missing values with the mean of that 5-minute interval:

```r
imputed <- activity;
# the interval for the missing observations
mintv <- imputed[missing, "interval"];
# we've calculated the mean of steps per interval in "ISTEPS"
idx <- sapply(mintv, function(x) which(isteps$interval == x));
imputed[missing, "steps"] <- isteps[idx, "steps"];
```

Now let's recalculate the total number of steps taken per day after imputing
missing values:

```r
nsteps <- as.vector(tapply(imputed$steps, imputed$date, sum));
```

Again, here's the histogram:

```r
hist(nsteps);
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

Finally, the new **mean** and **median** after imputing missing values:

```r
mean(nsteps);
```

```
## [1] 10766.19
```

```r
median(nsteps);
```

```
## [1] 10766.19
```

Seems like our approach of imputing missing values didn't change the mean
and median much.

## Are there differences in activity patterns between weekdays and weekends?

```r
weekday <- sapply(imputed$date, function(x) {
    wk <- as.POSIXlt(x)$wday;
    if (wk == 0 | wk == 6) { # Sunday or Saturday
        "weekend";
    } else {
        "weekday";
    }
});
imputed$weekday <- as.factor(weekday);
```

Now is the plot:

```r
wkend <- subset(imputed, weekday == "weekend");
wefrm <- ddply(wkend, "interval", summarize, steps = mean(steps));

wkday <- subset(imputed, weekday == "weekday");
wkfrm <- ddply(wkday, "interval", summarize, steps = mean(steps));

par(mfrow = c(2, 1));
plot(wefrm$interval, wefrm$steps, type = "l");
title("weekend");
plot(wkfrm$interval, wkfrm$steps, type = "l");
title("weekday");
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
