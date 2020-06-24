**1. Loading and preprocessing the data. Data Load**

First, setting the working directory, then download the .csv dataset

``` r
setwd("~/Documents/Estudios/Coursera/Data Science - Johns Hopkins/Data/")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "rep_data-activity (1).zip"
download.file(url, destfile)
unzip(destfile)
activity <- read.csv("activity.csv", sep = ",")
```

Column names and structure (the missing values can be ignored):

``` r
names(activity)
```

    ## [1] "steps"    "date"     "interval"

``` r
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
head(activity[which(!is.na(activity$steps)), ]) # NA rows removed form the file
```

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

**2. What is mean total number of steps taken per day? Mean calculation
of “total number of step taken per day” over all days**

``` r
library(reshape2)
total_activity <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval"))
head(total_activity)
```

    ##         date interval variable value
    ## 1 2012-10-02        0    steps     0
    ## 2 2012-10-02        5    steps     0
    ## 3 2012-10-02       10    steps     0
    ## 4 2012-10-02       15    steps     0
    ## 5 2012-10-02       20    steps     0
    ## 6 2012-10-02       25    steps     0

``` r
total_steps_day <- dcast(total_activity, date ~ variable, sum)
head(total_steps_day)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

``` r
summary(total_steps_day)
```

    ##          date        steps      
    ##  2012-10-02: 1   Min.   :   41  
    ##  2012-10-03: 1   1st Qu.: 8841  
    ##  2012-10-04: 1   Median :10765  
    ##  2012-10-05: 1   Mean   :10766  
    ##  2012-10-06: 1   3rd Qu.:13294  
    ##  2012-10-07: 1   Max.   :21194  
    ##  (Other)   :47

Mean value calculated is **10766.19** steps per day, and median value
**10765** steps per day

Plot showing Total Steps per day

``` r
hist(total_steps_day$steps, main = "Total steps per day",
     xlab = "Steps per day", ylab = "Days", 
     breaks = 10, col = "dark green")
abline(v = mean(total_steps_day$steps), lty = 1, lwd = 2, col = "red")
abline(v = median(total_steps_day$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("red", "dark blue"),       lty = c(1, 2), lwd = c(2, 2))
```

![](PA1-template_files/figure-markdown_github/total%20step%20per%20day%20histogram-1.png)

Same plot using ggplot.

``` r
library(ggplot2)
ggplot(total_steps_day, aes(steps, fill="#FF9999", legend="steps")) + geom_histogram(bins = 8) + ggtitle("Total steps per day")
```

![](PA1-template_files/figure-markdown_github/ggplot%20histogram-1.png)

Plot showing the total number of steps taken per day over two months.

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.6.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
total_steps_day$date <- as.Date(total_steps_day$date)
ggplot(total_steps_day, aes(date, steps)) + geom_line() +            
        scale_x_date(date_labels = "%b %d") + 
        ylab("Total number of steps")
```

![](PA1-template_files/figure-markdown_github/total%20steps%20trend-1.png)

**3. What is the average daily activity pattern?** In order to find out
the average daily pattern, we can create a time series plot, where you
can find the average number of steps for each interval.

``` r
meanStepsInterval <- dcast(total_activity, interval ~ variable, mean, na.rm = TRUE)
head(meanStepsInterval)
```

    ##   interval     steps
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

``` r
summary(meanStepsInterval)
```

    ##     interval          steps        
    ##  Min.   :   0.0   Min.   :  0.000  
    ##  1st Qu.: 588.8   1st Qu.:  2.486  
    ##  Median :1177.5   Median : 34.113  
    ##  Mean   :1177.5   Mean   : 37.383  
    ##  3rd Qu.:1766.2   3rd Qu.: 52.835  
    ##  Max.   :2355.0   Max.   :206.170

Base R plotting system to create a time series plot (interval on the x
axis, and the average steps data on the y axis).

``` r
plot(x=meanStepsInterval$interval, y=meanStepsInterval$steps, type="l",
     main="Time Series Plot of Average Steps per Interval",
     ylab="Average Steps", xlab="5 minutes Intervals",
     col="darkgreen", lwd=1.5, family="serif")
```

![](PA1-template_files/figure-markdown_github/unnamed-chunk-2-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

``` r
meanStepsInterval[grep(max(meanStepsInterval$steps), meanStepsInterval$steps), ]
```

    ##     interval    steps
    ## 104      835 206.1698

The 5-minute interval containing the maximum number of steps is interval
**835**.

**4. Imputing missing values**.

First, let’s confirm if there are any NA values in the dataset at all:

``` r
anyNA(activity)
```

    ## [1] TRUE

``` r
sum(is.na(activity))
```

    ## [1] 2304

There are **2304** rows with a missing value, so let’s now replace all
of those NA values by the time average over all other days.

``` r
## Impute missing values
total_activity2 <- split(activity, activity$interval)
total_activity2 <- lapply(total_activity2, function(x) {
        x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
        return(x)
})
total_activity2 <- do.call("rbind", total_activity2)
row.names(total_activity2) <- NULL
total_activity2 <- split(total_activity2, total_activity2$date)
df <- lapply(total_activity2, function(x) {
        x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
        return(x)
})
total_activity2 <- do.call("rbind", total_activity2)
row.names(total_activity2) <- NULL
head(total_activity2)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

Let’s check again if there are any NA values in the dataset at all:

``` r
anyNA(total_activity2)
```

    ## [1] FALSE

and voila! There are no NA values left and all were imputed to the
average value

Histogram of the total number of steps taken each day and calculation of
the mean and median total number of steps taken per day.

``` r
imputedActivity <- aggregate(steps ~ date, total_activity2, sum)
head(imputedActivity)
```

    ##         date    steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

``` r
summary(imputedActivity)
```

    ##          date        steps      
    ##  2012-10-01: 1   Min.   :   41  
    ##  2012-10-02: 1   1st Qu.: 9819  
    ##  2012-10-03: 1   Median :10766  
    ##  2012-10-04: 1   Mean   :10766  
    ##  2012-10-05: 1   3rd Qu.:12811  
    ##  2012-10-06: 1   Max.   :21194  
    ##  (Other)   :55

So the new median and mean are both **10766**, which indeed differ from
the estimates (mean and median) of the original dataset

Creating the histogram:

``` r
paletteRed <- colorRampPalette(c("deeppink", "darkgreen", "darkred"))
hist(imputedActivity$steps, breaks=20, xlab="Number of Steps Taken", 
     main="Total Number of Steps Taken per Day -includes Imputed Values-",
     col=paletteRed(22), family="serif")
```

![](PA1-template_files/figure-markdown_github/unnamed-chunk-7-1.png)

Furthermore, what is the impact of imputing missing data on the
estimates of the total daily number of steps?

1.  Mean and Median including NA values

``` r
NAtotal_steps_day <- summary(total_steps_day$steps)
print(NAtotal_steps_day) 
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    8841   10765   10766   13294   21194

1.  Mean and Median including imputed values

``` r
imputedActivitySummary <- summary(imputedActivity$steps)  
print(imputedActivitySummary)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

The values of the two data sets pretty much the same. The mean values
are exactly the same, at **10766** steps, whereas the median value is
slightly larger for the imputed data set, at **10766** steps, rather
than **10765** steps in the data set including NA values.

***5. Are there differences in activity patterns between weekdays and
weekends? - Using the dataset with the filled-in missing values***

I used the weekdays function to calculate the day of the week each day
value was assigned to. Then the value Weekend needed to be assigned to
all rows for the Saturday and Sunday and the rest all assigned to
Weekday.

``` r
daytype <- function(dates) {
        if (dates %in% c("Sunday", "Saturday")) {
                "weekend"
        }
        else {
                "weekday"
        }
}
total_activity2$dayName <- weekdays(as.Date(total_activity2$date))
total_activity2$dayType <- as.factor(apply(as.matrix(total_activity2$dayName), 1, daytype))
head(total_activity2)
```

    ##       steps       date interval dayName dayType
    ## 1 1.7169811 2012-10-01        0  Monday weekday
    ## 2 0.3396226 2012-10-01        5  Monday weekday
    ## 3 0.1320755 2012-10-01       10  Monday weekday
    ## 4 0.1509434 2012-10-01       15  Monday weekday
    ## 5 0.0754717 2012-10-01       20  Monday weekday
    ## 6 2.0943396 2012-10-01       25  Monday weekday

As last, panel plot containing a time series plot needs to be made (i.e.
type = “l”) of the 5-minute interval (x-axis) and the average number of
steps taken, averaged across all weekday days or weekend days (y-axis)

``` r
meanStepsWeekday <- aggregate(total_activity2$steps,
                                by = list(total_activity2$interval, total_activity2$dayType), mean)
names(meanStepsWeekday) <- c("interval", "dayType", "steps")

library(lattice)
xyplot(steps ~ interval | dayType, 
       data = meanStepsWeekday,
       type = 'l',
       xlab = '5-Minute Intervals',
       ylab = 'Average Number of Steps',
       layout = c(1,2))
```

![](PA1-template_files/figure-markdown_github/unnamed-chunk-11-1.png)
