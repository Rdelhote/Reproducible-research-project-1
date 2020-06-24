---
title: "Steps_Activity_Research"
author: "Ramon De LHotellerie"
date: "6/23/2020"
output: html_document
---
=======

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = FALSE)
```

**1. Loading and preprocessing the data. Data Load**

First, setting the working directory, then download the .csv dataset 

```{r load data}
setwd("~/Documents/Estudios/Coursera/Data Science - Johns Hopkins/Data/")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "rep_data-activity (1).zip"
download.file(url, destfile)
unzip(destfile)
activity <- read.csv("activity.csv", sep = ",")
```

Column names and structure (the missing values can be ignored):
```{r activity details}
names(activity)
str(activity)
head(activity[which(!is.na(activity$steps)), ]) # NA rows removed form the file
```

**2. What is mean total number of steps taken per day? Mean calculation of "total number of step taken per day" over all days**

```{r steps mean}
library(reshape2)
total_activity <- melt(activity[which(!is.na(activity$steps)), ], id.vars = c("date", "interval"))
head(total_activity)
total_steps_day <- dcast(total_activity, date ~ variable, sum)
head(total_steps_day)
summary(total_steps_day)
``` 
Mean value calculated is **10766.19** steps per day, and median value **10765** steps per day

Plot showing Total Steps per day

```{r total step per day histogram}
hist(total_steps_day$steps, main = "Total steps per day",
     xlab = "Steps per day", ylab = "Days", 
     breaks = 10, col = "dark green")
abline(v = mean(total_steps_day$steps), lty = 1, lwd = 2, col = "red")
abline(v = median(total_steps_day$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("red", "dark blue"),       lty = c(1, 2), lwd = c(2, 2))
dev.copy(png,'1st histogram.png')
dev.off()
```

Same plot using ggplot.
```{r ggplot histogram}
library(ggplot2)
ggplot(total_steps_day, aes(steps, fill="#FF9999", legend="steps")) + geom_histogram(bins = 8) + ggtitle("Total steps per day")
dev.copy(png,'2nd histogram.png')
dev.off()
```

Plot showing the total number of steps taken per day over two months.
```{r total steps trend}
library(lubridate)
total_steps_day$date <- as.Date(total_steps_day$date)
ggplot(total_steps_day, aes(date, steps)) + geom_line() +            
        scale_x_date(date_labels = "%b %d") + 
        ylab("Total number of steps")
dev.copy(png,'Activity Pattern.png')
dev.off()
```

**3. What is the average daily activity pattern?**
In order to find out the average daily pattern, we can create a time series plot, where you can find the average number of steps for each interval.

```{r}
meanStepsInterval <- dcast(total_activity, interval ~ variable, mean, na.rm = TRUE)
head(meanStepsInterval)
summary(meanStepsInterval)
```

Base R plotting system to create a time series plot (interval on the x axis, and the average steps data on the y axis).

```{r}
plot(x=meanStepsInterval$interval, y=meanStepsInterval$steps, type="l",
     main="Time Series Plot of Average Steps per Interval",
     ylab="Average Steps", xlab="5 minutes Intervals",
     col="darkgreen", lwd=1.5, family="serif")
dev.copy(png,'Average Steps per Interval Plot.png')
dev.off()
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
meanStepsInterval[grep(max(meanStepsInterval$steps), meanStepsInterval$steps), ]
```

The 5-minute interval containing the maximum number of steps is interval **835**.

**4. Imputing missing values**. 

First, let's confirm if there are any NA values in the dataset at all:
```{r}
anyNA(activity)
sum(is.na(activity))
```

There are **2304** rows with a missing value, so let's now replace all of those NA values by the time average over all other days. 
```{r impute missing days data}
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
Let’s check again if there are any NA values in the dataset at all:
```{r}
anyNA(total_activity2)
```
and voila! There are no NA values left and all were imputed to the average value

Histogram of the total number of steps taken each day and calculation of the mean and median total number of steps taken per day. 
```{r}
imputedActivity <- aggregate(steps ~ date, total_activity2, sum)
head(imputedActivity)
summary(imputedActivity)
```
So the new median and mean are both **10766**, which indeed  differ from the estimates (mean and median) of the original dataset

Creating the histogram:

```{r}
paletteRed <- colorRampPalette(c("deeppink", "darkgreen", "darkred"))
hist(imputedActivity$steps, breaks=20, xlab="Number of Steps Taken", 
     main="Total Number of Steps Taken per Day -includes Imputed Values-",
     col=paletteRed(22), family="serif")
```

Furthermore, what is the impact of imputing missing data on the estimates of the total daily number of steps?

a. Mean and Median including NA values
```{r}
NAtotal_steps_day <- summary(total_steps_day$steps)
print(NAtotal_steps_day) 
```
b. Mean and Median including imputed values
```{r}
imputedActivitySummary <- summary(imputedActivity$steps)  
print(imputedActivitySummary)
```
The values of the two data sets pretty much the same. The mean values are exactly the same, at **10766** steps, whereas the median value is slightly larger for the imputed data set, at **10766** steps, rather than **10765** steps in the data set including NA values.

***5. Are there differences in activity patterns between weekdays and weekends? - Using the dataset with the filled-in missing values***

I used the weekdays function to calculate the day of the week each day value was assigned to. 
Then the value Weekend needed to be assigned to all rows for the Saturday and Sunday and the rest all assigned to Weekday. 

```{r}
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

As last, panel plot containing a time series plot needs to be made (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
```{r}
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
dev.copy(png,'Weekday Weekend Pattern.png')
dev.off()
```