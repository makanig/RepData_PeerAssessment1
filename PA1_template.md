# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

### Load the data with read.csv()
### Process/transform the data into a format suitable for analysis



```r
df1 <- read.csv("activity/activity.csv", stringsAsFactors=FALSE) 
df1 <- tbl_df(df1)
df1$date <- as.Date(df1$date)
message("Read activity.csv row count:", nrow(df1))
```

```
## Read activity.csv row count:17568
```

## What is mean total number of steps taken per day?

###   Calculate the total number of steps taken per day  

```r
dfSum <- ddply(df1, c("date"), summarise, dailySteps = sum(steps))

#   Make a histogram of the total number of steps taken each day
m <- ggplot(dfSum, aes(x=date, y=dailySteps)) +  geom_bar(stat="identity") 
m <- m +  ggtitle("Total steps per day - N/As ignored") + xlab("Date")+ ylab("Steps per day") 
plot(m)
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

##   Calculate and report the mean and median of the total number of steps taken per day


```r
print(sprintf("Mean total number of steps per day: %.0f", mean(dfSum$dailySteps, na.rm=TRUE)))
```

```
## [1] "Mean total number of steps per day: 10766"
```

```r
print(sprintf("Median of total number of steps per day:%d", median(dfSum$dailySteps, na.rm=TRUE)))
```

```
## [1] "Median of total number of steps per day:10765"
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
### the average number of steps taken, averaged across all days (y-axis)

```r
dfDaily <- ddply(df1, c("interval"), summarise, 
                 intervalMeanSteps = mean(steps, na.rm=TRUE))
m <- ggplot(dfDaily, aes(x=interval, y=intervalMeanSteps)) +  geom_line(stat="identity") 
m <- m +  ggtitle("Average Daily Activity Pattern") + xlab("Interval")+ ylab("Steps") 
plot(m)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intMax <- filter(dfDaily, intervalMeanSteps == max(dfDaily$intervalMeanSteps))
print(sprintf("Highest 5-minute interval: %d has %.0f steps", 
              intMax$interval, intMax$intervalMeanSteps))
```

```
## [1] "Highest 5-minute interval: 835 has 206 steps"
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset 

```r
numNas <- sum(is.na(df1))
print(sprintf("Total N/As or missing values in the dataset:%d", numNas))
```

```
## [1] "Total N/As or missing values in the dataset:2304"
```

### Strategy for filling in all of the missing values in the dataset. 
#### a) Each N/A missing value is filled with the mean for that day. 
#### b) If all readings in a day have N/As, then the mean is taken as 0

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# I first calculate the mean for the day
dfMeanSteps <- ddply(df1, c("date"), summarise, intervalMean = mean(steps, na.rm=TRUE))
funM <- function(x) {if (is.na(x)) 0 else x}
dfMeanSteps$intervalMean <- sapply(dfMeanSteps$intervalMean,funM)

# Then I apply the mean of the day to the N/A values and create a dfNew
fixNaStep <- function(dfRow, dfMeanSteps) { 
  #message(toString(dfRow))
  s <- dfRow$steps ; d <- dfRow$date ; i <- dfRow$interval
  #print(sprintf("s:%s  d:%s i%d",s,d,i))
  if (is.na(s)) { 
    r <- filter(dfMeanSteps, date==d)
    s <- r$intervalMean
  }
  dfRow$steps <- s[[1]]
  dfRow
}
dfNew <- ddply(df1, .(steps, date, interval), fixNaStep, dfMeanSteps)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
#   Calculate the total number of steps taken per day
dfSum <- ddply(dfNew, c("date"), summarise, dailySteps = sum(steps))

#   Make a histogram of the total number of steps taken each day
m <- ggplot(dfSum, aes(x=date, y=dailySteps)) +  geom_bar(stat="identity") 
m <- m +  ggtitle("Total steps per day with imputed values") + xlab("Date") + ylab("Steps per day") 
plot(m)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

###   Calculate and report the mean and median of the total number of steps taken per day

```r
print(sprintf("Mean total number of steps per day: %.0f", mean(dfSum$dailySteps)))
```

```
## [1] "Mean total number of steps per day: 9354"
```

```r
print(sprintf("Median of total number of steps per day:%d", median(dfSum$dailySteps)))
```

```
## [1] "Median of total number of steps per day:10395"
```
These values above differ from the estimates from the first part of the assignment. This is
because of imputing missing data on the estimates of the total daily number of steps.
Both the mean and median have decreased because previously data that was ignored because
they were N/As, have now been replaced by 0's, bringing down the overall mean and median.
The mean was more affected since all missing values now have a value of 0. 
The median is also affected but not to the same degree.

The code below also checks and confirms whether any dates have partial NA readings
This is used to confirm that all previous N/A values have in fact become 0 now.
If any dates had partial NA values, those would have been replaced with the mean for 
that day, and not contributing to a reduction of the overall mean.

```r
df2 <- filter(df1, !is.na(steps))
df3 <- filter(df1, is.na(steps))
u1 <- unique(df1$date)
u2 <- unique(df2$date)
u3 <- unique(df3$date)
u4 <- c(u2,u3)
if (length(u1) == length(u4)) print("No dates have partial NA readings")
```

```
## [1] "No dates have partial NA readings"
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels  - "weekday" and "weekend"

```r
w <- weekdays(as.Date(dfNew$date))

funcDayType <- function(x) {if (x == "Saturday" || x == "Sunday") "weekend" else "weekday"}

w <- sapply(w,funcDayType)
dfNew$dayType <- factor(w)
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
dfSum3 <- ddply(dfNew, c("dayType", "interval"), summarise, meanIntervalSteps = mean(steps))

m <- ggplot(dfSum3, aes(x=interval, y=meanIntervalSteps)) +  geom_line() 
m <- m +  ggtitle("Weekday vs Weekend activity") + xlab("Interval")+ ylab("Steps") 
m <- m + facet_grid(dayType ~  .)
plot(m)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 





