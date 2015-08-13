
## Reproducible Research: Peer Assessment 1


load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

setdir <- function() {
  setwd("C:/Users/gautam.Mom81/Documents/courseraDataScienceSpecialization/reproducibleResearch/RepData_PeerAssessment1")
}


## Loading and preprocessing the data

# Load the data (i.e. read.csv())
# 
# Process/transform the data (if necessary) into a format suitable for your analysis

step1 <- function() {
  
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(rCharts)
  
  df1 <- read.csv("activity/activity.csv", stringsAsFactors=FALSE) 
  df1 <- tbl_df(df1)
  df1$date <- as.Date(df1$date)
  message("Read activity.csv row count:", nrow(df1))
  
  ## What is mean total number of steps taken per day?
  
  #   Calculate the total number of steps taken per day  
  dfSum <- ddply(df1, c("date"), summarise, dailySteps = sum(steps))
  
  #   
  #   Make a histogram of the total number of steps taken each day
  #   
  m <- ggplot(dfSum, aes(x=date, y=dailySteps)) +  geom_bar(stat="identity") 
  m <- m +  ggtitle("Total steps per day - N/As ignored") + xlab("Date")+ ylab("Steps per day") 
  plot(m)
  
  #   Calculate and report the mean and median of the total number of steps taken per day
  
  print(sprintf("Mean total number of steps per day: %.0f", mean(dfSum$dailySteps, na.rm=TRUE)))
  print(sprintf("Median of total number of steps per day:%d", median(dfSum$dailySteps, na.rm=TRUE)))
  
  
  ## What is the average daily activity pattern?
  # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
  # the average number of steps taken, averaged across all days (y-axis)
  dfDaily <- ddply(df1, c("interval"), summarise, 
                  intervalMeanSteps = mean(steps, na.rm=TRUE))
  m <- ggplot(dfDaily, aes(x=interval, y=intervalMeanSteps)) +  geom_line(stat="identity") 
  m <- m +  ggtitle("Daily Activity Pattern") + xlab("Time of Day")+ ylab("Steps") 
  plot(m)
  
  # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  intMax <- filter(dfDaily, intervalMeanSteps == max(dfDaily$intervalMeanSteps))
  print(sprintf("Highest 5-minute interval: %d has %.0f steps", 
                intMax$interval, intMax$intervalMeanSteps))
  
  
  ## Imputing missing values
  # Calculate and report the total number of missing values in the dataset 
  # (i.e. the total number of rows with NAs)
  
  numNas <- sum(is.na(df1))
  print(sprintf("Total N/As in the dataset:%d", numNas))
  
  
  # Strategy for filling in all of the missing values in the dataset. 
  # a) Each N/A missing value is filled with the mean for that day. 
  # b) If all readings in a day have N/As, then the mean is taken as 0

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
  
  #   Calculate the total number of steps taken per day
  dfSum <- ddply(dfNew, c("date"), summarise, dailySteps = sum(steps))
  
  #   Make a histogram of the total number of steps taken each day
  m <- ggplot(dfSum, aes(x=date, y=dailySteps)) +  geom_bar(stat="identity") 
  m <- m +  ggtitle("Total steps per day with imputed values") + xlab("Date") + ylab("Steps per day") 
  plot(m)
  
  #   Calculate and report the mean and median of the total number of steps taken per day
  
  print(sprintf("Mean total number of steps per day: %.0f", mean(dfSum$dailySteps)))
  print(sprintf("Median of total number of steps per day:%d", median(dfSum$dailySteps)))
  
  # Do these values differ from the estimates from the first part of the assignment? 
  # What is the impact of imputing missing data on the estimates of the total daily number of steps?  
  df2 <- filter(df1, !is.na(steps))
  df3 <- filter(df1, is.na(steps))
  u1 <- unique(df1$date)
  u2 <- unique(df2$date)
  u3 <- unique(df3$date)
  u4 <- c(u2,u3)
  if (length(u1) == length(u4)) print("No dates have partial NA readings")

  
  ## Are there differences in activity patterns between weekdays and weekends?
  # Create a new factor variable in the dataset with two levels:
  #  - "weekday" and "weekend"
  
  w <- weekdays(as.Date(dfNew$date))
  
  funcDayType <- function(x) {if (x == "Saturday" || x == "Sunday") "weekend" else "weekday"}
  
  w <- sapply(w,funcDayType)
  dfNew$dayType <- factor(w)

  dfSum3 <- ddply(dfNew, c("dayType", "interval"), summarise, meanIntervalSteps = mean(steps))

  #   Make a histogram of the total number of steps taken each day
  m <- ggplot(dfSum3, aes(x=interval, y=meanIntervalSteps)) +  geom_line() 
  # With one variable
  m <- m +  ggtitle("Weekday vs Weekend activity") + xlab("Daily interval")+ ylab("Steps") 
  m <- m + facet_grid(dayType ~  .)
  plot(m)
}

tstStep <- function() {
  
  df <- read.csv("activity/activity.csv", stringsAsFactors=FALSE) 
  df <- tbl_df(df)
  message("Read activity.csv row count:", nrow(df))

  # I first calculate the mean for the day
  dfMeanSteps <- ddply(df, c("date"), summarise, intervalMean = mean(steps, na.rm=TRUE))
  funM <- function(x) {if (is.na(x)) 0 else x}
  dfMeanSteps$intervalMean <- lapply(dfMeanSteps$intervalMean,funM)
  
  # Then I apply the mean of the day to the N/A values and create a dfNew
  fixNaStep <- function(dfRow, dfMeanSteps) { 
    # message(toString(dfRow))
 
    s <- dfRow[[1]] ; d <- dfRow[[2]] ; i <- dfRow[[3]]
    #print(sprintf("s:%s  d:%s i:%s",s,d,i))
    
    if (is.na(s)) { 
      r <- filter(dfMeanSteps, date==d)
      s <- r$intervalMean
    }
    message("class of return:", class(s[[1]]))
    s[[1]]
  }
  fixedSteps <- apply(df[1:5,], 1, fixNaStep, dfMeanSteps)
  
}

tstStep2 <- function() {
  library(dplyr)
  df <- read.csv("activity/activity.csv", stringsAsFactors=FALSE) 
  df <- tbl_df(df)
  message("Read activity.csv row count:", nrow(df))
  
  # I first calculate the mean for the day
  dfMeanSteps <- ddply(df, c("date"), summarise, intervalMean = mean(steps, na.rm=TRUE))
  funM <- function(x) {if (is.na(x)) 0 else x}
  dfMeanSteps$intervalMean <- lapply(dfMeanSteps$intervalMean,funM)
  
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
  #ddply(dataFrame, .(x), function(x) { # do stuff })
  fixedDf <- ddply(df, .(steps, date, interval), fixNaStep, dfMeanSteps)

}



step2 <- function() {
  
}


## What is the average daily activity pattern?
step3 <- function() {
  
  
}


## Imputing missing values
step4 <- function() {
  
}


## Are there differences in activity patterns between weekdays and weekends?

step5 <- function() {
  
}
