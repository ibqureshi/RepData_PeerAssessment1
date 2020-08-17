
library(ggplot2)
library(lubridate)
dataraw <- read.csv("activity.csv", stringsAsFactors = FALSE)
data <- na.omit(dataraw)

## What is mean total number of steps taken per day?

m <- mean(data$steps)
mean <- with(data, aggregate(steps, by = list(date), mean))
median <- with( data, aggregate(data$steps, by = list(date), median))
totalstepsbyday <- with(data, aggregate(steps, by = list(date), sum))

hist(totalstepsbyday$x,xlab = "Total Steps Per Day", main = "Total Steps")

## What is the average daily activity pattern?

plot2data <- with(data, aggregate(steps, by = list(interval), sum))

plot(plot2data, type = "l", xlab = "Interval", ylab = "Steps", col = "black")
title(main="Steps over Time")

max <- max(data$steps)
data[which(data$steps == max), ]

## Imputing missing values

missing <- is.na(dataraw[,1])
dataraw[missing,1]<-m

mean2 <- with(dataraw, aggregate(steps, by = list(date), mean))
median2 <- with(dataraw, aggregate(data$steps, by = list(date), median))
totalstepsbyday2 <- with(dataraw, aggregate(steps, by = list(date), sum))

hist(totalstepsbyday2$x,xlab = "Total Steps Per Day", main = "Total Steps")

## Are there differences in activity patterns between weekdays and weekends?

par(mfrow = c(1,2))

dataraw$day <- wday(as.POSIXlt(dataraw$date))

for (i in 1:nrow(dataraw))
{
  if(dataraw[i,4] == 1)
  {
    dataraw$wknd[i] <- "Weekend"
  }
  else if (dataraw[i, 4] == 7)
  {
    dataraw$wknd[i] <- "Weekend"
  }
  else
  {
    dataraw$wknd[i] <- "Weekday"
  }
}

week <- with(dataraw, aggregate(steps, by = list(interval, wknd), sum))

wknd <- week[week$Group.2 %in% "Weekend" ,]
wkdy <- week[week$Group.2 %in% "Weekday",]

plot(wknd$Group.1, wknd$x, type = "l", xlab = "Interval", ylab = "Steps", col = "black", main = "Weekend")
plot(wkdy$Group.1, wkdy$x, type = "l", xlab = "Interval", ylab = "Steps", col = "black", main = "Weekday")

