## Loading and processing the data

data <- read.csv("activity.csv", header= TRUE)

## What is the mean total number of steps taken per day?
hist_data <- aggregate(steps ~ date, data= data, sum, na.rm= TRUE)

hist(hist_data$steps, xlab = "Steps", ylab = "Frequency", main = "Total number of steps taken each day")

data_na_rm <- subset(data, steps != "NA")

mean_steps <- mean(hist_data$steps)
median_steps <- median(hist_data$steps)

## What is the average daily activity pattern?
average_steps_interval <- aggregate(steps ~ interval, data= data, FUN= mean)

plot(average_steps_interval$interval, average_steps_interval$steps, type= "l", xlab="interval", ylab= "average number of steps")
title("Average steps averaged across all days at 5 minute interval")

interval_with_max_steps <- subset(average_steps_interval, steps== max(steps), select = interval)

## Imputting missing values
total_missing_values <- sum(is.na(data$steps))

## The strategy to imput is to fill the NA values with the average steps for that interval
new_data <- data.frame(data)
new_data$steps <- replace(new_data$steps, is.na(new_data$steps), average_steps_interval$steps)
hist_new_data <- aggregate(steps ~ date, data= new_data, sum, na.rm= TRUE)

hist(hist_new_data$steps, xlab = "Steps", ylab= "Frequency", main = "Total number of steps taken each day")


mean_new_data_steps <- mean(hist_new_data$steps)
median_new_data_steps <- median(hist_new_data$steps)

## The mean steps is same as the one before imputting the NA values
## and the median is almost the same as the one before imputting the NA values. 

## Are there differences in activity patterns between weekdays and weekends?
new_data$wkday_wkend <- weekdays(as.Date(new_data$date))

new_data[new_data== "Saturday"]= "weekend"
new_data[new_data== "Sunday"]= "weekend"
new_data[new_data== "Monday"]= "weekday"
new_data[new_data== "Tuesday"]= "weekday"
new_data[new_data== "Wednesday"]= "weekday"
new_data[new_data== "Thursday"]= "weekday"
new_data[new_data== "Friday"]= "weekday"

average_newdata_steps_interval <- aggregate(steps ~ interval + wkday_wkend, data= new_data, FUN= mean)

library(ggplot2)

g<- qplot(average_newdata_steps_interval$interval, average_newdata_steps_interval$steps, data= average_newdata_steps_interval, facets= wkday_wkend~.)

g<- g + geom_line()

g<- g + labs(x= "Interval") + labs(y="Number of steps")

g<- g + labs(title = "Time series plot of the 5-minute interval & average number of steps")

g
