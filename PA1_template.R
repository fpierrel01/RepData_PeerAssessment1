# Loading and preprocessing the data
## Unzip the activity.zip file and read the csv file 
        unzip(zipfile="activity.zip")
        data <- read.csv("activity.csv")

## Load the "ggplot2" plotting System
        library(ggplot2)

## Calculate the total steps taken per day while removing missing data
        total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
        
        qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
## Calculate the mean and median        
        mean(total.steps, na.rm=TRUE)
        ### The mean is  [1] 9354.23
        median(total.steps, na.rm=TRUE)
        ### The median is [1] 10395
        
# Average daily Activity pattern
##Time series plot of 5 minute interval
        library(ggplot2)
        averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                              FUN=mean, na.rm=TRUE)
        ggplot(data=averages, aes(x=interval, y=steps)) +
                geom_line() +
                xlab("5-minute interval") +
                ylab("average number of steps taken")        
        
## The 5-minute interval , on average, that contains the maximum number of steps        
        averages[which.max(averages$steps),]
        
        ###     interval    steps
        ### 104      835 206.1698

# Imputing missing values       
## Calculate the total number of missing values
        missing <- is.na(data$steps)
        table(missing)        
        ### missing
        ### FALSE  TRUE 
        ### 15264  2304
        
## Filling in all missing values with the mean value of its 5-minute interval
        fill.value <- function(steps, interval) {
                filled <- NA
                if (!is.na(steps))
                        filled <- c(steps)
                else
                        filled <- (averages[averages$interval==interval, "steps"])
                return(filled)
        }
        filled.data <- data
        filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)  
       
## Calculate the total steps taken per day with the NA filled in with mean value of its 5-minute interval        
        total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
        
        qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
        
        ## Calculate the mean and median
        mean(total.steps)
        ### The mean is [1] 10766.19
        median(total.steps)
        ### The median is [1] 10766.19
        ##
        ### These values differ from the estimates from the first part of the assignment.
        ### Imputing the missinf data caused the mean to be equal to the median
        
# Are there differences in activity pattern between weekdays and weekends?
## Factor variable with two levels - "weekday" and "weekend".
        weekday.or.weekend <- function(date) {
                day <- weekdays(date)
                if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                        return("weekday")
                else if (day %in% c("Saturday", "Sunday"))
                        return("weekend")
                else
                        stop("invalid date")
        }
        filled.data$date <- as.Date(filled.data$date)
        filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
        
                                
        ##Time series plot of 5 minute interval                                    
        averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
        ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
                xlab("5-minute interval") + ylab("Number of steps") 
        
        
        