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
        
        
        