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
        
        
        
        