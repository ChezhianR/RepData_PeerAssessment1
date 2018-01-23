---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
##Download Files
	
	if(!file.exists("./data"))
	{
		dir.create("./data")
	}

url1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"	

	download.file(url1,destfile="./data/Activity.zip")

##unzip files 

	unzip(zipfile="./data/Activity.zip",exdir="./data")
	ref_path <- file.path("./data" )

	##read CSV Data 	

	activitydata<- read.csv(file.path(ref_path,"activity.csv"),header=TRUE)

	##Manipulate data for processing 
	activitydata$date <- as.POSIXct(activitydata$date, format = "%Y-%m-%d")
	weekday <- weekdays(activitydata$date)
	activitydata <- cbind(activitydata,weekday)
```



## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
daily_activity <- activitydata %>% group_by(date) %>% summarise( total_steps = sum(steps,na.rm=TRUE) )

hist(daily_activity$total_steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "orange", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



## What is the average daily activity pattern?


```r
	average_daily_activity <- aggregate(activitydata$steps, by=list(activitydata$interval), FUN=mean, na.rm=TRUE)
	names(average_daily_activity) <- c("interval", "mean")
	plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="orange", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Imputing missing values


```r
 activity_fill <- activitydata
 activity_fill$steps[which(is.na(activity_fill$steps))] <- tapply(activity_fill$steps, activity_fill$interval, mean, na.rm=T, simplify=F )
 activity_fill$steps <- as.vector(activity_fill$steps, mode="numeric")	   
```




## Are there differences in activity patterns between weekdays and weekends?

```r
##Find days 
	activitydata$datetype <- sapply(activitydata$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
		library(ggplot2)

	activity_by_date <- aggregate(steps~interval + datetype, activitydata, mean, na.rm = TRUE)
	plot<-ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
	 print(plot)  
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

