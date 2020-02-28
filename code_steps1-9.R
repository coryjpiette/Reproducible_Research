# Loading the Data

library(ggplot2)
\library(plyr)
activity <- read.csv("activity.csv")

# Processing the Data

activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")

##pulling data without nas
clean <- activity[!is.na(activity$steps),]

What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

## summarizing total steps per date
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")

## Creating the historgram of total steps per day
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

## Mean of Steps
as.integer(mean(sumTable$Steps))
## [1] 10766

## Median of Steps
as.integer(median(sumTable$Steps))
## [1] 10765

## mean: 10766 steps.
## median: 10765 steps.


library(plyr)
library(ggplot2)

## pulling data without nas
clean <- activity[!is.na(activity$steps),]

## create average number of steps per interval
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

## Create line plot of average number of steps per interval
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")




## Maximum steps by interval

maxSteps <- max(intervalTable$Avg)

intervalTable[intervalTable$Avg==maxSteps,1]
## [1] 835

## Max 5-minute interval: 835


## Number of NAs in original data set
nrow(activity[is.na(activity$steps),])
## [1] 2304
## The total number of rows with steps = ‘NA’: 2304.

## Create the average number of steps per weekday and interval

avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))

## Create dataset with all NAs for substitution

nadata<- activity[is.na(activity$steps),]
## Merge NA data with average weekday interval for substitution

newdata<-merge(nadata, avgTable, by=c("interval", "day"))
Create a new dataset that is equal to the original dataset but with the missing data filled in.

## Reorder the new substituded data in the same format as clean data set
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

## Merge the NA averages and non NA data together

mergeData <- rbind(clean, newdata2)
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

##Create sum of steps per date to compare with step 1

sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")

## Mean of Steps with NA data

as.integer(mean(sumTable2$Steps))
## [1] 10821

## Median of Steps with NA data
as.integer(median(sumTable2$Steps))
## [1] 11015
## Creating the histogram of total steps per day, categorized by data set to show impact

hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )


## The new mean of the imputed data: 10821 steps

## The new median of the imputed data: 11015 steps 

## Create new category based on the days of the week

mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

library(lattice) 
## Summarize data by interval and type of day

intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

##Plot data

xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

## Trends are different
