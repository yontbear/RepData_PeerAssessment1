#Loading and preprocessing the data
data <- read.csv("activity.csv")
head(data)
asDate <- strptime(data$date, "%Y-%m-%d")
newData <- cbind(data, asDate)
activity <- data.frame(newData$steps , newData$asDate, newData$interval)
names(activity) <- c("steps", "date", "interval")


#What is mean total number of steps taken per day?
good <- complete.cases(activity)
completeData <- activity[completeData,]
totalSteps <- tapply(activity$steps, activity$date, sum)
hist(totalSteps, col = "red", xlab = "Total Steps per Day", ylab = "Frequency", 
     main = "total steps by date")
mean <- mean(completeData$steps)
mean
median <- median(completeData$steps)
median

#What is the average daily activity pattern?
stepsInterval<-aggregate(steps~interval,data=activity, FUN=mean, na.rm=TRUE)
head(stepsInterval)
plot(steps~interval,data=stepsInterval,type="l")
max <- max(stepsInterval$steps)
max

#Imputing missing values
##calculate total number of missing data
sum(is.na(activity))
missingNum <- length(which(is.na(activity$steps)))

##Devise a strategy for filling in all of the missing values in the dataset. 
##Use the mean for that 5-minute interval
stepFit <- function(interval) {
        stepsInterval[stepsInterval$interval == interval, ]$steps
}

newDataset <- activity  # Make a new dataset with the original data
count = 0  # Count the number of data filled in
for (i in 1:nrow(newDataset)) {
        if (is.na(newDataset[i, ]$steps)) {
                newDataset[i, ]$steps <- stepFit(newDataset[i, ]$interval)
                count = count + 1
        }
}
newTotalSteps <- aggregate(steps ~ date, data = newDataset, sum)
head(newTotalSteps)
hist(newTotalSteps$steps, main = "Filled value histogram", xlab = "steps")

mean(newTotalSteps$steps)
median(newTotalSteps$steps)

#Are there differences in activity patterns between weekdays and weekends?
weekdays(newDataset$date)

newDataset$weektime <- as.factor(ifelse(weekdays(newDataset$date) %in% c("Saturday","Sunday"),
                             "weekend", "weekday"))
stepsInterval2 = aggregate(steps ~ interval + weektime, newDataset, mean)
library(lattice)
xyplot(steps ~ interval | factor(weektime), data = stepsInterval2, aspect = 1/2, 
       type = "l")
