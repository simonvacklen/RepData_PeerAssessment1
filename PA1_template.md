# Coursera Reproducible Research: Peer Assessment 1

---





Importing data

```r
activity <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

```r
day_sum <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(day_sum) <- c("date", "total_steps")
hist(day_sum$total_steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(day_sum$total_steps)
```

```
## [1] 9354.23
```

```r
median(day_sum$total_steps)
```

```
## [1] 10395
```

What is the average daily activity pattern?

```r
interval_mean <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
names(interval_mean) <- c("interval", "total_steps")
plot(interval_mean$interval, interval_mean$total_steps, type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max_mean <- max(interval_mean$total_steps)
interval_mean[interval_mean$total_steps %in% max_mean, 1]
```

```
## [1] 835
```
	
## Imputing missing values

Total number of missing values in the dataset

```r
nrow(as.matrix(null_steps[null_steps==TRUE]))
```

```
## Error in as.matrix(null_steps[null_steps == TRUE]): object 'null_steps' not found
```

Here i have chosen to replace the missing values with the means of the whole dataset

```r
activity_imp <- activity
activity_imp$steps[is.na(activity_imp$steps)] <- mean(activity_imp$steps, na.rm=TRUE)
```

After imputing, the histogram is as one would expect, more clustered around the middle. Here i include the original un-imputed dataset
with the imputed one for easier comparison

```r
day_sum_imp <- aggregate(activity_imp$steps, by = list(activity_imp$date), FUN = sum)
names(day_sum_imp) <- c("date", "total_steps")
par(mfrow=c(1,2))
hist(day_sum_imp$total_steps, main="Imputed")
hist(day_sum$total_steps, main ="Not Imputed")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Mean before imputation:

```r
mean(day_sum$total_steps)
```

```
## [1] 9354.23
```

Mean after imputation:

```r
mean(day_sum_imp$total_steps)
```

```
## [1] 10766.19
```

Median before imputation:

```r
median(day_sum$total_steps)
```

```
## [1] 10395
```

Median after imputation:

```r
median(day_sum_imp$total_steps)
```

```
## [1] 10766.19
```

Both the median and the mean of the steps per day increased after imputation, not surprisingly since the imputed values adds to the total
number of steps per day. Mean and median per 5-minute interval would not necessarily have increased.


# Are there differences in activity patterns between weekdays and weekends?

To get the weekdays in English i have to set the locale

```r
Sys.setlocale("LC_ALL", "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```


Here the variable "Weekdays" is created, and also the variable "Weekend" with its two factors "Weekend" and "Weekday".

```r
activity_imp$weekday <- weekdays(as.Date(activity_imp$date, format = "%Y-%m-%d"))
activity_imp$weekend[activity_imp$weekday %in% c("Saturday", "Sunday")] <- c("Weekend")
activity_imp$weekend[is.na(activity_imp$weekend)] <- c("Weekday")
```

To check the created variables let look at a crosstabulation:

```r
table(activity_imp$weekday, activity_imp$weekend)
```

```
##            
##             Weekday Weekend
##   Friday       2592       0
##   Monday       2592       0
##   Saturday        0    2304
##   Sunday          0    2304
##   Thursday     2592       0
##   Tuesday      2592       0
##   Wednesday    2592       0
```

# Graphs to see the difference in mean between weekends and weekdays

First I create a dataset for each graph

```r
plot_wd <- subset(activity_imp, activity_imp$weekend=="Weekday")
plot_we <- subset(activity_imp, activity_imp$weekend=="Weekend")
```

Then I create datasets with the mean for each interval

```r
wd_mean <- aggregate(plot_wd$steps, by = list(plot_wd$interval), FUN = mean)
names(wd_mean) <- c("date", "mean_steps")
we_mean <- aggregate(plot_we$steps, by = list(plot_we$interval), FUN = mean)
names(we_mean) <- c("date", "mean_steps")
```

And finally the graphs themselves;

```r
par(mfrow=c(2,1))
with(wd_mean, plot(date, mean_steps, type="l", main="Weekday"))
with(we_mean, plot(date, mean_steps, type="l", main="Weekend"))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 
