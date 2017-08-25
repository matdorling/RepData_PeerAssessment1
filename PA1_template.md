# Assignment 1
M Dorling  
23 August 2017  

## Part 1 - Loading the data


```r
df <- read.csv(file="activity.csv")
df$date <- as.Date(df$date)
```

## Part 2 - calculate total steps taken per day


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
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
# Group data by day and sum steps for each day
daily_total_steps <- df %>% group_by(df$date) %>% summarise(sum(steps))
```

```
## Warning: package 'bindrcpp' was built under R version 3.2.5
```

Below is a histogram of total daily steps, ignoring missing values:


```r
hist(daily_total_steps$`sum(steps)`, breaks=15, main="Total daily steps", xlab="Total daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The mean and median total daily steps are calculated next:


```r
meansteps = mean(daily_total_steps$`sum(steps)`, na.rm = T)
print(sprintf("Mean daily steps =  %f", meansteps))
```

```
## [1] "Mean daily steps =  10766.188679"
```

```r
mediansteps = median(daily_total_steps$`sum(steps)`, na.rm = T)
print(sprintf("Median daily steps = %f", mediansteps))
```

```
## [1] "Median daily steps = 10765.000000"
```

## Part 3 average daily activity

First, group the data by 5-minute interval


```r
# Group data by day and sum steps for each day
group_by_interval <- df %>% group_by(df$interval) %>% summarise(mean(steps, na.rm=T))
```

And produce a line plot:


```r
plot(group_by_interval$`df$interval`, group_by_interval$`mean(steps, na.rm = T)`, type="l", main="Average steps per 5-minute interval", xlab="Interval", ylab="Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

And calculate the 5-minute interval with maximum average steps:


```r
# Calculate maximum interval by steps
maxinterval = max(group_by_interval$`mean(steps, na.rm = T)`)

# Find which interval this value corresponds to
interval <- which(group_by_interval$`mean(steps, na.rm = T)`==maxinterval)

print(sprintf("Interval with max average steps =  %s", group_by_interval$`df$interval`[interval]))
```

```
## [1] "Interval with max average steps =  835"
```

## Imputation

The missing data will be imputed by taking the mean value for the 5-minute interval in question and inserting that into the blank value.


```r
# Impute missing data with the mean for that interval

# make a copy of the data for imputation
df_impute <- df

# loop over all samples and if NA is found, replace with appropriae value from averaged data

for(i in 1:length(df_impute$steps)){
  if(is.na(df_impute$steps[i]==TRUE)){
    # Get the interval number of the missing value
    missingInterval <- df_impute$interval[i]
    # Replace the missing value with the corresponding mean value
    imputevalue <- group_by_interval$`mean(steps, na.rm = T)`[which(group_by_interval$`df$interval`==missingInterval)]
    df_impute$steps[i] = imputevalue
  }
}
```

Now we repeat the histogram and mean/median calculations, using imputed data.


```r
# Group imputed data by day and sum steps for each day
daily_total_steps_imputed <- df_impute %>% group_by(df_impute$date) %>% summarise(sum(steps))
```

Below is a histogram of total daily steps, using imputed data:


```r
hist(daily_total_steps_imputed$`sum(steps)`, breaks=15, main="Total daily steps after imputation", xlab="Total daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The mean and median total daily steps are calculated next, using imputed data:


```r
meansteps_imputed = mean(daily_total_steps_imputed$`sum(steps)`, na.rm = T)
print(sprintf("Mean daily steps after imputation =  %f", meansteps_imputed))
```

```
## [1] "Mean daily steps after imputation =  10766.188679"
```

```r
mediansteps_imputed = median(daily_total_steps_imputed$`sum(steps)`, na.rm = T)
print(sprintf("Median daily steps = %f", mediansteps_imputed))
```

```
## [1] "Median daily steps = 10766.188679"
```

## Weekdays vs Weekends

We'd like to create a plot of averaged steps for each time interval, split into weekends versus weekdays.

First, create a factor variable for weekend versus weekday:


```r
library(chron)
```

```
## Warning: package 'chron' was built under R version 3.2.5
```

```r
# Use is.weekend function from chron package to return TRUE or FALSE for weekends, and add as a column to dataframe.
df_impute$weekend <- is.weekend(df_impute$date)

# separate the weekends and weekdays

df.weekdays <- df_impute[df_impute$weekend==FALSE,]
df.weekends <- df_impute[df_impute$weekend==TRUE,]

# now group by interval and average as before

group_by_interval.weekday <- df.weekdays %>% group_by(df.weekdays$interval) %>% summarise(mean(steps, na.rm=T))

group_by_interval.weekends <- df.weekends %>% group_by(df.weekends$interval) %>% summarise(mean(steps, na.rm=T))
```

Now make a panel plot with lattice


```r
# Now make a panel plot with lattice
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.5
```

```r
weekday.plot <- xyplot(group_by_interval.weekday$`mean(steps, na.rm = T)` ~ group_by_interval.weekday$`df.weekdays$interval`, type="l", ylab="avg steps", xlab="interval", main="weekdays")

weekend.plot <- xyplot(group_by_interval.weekends$`mean(steps, na.rm = T)` ~ group_by_interval.weekends$`df.weekends$interval`, type="l", ylab="avg steps", xlab="interval", main="weekends")

print(weekday.plot, position=c(0, .5, 1, 1), more=TRUE)
print(weekend.plot, position=c(0, 0, 1, .5))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
