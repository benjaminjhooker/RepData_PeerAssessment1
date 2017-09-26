Exploratory Data Week 2 Project
================
Benjamin J. Hooker
September 25, 2017

Load packages for downloading files. I find the `curl` pacakge to be quite helpful when downloading "https" files

``` r
library(curl)
library(data.table)
```

Download the file, unzip it, and then read it with `read.csv` function to a variable called "data"

``` r
file_url <- ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")  
curl_download(file_url, destfile = "./data/week2_proj.zip")  
data <- read.csv(unzip("./data/week2_proj.zip"))
```

### Building the histogram:

A simple histogram of the data using the `hist` function:

``` r
hist(data$steps, main = "Historam of Steps", xlab = "Number of Steps")
```

![](PA1_template_files/figure-markdown_github/simplehist-1.png)

When exploring the data using the `table` function, there are 11K+ 0's recorded by the device, so it may make more sense to extract the non-zero numbers using the `which` function.

``` r
hist(which(data$steps > 0), col = "aquamarine", xlab = "Number of Steps",main = "Histogram of non-zero steps")
```

![](PA1_template_files/figure-markdown_github/thehistogram-1.png)

### Calculating the mean and median:

This is probably a good time to use the `tapply` function to generate some data- frames and then perform a `cbind` to make one nice table.

``` r
data_average <- as.data.frame(with(data, tapply(steps, date, mean)))
data_med <- as.data.frame(with(data, tapply(steps, date, median)))
data_table <- cbind(data_average, data_med)
colnames(data_table) <- c("Average", "Median")
data_table
```

    ##               Average Median
    ## 2012-10-01         NA     NA
    ## 2012-10-02  0.4375000      0
    ## 2012-10-03 39.4166667      0
    ## 2012-10-04 42.0694444      0
    ## 2012-10-05 46.1597222      0
    ## 2012-10-06 53.5416667      0
    ## 2012-10-07 38.2465278      0
    ## 2012-10-08         NA     NA
    ## 2012-10-09 44.4826389      0
    ## 2012-10-10 34.3750000      0
    ## 2012-10-11 35.7777778      0
    ## 2012-10-12 60.3541667      0
    ## 2012-10-13 43.1458333      0
    ## 2012-10-14 52.4236111      0
    ## 2012-10-15 35.2048611      0
    ## 2012-10-16 52.3750000      0
    ## 2012-10-17 46.7083333      0
    ## 2012-10-18 34.9166667      0
    ## 2012-10-19 41.0729167      0
    ## 2012-10-20 36.0937500      0
    ## 2012-10-21 30.6284722      0
    ## 2012-10-22 46.7361111      0
    ## 2012-10-23 30.9652778      0
    ## 2012-10-24 29.0104167      0
    ## 2012-10-25  8.6527778      0
    ## 2012-10-26 23.5347222      0
    ## 2012-10-27 35.1354167      0
    ## 2012-10-28 39.7847222      0
    ## 2012-10-29 17.4236111      0
    ## 2012-10-30 34.0937500      0
    ## 2012-10-31 53.5208333      0
    ## 2012-11-01         NA     NA
    ## 2012-11-02 36.8055556      0
    ## 2012-11-03 36.7048611      0
    ## 2012-11-04         NA     NA
    ## 2012-11-05 36.2465278      0
    ## 2012-11-06 28.9375000      0
    ## 2012-11-07 44.7326389      0
    ## 2012-11-08 11.1770833      0
    ## 2012-11-09         NA     NA
    ## 2012-11-10         NA     NA
    ## 2012-11-11 43.7777778      0
    ## 2012-11-12 37.3784722      0
    ## 2012-11-13 25.4722222      0
    ## 2012-11-14         NA     NA
    ## 2012-11-15  0.1423611      0
    ## 2012-11-16 18.8923611      0
    ## 2012-11-17 49.7881944      0
    ## 2012-11-18 52.4652778      0
    ## 2012-11-19 30.6979167      0
    ## 2012-11-20 15.5277778      0
    ## 2012-11-21 44.3993056      0
    ## 2012-11-22 70.9270833      0
    ## 2012-11-23 73.5902778      0
    ## 2012-11-24 50.2708333      0
    ## 2012-11-25 41.0902778      0
    ## 2012-11-26 38.7569444      0
    ## 2012-11-27 47.3819444      0
    ## 2012-11-28 35.3576389      0
    ## 2012-11-29 24.4687500      0
    ## 2012-11-30         NA     NA

### Plotting steps by interval

Here again I will use the `tapply` function to generate the time-series analysis to try to discover the mean of the steps taken at a given time interval for each day. I had to use an `na.omit` function earlier than expected to display a functional plot.

``` r
interval_average <- as.data.frame(with(na.omit(data), tapply(steps, interval, mean)))  
plot(interval_average, type = "l", col = 4, xlab = "Time interval (by 5 min)",ylab = "Average Steps", main = "Steps by Time interval")
```

![](PA1_template_files/figure-markdown_github/the%20plot%20by%20interval-1.png)

I think I have a fairly good idea of where the highest activity interval is, but I will run an `order` function on the new data frame to see which interval had the greatest number of steps.

``` r
df <- interval_average[order(-interval_average),]
head(df)
```

    ##      835      840      850      845      830      820 
    ## 206.1698 195.9245 183.3962 179.5660 177.3019 171.1509

### Missing Data

Calculating the number of "NAs"

``` r
sum(is.na(data))
```

    ## [1] 2304

### Making a data set without "NAs"

Here I will use the `na.omit` and create a data set that is free of NA's.

``` r
data1 <- na.omit(data)
sum(is.na(data1)) ## Check to ensure there are no "NAs"
```

    ## [1] 0

### Making another Histogram

With NA's removed, I will remake the previous histograms, but it might be more useful to compare them side by side:

``` r
par(mfrow = c(2,2))
hist(data$steps,main = "Historam of Steps Data", xlab = "Number of Steps")
hist(data1$steps, col = 3, main = "Historam of Steps Data1",xlab = "Number of Steps")
hist(which(data$steps > 0), col = "aquamarine", xlab = "Number of Steps",main = "Histogram of non-zero steps Data")
hist(which(data1$steps > 0), col = "blue", xlab = "Number of Steps",main = "Histogram of non-zero steps Data1")
```

![](PA1_template_files/figure-markdown_github/histoplots-1.png)

### Another Table

Here I will use the `tapply` function again, but the code will use the "data1" set instead of the original "data" set.

``` r
data_average1 <- as.data.frame(with(data1, tapply(steps, date, mean)))
data_med1 <- as.data.frame(with(data1, tapply(steps, date, median)))
data_table1 <- cbind(data_average1, data_med1)
colnames(data_table1) <- c("Average1", "Median1")
data_table1 <- na.omit(data_table1)
data_table1
```

    ##              Average1 Median1
    ## 2012-10-02  0.4375000       0
    ## 2012-10-03 39.4166667       0
    ## 2012-10-04 42.0694444       0
    ## 2012-10-05 46.1597222       0
    ## 2012-10-06 53.5416667       0
    ## 2012-10-07 38.2465278       0
    ## 2012-10-09 44.4826389       0
    ## 2012-10-10 34.3750000       0
    ## 2012-10-11 35.7777778       0
    ## 2012-10-12 60.3541667       0
    ## 2012-10-13 43.1458333       0
    ## 2012-10-14 52.4236111       0
    ## 2012-10-15 35.2048611       0
    ## 2012-10-16 52.3750000       0
    ## 2012-10-17 46.7083333       0
    ## 2012-10-18 34.9166667       0
    ## 2012-10-19 41.0729167       0
    ## 2012-10-20 36.0937500       0
    ## 2012-10-21 30.6284722       0
    ## 2012-10-22 46.7361111       0
    ## 2012-10-23 30.9652778       0
    ## 2012-10-24 29.0104167       0
    ## 2012-10-25  8.6527778       0
    ## 2012-10-26 23.5347222       0
    ## 2012-10-27 35.1354167       0
    ## 2012-10-28 39.7847222       0
    ## 2012-10-29 17.4236111       0
    ## 2012-10-30 34.0937500       0
    ## 2012-10-31 53.5208333       0
    ## 2012-11-02 36.8055556       0
    ## 2012-11-03 36.7048611       0
    ## 2012-11-05 36.2465278       0
    ## 2012-11-06 28.9375000       0
    ## 2012-11-07 44.7326389       0
    ## 2012-11-08 11.1770833       0
    ## 2012-11-11 43.7777778       0
    ## 2012-11-12 37.3784722       0
    ## 2012-11-13 25.4722222       0
    ## 2012-11-15  0.1423611       0
    ## 2012-11-16 18.8923611       0
    ## 2012-11-17 49.7881944       0
    ## 2012-11-18 52.4652778       0
    ## 2012-11-19 30.6979167       0
    ## 2012-11-20 15.5277778       0
    ## 2012-11-21 44.3993056       0
    ## 2012-11-22 70.9270833       0
    ## 2012-11-23 73.5902778       0
    ## 2012-11-24 50.2708333       0
    ## 2012-11-25 41.0902778       0
    ## 2012-11-26 38.7569444       0
    ## 2012-11-27 47.3819444       0
    ## 2012-11-28 35.3576389       0
    ## 2012-11-29 24.4687500       0

### Making Weekends

First I need to convert the information in the data set to acutal dates using the `as.Date` function in conjunction with `as.character` function and the "format = '%Y-%m-%d'" option.

``` r
data1$date <- as.Date(as.character(data1$date), "%Y-%m-%d")
```

Now I need to create a factor variable in another column to create a table to diferentiate between the two types of days.

``` r
data1[,4] <- weekdays(data1$date)
for(i in 1:nrow(data1)){
        if (data1[i,4]== "Saturday" | data1[i,4]== "Sunday")
        {data1[i,5]<-"Weekend"}
        else {data1[i,5]<-"weekday"
                }
}
```

Weekend factor created, now it is time to make it a factor.

``` r
data1$V5 <- as.factor(data1$V5)
names(data1)[5] <- "Weekend"
```

Now it is time to subset the data into "Weekdays" and "Weekends"

``` r
weekend <- data1[data1$Weekend == "Weekend",]
weekday <- data1[data1$Weekend == "weekday",]
```

Run some `tapply` functions like earlier on the new data frames and then plot the results per the assignment.

``` r
interval_average1 <- as.data.frame(with(na.omit(weekday), tapply(steps, interval,mean)))
interval_average2 <- as.data.frame(with(na.omit(weekend), tapply(steps, interval,mean)))
par(mfrow = c(2,1))
plot(interval_average1, type = "l", col = 2, xlab = "Intervals (5 min)",ylab = "Average number of steps", main = "Weekday steps")
plot(interval_average2, type = "l", col = 4, xlab = "Intervals (5 min)",ylab = "Average number of steps", main = "Weekend steps")
```

![](PA1_template_files/figure-markdown_github/plotting%20again-1.png)
