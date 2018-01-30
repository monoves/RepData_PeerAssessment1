Analysis of Personal Movement using Activity Monitoring Devices
===============================================================

Synopsis:
=========

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
??quantified self?? movement ?? a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data. This code makes use of data from a personal
activity monitoring device by analyzing total steps, average steps in
five minute intervals, as well as compared activity levels between
weekdays and weekends. This device collects data at 5 minute intervals
through out the day. The data consists of two months of data from an
anonymous individual collected during the months of October and
November, 2012 and include the number of steps taken in 5 minute
intervals each day.

Data Processing:
================

I manually set the working directory The code refining and
checking/processing the data in the format that I need

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    activitydata <- read.csv("activity.csv", header=TRUE, sep = ",", na.strings = "NA")
    summary(activitydata)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    activitydata$date <- as.Date(activitydata$date, format = "%Y-%m-%d")
    activitydata$interval <- factor(activitydata$interval)

### The total number of steps taken is summed up

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    NAs <- is.na(as.character(activitydata$steps))
    activitydata_noNA <- activitydata[!NAs,]
    head(activitydata_noNA)

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25

    dailysteps <- aggregate(steps ~ date, data = activitydata_noNA, sum)
    colnames(dailysteps) <- c("date", "steps")

### A histogram of the total number of steps taken each day

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    hist(as.numeric(dailysteps$steps), col = "blue", border = "green", xlab = "Steps Number", ylab = "Frequency", main = "Total Number of Steps Histogram")

![](test_files/figure-markdown_strict/unnamed-chunk-3-1.png) \#\#\# Mean
and median are calculated

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    mean(dailysteps$steps)

    ## [1] 10766.19

    median(dailysteps$steps)

    ## [1] 10765

### The average daily activity pattern is calculated in five minute intervals, and then a time plot of average number of steps taken is plotted

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    averageintervalsteps <- aggregate(steps ~ interval, data = activitydata_noNA, mean, na.rm = TRUE)
    colnames(averageintervalsteps) <- c("interval", "steps")

    plot(steps ~ interval, data = averageintervalsteps, type = "l", xlab = "Intervals", ylab = "Average Steps per Interval", main = "Average Steps In Intervals", col = "red")

![](test_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### The maximum number of average steps in a five minute interval

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    stepsmax <- max(averageintervalsteps$steps)
    intervalmaxsteps <- averageintervalsteps[which.max(averageintervalsteps$steps),]$interval
    intervalmaxsteps

    ## [1] 835
    ## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355

### Find the missing values, and impute the missing values with the mean of five minute intervals

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    sum(is.na(as.character(activitydata$steps)))

    ## [1] 2304

    sum(is.na(as.character(activitydata$date)))

    ## [1] 0

    sum(is.na(as.character(activitydata$interval)))

    ## [1] 0

    NAs <- which(is.na(as.character(activitydata$steps)))

    meansteps <- function(interval){
      averageintervalsteps[averageintervalsteps$interval==interval, "steps"]
    }
    allactivitydata <- activitydata
    values = 0
    for (i in 1:nrow(allactivitydata)) {
      if (is.na(allactivitydata[i, "steps"])) {
        allactivitydata[i,"steps"] <- meansteps(allactivitydata[i, "interval"])
        values = values + 1
      }
    }
    summary(allactivitydata)

    ##      steps             date               interval    
    ##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
    ##  Median :  0.00   Median :2012-10-31   10     :   61  
    ##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
    ##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
    ##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
    ##                                        (Other):17202

    str(allactivitydata)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

### A histogram of total steps with NAs filled is plotted

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    totalsteps <- aggregate(steps ~ date, data = allactivitydata, sum)
    colnames(totalsteps) <- c("date", "steps")
    hist(as.numeric(totalsteps$steps), xlab = "Steps Number", ylab="Frequency", main = "Total Number of Steps with filled NAs", col = "yellow")

![](test_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    mean(totalsteps$steps)

    ## [1] 10766.19

    median(totalsteps$steps)

    ## [1] 10766.19

### The difference between weekdays and weekends is plotted on a paneled xy plot for comparison

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    library(lattice)
    allactivitydata$day <- ifelse(as.POSIXlt(as.Date(allactivitydata$date))$wday%%6 == 0, "weekend", "weekday")
    allactivitydata$day <- factor(allactivitydata$day, levels = c("weekday", "weekend"))
    newset = aggregate (steps ~ interval + day, allactivitydata, mean)
    xyplot(steps ~ interval | factor(day), data = newset, aspect = 1/2, type = "l")

![](test_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Results
=======

The results seem to differ greatly from one another when comparing the
weekdays and the weekends. The weekends seem to have more consistent
activity and peaks ranging from just below to 150 to 200 steps. The
weekends also appear to have activity that begins earlier than during
the weekdays. The weekday only has one prominent peaks that reaches
above 200 steps. Then it sharply moves down and remains consistently
peaking from 50 to 100 steps at a time. All in all, the data seems to
vary, but both depict a good amount of activity throughout the days.

    setwd("~/Monica/DataScience/ReproducibleResearch/repdata%2Fdata%2Factivity")
    library(rmarkdown)

    ## Warning: package 'rmarkdown' was built under R version 3.4.3

    sink("PA1_template.Rmd")
    rmarkdown::render("PA1_template.Rmd")

    ## "C:/Program Files/RStudio/bin/pandoc/pandoc" +RTS -K512m -RTS PA1_template.utf8.md --to html --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output PA1_template.html --smart --email-obfuscation none --self-contained --standalone --section-divs --template "C:\Users\Monica\Documents\R\R-3.4.1\library\rmarkdown\rmd\h\default.html" --no-highlight --variable highlightjs=1 --variable "theme:bootstrap" --include-in-header "C:\Users\Monica\AppData\Local\Temp\RtmpOCQORb\rmarkdown-str24e8dd45a3.html" --mathjax --variable "mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

    ## 
    ## Output created: PA1_template.html
