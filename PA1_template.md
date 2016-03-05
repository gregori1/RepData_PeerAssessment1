# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
library(ggplot2)
unzip(zipfile = "activity.zip")
activity <- read.csv("activity.csv")
kroki <- activity$steps
dzien <- activity$date
inter <- activity$interval
activity2 <- activity[!is.na(activity$steps),]
agregacja <- aggregate(list(kroki = kroki), list(dzien = dzien), FUN=sum)
agregacjaZERO <- agregacja
agregacjaZERO[is.na(agregacjaZERO)] <- 0
agregacjaNA <- agregacja[!is.na(agregacja$kroki),]
```
## What is mean total number of steps taken per day?

```r
## I use data without NA, but including 'zero-step' days

ggplot(data=agregacjaNA, aes(agregacjaNA$kroki))+geom_histogram(breaks=seq(0,25000,by=5000),col="blue",aes(fill=..count..))+scale_fill_gradient("count", low="white", high="red")+ggtitle("Histogram total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)\

```r
summary(agregacjaNA$kroki)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

```r
agregacjaAVG <- mean(agregacjaNA$kroki)
agregacjaMEDIAN <- median(agregacjaNA$kroki)

agregacjaAVG  # mean number of steps taken per day
```

```
## [1] 10766.19
```

```r
agregacjaMEDIAN  # median number of steps taken per day
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
ggplot(data = agregacjaNA, aes(x=dzien, y=kroki))+geom_bar(stat = "identity", fill="steelblue")+geom_text(aes(label=kroki),vjust=-0.3, size=3.5)+theme_minimal()+theme(axis.text.x = element_text(angle=90,hjust = 1))+ggtitle("Total number of steps per day (without zero-step days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

```r
ggplot(data = agregacjaZERO, aes(x=dzien, y=kroki))+geom_bar(stat = "identity", fill="steelblue")+geom_text(aes(label=kroki),vjust=-0.3, size=3.5)+theme_minimal()+theme(axis.text.x = element_text(angle=90,hjust = 1))+ggtitle("Total number of steps per day (with zero-step days)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-2.png)\
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
