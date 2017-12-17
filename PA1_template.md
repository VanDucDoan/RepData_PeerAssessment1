---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
# file name
file_name <- "activity.zip"
# read file and save the data in a data frame, named as 'df'
df <- read.csv(unz(file_name, "activity.csv"), header = TRUE, sep = ",")
# display the head data in the data frame
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
# find data without missing
isGoodData <- !is.na(df[, 1]) 
# group the data according to date
dayNumSteps <- split(df[isGoodData, 1], df[isGoodData, 2])
# calculate sum of the number of steps taken each day
totNumSteps <- sapply(dayNumSteps, sum)
# make a histogram of the total number steps taken each day
hist(totNumSteps, main = "Histogram of the total number steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# calculate the mean and median
goodTotNumSteps <- numeric() 
iCount <- 0
for (i in 1:length(totNumSteps)) {
    if (length(dayNumSteps[[i]]) != 0) {
        iCount <- iCount + 1
        goodTotNumSteps[iCount] <- totNumSteps[[i]]
    }
}
# display total number of steps in days the date are taken
goodTotNumSteps
```

```
##  [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426
## [12] 15098 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355
## [23]  2492  6778 10119 11458  5018  9819 15414 10600 10571 10439  8334
## [34] 12883  3219 12608 10765  7336    41  5441 14339 15110  8841  4472
## [45] 12787 20427 21194 14478 11834 11162 13646 10183  7047
```

```r
# mean
mean(goodTotNumSteps)
```

```
## [1] 10766.19
```

```r
# median
median(goodTotNumSteps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
# group the data according to interval
intervalNumSteps <- split(df[isGoodData, 1], df[isGoodData, 3])
# calculate sum of the number of steps taken each day
totIntervalNumSteps <- sapply(intervalNumSteps, sum)
averageIntervalNumSteps <- numeric(length = length(totIntervalNumSteps))
for (i in 1:length(totIntervalNumSteps)) {
    averageIntervalNumSteps[i] <- totIntervalNumSteps[i] / length(intervalNumSteps[[i]])
}
# avarage number of steps taken
averageIntervalNumSteps
```

```
##   [1]   1.7169811   0.3396226   0.1320755   0.1509434   0.0754717
##   [6]   2.0943396   0.5283019   0.8679245   0.0000000   1.4716981
##  [11]   0.3018868   0.1320755   0.3207547   0.6792453   0.1509434
##  [16]   0.3396226   0.0000000   1.1132075   1.8301887   0.1698113
##  [21]   0.1698113   0.3773585   0.2641509   0.0000000   0.0000000
##  [26]   0.0000000   1.1320755   0.0000000   0.0000000   0.1320755
##  [31]   0.0000000   0.2264151   0.0000000   0.0000000   1.5471698
##  [36]   0.9433962   0.0000000   0.0000000   0.0000000   0.0000000
##  [41]   0.2075472   0.6226415   1.6226415   0.5849057   0.4905660
##  [46]   0.0754717   0.0000000   0.0000000   1.1886792   0.9433962
##  [51]   2.5660377   0.0000000   0.3396226   0.3584906   4.1132075
##  [56]   0.6603774   3.4905660   0.8301887   3.1132075   1.1132075
##  [61]   0.0000000   1.5660377   3.0000000   2.2452830   3.3207547
##  [66]   2.9622642   2.0943396   6.0566038  16.0188679  18.3396226
##  [71]  39.4528302  44.4905660  31.4905660  49.2641509  53.7735849
##  [76]  63.4528302  49.9622642  47.0754717  52.1509434  39.3396226
##  [81]  44.0188679  44.1698113  37.3584906  49.0377358  43.8113208
##  [86]  44.3773585  50.5094340  54.5094340  49.9245283  50.9811321
##  [91]  55.6792453  44.3207547  52.2641509  69.5471698  57.8490566
##  [96]  56.1509434  73.3773585  68.2075472 129.4339623 157.5283019
## [101] 171.1509434 155.3962264 177.3018868 206.1698113 195.9245283
## [106] 179.5660377 183.3962264 167.0188679 143.4528302 124.0377358
## [111] 109.1132075 108.1132075 103.7169811  95.9622642  66.2075472
## [116]  45.2264151  24.7924528  38.7547170  34.9811321  21.0566038
## [121]  40.5660377  26.9811321  42.4150943  52.6603774  38.9245283
## [126]  50.7924528  44.2830189  37.4150943  34.6981132  28.3396226
## [131]  25.0943396  31.9433962  31.3584906  29.6792453  21.3207547
## [136]  25.5471698  28.3773585  26.4716981  33.4339623  49.9811321
## [141]  42.0377358  44.6037736  46.0377358  59.1886792  63.8679245
## [146]  87.6981132  94.8490566  92.7735849  63.3962264  50.1698113
## [151]  54.4716981  32.4150943  26.5283019  37.7358491  45.0566038
## [156]  67.2830189  42.3396226  39.8867925  43.2641509  40.9811321
## [161]  46.2452830  56.4339623  42.7547170  25.1320755  39.9622642
## [166]  53.5471698  47.3207547  60.8113208  55.7547170  51.9622642
## [171]  43.5849057  48.6981132  35.4716981  37.5471698  41.8490566
## [176]  27.5094340  17.1132075  26.0754717  43.6226415  43.7735849
## [181]  30.0188679  36.0754717  35.4905660  38.8490566  45.9622642
## [186]  47.7547170  48.1320755  65.3207547  82.9056604  98.6603774
## [191] 102.1132075  83.9622642  62.1320755  64.1320755  74.5471698
## [196]  63.1698113  56.9056604  59.7735849  43.8679245  38.5660377
## [201]  44.6603774  45.4528302  46.2075472  43.6792453  46.6226415
## [206]  56.3018868  50.7169811  61.2264151  72.7169811  78.9433962
## [211]  68.9433962  59.6603774  75.0943396  56.5094340  34.7735849
## [216]  37.4528302  40.6792453  58.0188679  74.6981132  85.3207547
## [221]  59.2641509  67.7735849  77.6981132  74.2452830  85.3396226
## [226]  99.4528302  86.5849057  85.6037736  84.8679245  77.8301887
## [231]  58.0377358  53.3584906  36.3207547  20.7169811  27.3962264
## [236]  40.0188679  30.2075472  25.5471698  45.6603774  33.5283019
## [241]  19.6226415  19.0188679  19.3396226  33.3396226  26.8113208
## [246]  21.1698113  27.3018868  21.3396226  19.5471698  21.3207547
## [251]  32.3018868  20.1509434  15.9433962  17.2264151  23.4528302
## [256]  19.2452830  12.4528302   8.0188679  14.6603774  16.3018868
## [261]   8.6792453   7.7924528   8.1320755   2.6226415   1.4528302
## [266]   3.6792453   4.8113208   8.5094340   7.0754717   8.6981132
## [271]   9.7547170   2.2075472   0.3207547   0.1132075   1.6037736
## [276]   4.6037736   3.3018868   2.8490566   0.0000000   0.8301887
## [281]   0.9622642   1.5849057   2.6037736   4.6981132   3.3018868
## [286]   0.6415094   0.2264151   1.0754717
```

```r
# Calculate time axis
timeVal <- numeric(length = 288)
for (i in 1:288) {
    timeFormat <- strptime(sprintf("%04.0f", df[i, 3]), format = "%H%M")
    timeVal[i] <- timeFormat$hour * 60 + timeFormat$min
}
# time axis
timeVal
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55   60   65
##  [15]   70   75   80   85   90   95  100  105  110  115  120  125  130  135
##  [29]  140  145  150  155  160  165  170  175  180  185  190  195  200  205
##  [43]  210  215  220  225  230  235  240  245  250  255  260  265  270  275
##  [57]  280  285  290  295  300  305  310  315  320  325  330  335  340  345
##  [71]  350  355  360  365  370  375  380  385  390  395  400  405  410  415
##  [85]  420  425  430  435  440  445  450  455  460  465  470  475  480  485
##  [99]  490  495  500  505  510  515  520  525  530  535  540  545  550  555
## [113]  560  565  570  575  580  585  590  595  600  605  610  615  620  625
## [127]  630  635  640  645  650  655  660  665  670  675  680  685  690  695
## [141]  700  705  710  715  720  725  730  735  740  745  750  755  760  765
## [155]  770  775  780  785  790  795  800  805  810  815  820  825  830  835
## [169]  840  845  850  855  860  865  870  875  880  885  890  895  900  905
## [183]  910  915  920  925  930  935  940  945  950  955  960  965  970  975
## [197]  980  985  990  995 1000 1005 1010 1015 1020 1025 1030 1035 1040 1045
## [211] 1050 1055 1060 1065 1070 1075 1080 1085 1090 1095 1100 1105 1110 1115
## [225] 1120 1125 1130 1135 1140 1145 1150 1155 1160 1165 1170 1175 1180 1185
## [239] 1190 1195 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245 1250 1255
## [253] 1260 1265 1270 1275 1280 1285 1290 1295 1300 1305 1310 1315 1320 1325
## [267] 1330 1335 1340 1345 1350 1355 1360 1365 1370 1375 1380 1385 1390 1395
## [281] 1400 1405 1410 1415 1420 1425 1430 1435
```


```r
# plot the data
plot(timeVal, averageIntervalNumSteps,
     main = "Averaged across all days",
     xlab = "", ylab = "Average number of steps",
     col = "black",
     type = "l",
     xaxt = "n")
xsticks <- c(0 * 60, 5 * 60, 10 * 60, 15 * 60, 20 * 60)
days <- c("00:00", "05:00", "10:00", "15:00", "20:00") 
axis(side = 1, at = xsticks, labels = days, tck = -0.05)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# interval of maximum number of steps
intervalMax <- timeVal[averageIntervalNumSteps == max(averageIntervalNumSteps)]
sprintf("%02.0f:%02.0f", intervalMax %/% 60, intervalMax %% 60)
```

```
## [1] "08:35"
```


## Imputing missing values

```r
# number of missing data
sum(is.na(df[, 1])) 
```

```
## [1] 2304
```

```r
# impute the missing data by mean (the mean for 5-minute interval)
new_df <- data.frame(df)
for (i in 1:nrow(new_df)) {
    if (is.na(new_df[i, 1]) == TRUE) {
        new_df[i, 1] <- as.integer(round(averageIntervalNumSteps[((i %% 288) == 0) * 288 + (i %% 288)]))
    }
}
# display head of imputed data
head(new_df)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```


```r
# group the data according to date
new_dayNumSteps <- split(new_df[, 1], new_df[, 2])
# calculate sum of the number of steps taken each day
new_totNumSteps <- sapply(new_dayNumSteps, sum)
# make a histogram of the total number steps taken each day
hist(new_totNumSteps, main = "Histogram of the total number steps (imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# mean
mean(new_totNumSteps)
```

```
## [1] 10765.64
```

```r
# median
median(new_totNumSteps)
```

```
## [1] 10762
```

Compare with the mean and median in the first part, they slightly different (smaller than the values in the first part).


```r
# group the data according to interval
new_intervalNumSteps <- split(new_df[, 1], new_df[, 3])
# calculate average of the number of steps taken each day
new_averageIntervalNumSteps <- sapply(new_intervalNumSteps, mean)
# plot the data
matplot(timeVal, 
       cbind(averageIntervalNumSteps, new_averageIntervalNumSteps),
       main = "Averaged across all days (imputed)",
       xlab = "", ylab = "Average number of steps (imputed)",
       col = c("black", "red"),
       lwd = c(4, 1),
       lty = c(1, 1),
       type = c("l", "l"),
       xaxt = "n")
xsticks <- c(0 * 60, 5 * 60, 10 * 60, 15 * 60, 20 * 60)
days <- c("00:00", "05:00", "10:00", "15:00", "20:00") 
axis(side = 1, at = xsticks, labels = days, tck = -0.05)
legend("topright", legend = c("Missing", "Imputed"), col = c("black", "red"), lty = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
# interval of maximum number of steps
new_intervalMax <- timeVal[new_averageIntervalNumSteps == max(new_averageIntervalNumSteps)]
sprintf("%02.0f:%02.0f", intervalMax %/% 60, intervalMax %% 60)
```

```
## [1] "08:35"
```
Since imputation of data uses the mean of 5-minute interval, the plot of averaged number across all days is insignificantly changed.

## Are there differences in activity patterns between weekdays and weekends?

```r
# add a column of day
new_df$day <- weekdays(strptime(as.character(new_df[, 2]), format = "%Y-%m-%d"))
for (i in 1:nrow(new_df)) {
    if ((new_df$day[i] != "Saturday") & (new_df$day[i] != "Sunday"))
        new_df$dayType[i] <- "weekday"
    else
        new_df$dayType[i] <- "weekend"
}
# factor variable
new_df$dayType <- factor(new_df$dayType)
```


```r
# split data
weekdayData <- split(new_df[new_df[, 5] == "weekday", 1], new_df[new_df[, 5] == "weekday", 3])
weekendData <- split(new_df[new_df[, 5] == "weekend", 1], new_df[new_df[, 5] == "weekend", 3])
# calculate average of the number of steps taken each day
wkd_averageIntervalNumSteps <- sapply(weekdayData, mean)
wke_averageIntervalNumSteps <- sapply(weekendData, mean)
```


```r
par(mfrow = c(2, 1), 
    oma = c(0, 0, 0, 0),
    omd = c(0, 0, 0, 0),
    omi = c(0, 0, 0, 0),
    mai = c(0.8, 0.8, 0.2, 0.2))

# Plot 2.1: Plot of weekend data
plot(timeVal, wke_averageIntervalNumSteps,
     main = "Averaged across all weekends",
     xlab = "", ylab = "Average number of steps",
     col = "black",
     type = "l",
     xaxt = "n")
xsticks <- c(0 * 60, 5 * 60, 10 * 60, 15 * 60, 20 * 60)
days <- c("00:00", "05:00", "10:00", "15:00", "20:00") 
axis(side = 1, at = xsticks, labels = days, tck = -0.05)

# Plot 4.2: Plot of weekday data
plot(timeVal, wkd_averageIntervalNumSteps,
     main = "Averaged across all weekdays",
     xlab = "", ylab = "Average number of steps",
     col = "black",
     type = "l",
     xaxt = "n")
xsticks <- c(0 * 60, 5 * 60, 10 * 60, 15 * 60, 20 * 60)
days <- c("00:00", "05:00", "10:00", "15:00", "20:00") 
axis(side = 1, at = xsticks, labels = days, tck = -0.05)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
