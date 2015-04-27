#Codebook

##Variable list and description

Variable name    | Variable description
-----------------|------------
subject          | ID of the subject who performed the activity for each window sample. Its range is from 1 to 30.
activity         | Activity name
featDomain       | Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument   | Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration | Feature: Acceleration signal (Body or Gravity)
featVariable     | Feature: Variable (Mean or SD)
featJerk         | Feature: Jerk signal
featMagnitude    | Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis         | Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount        | Feature: Count of data points used to compute `average`
featAverage      | Feature: Average of each variable for each activity and each subject

##Structure of the tidy data set
```R
> str(dtTidy) # dtTidy: tidy data set

Classes ‘data.table’ and 'data.frame':  11880 obs. of  11 variables:
 $ subject         : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity        : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ featDomain      : Factor w/ 2 levels "Time","Freq": 1 1 1 1 1 1 1 1 1 1 ...
 $ featAcceleration: Factor w/ 3 levels NA,"Body","Gravity": 1 1 1 1 1 1 1 1 1 1 ...
 $ featInstrument  : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ featJerk        : Factor w/ 2 levels NA,"Jerk": 1 1 1 1 1 1 1 1 2 2 ...
 $ featMagnitude   : Factor w/ 2 levels NA,"Magnitude": 1 1 1 1 1 1 2 2 1 1 ...
 $ featVariable    : Factor w/ 2 levels "Mean","SD": 1 1 1 2 2 2 1 2 1 1 ...
 $ featAxis        : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
 $ count           : int  50 50 50 50 50 50 50 50 50 50 ...
 $ average         : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
 - attr(*, "sorted")= chr  "subject" "activity" "featDomain" "featAcceleration" ...
 - attr(*, ".internal.selfref")=<externalptr>
```

##Keys of the tidy data set

```R
> key(dtTidy) # dtTidy: tidy data set

[1] "subject"          "activity"         "featDomain"       "featAcceleration" "featInstrument"
[6] "featJerk"         "featMagnitude"    "featVariable"     "featAxis"
```

##Summary of the tidy data set

```R
> summary(dtTidy)
    subject                   activity    featDomain  featAcceleration       featInstrument
 Min.   : 1.0   LAYING            :1980   Time:7200   NA     :4680     Accelerometer:7200
 1st Qu.: 8.0   SITTING           :1980   Freq:4680   Body   :5760     Gyroscope    :4680
 Median :15.5   STANDING          :1980               Gravity:1440
 Mean   :15.5   WALKING           :1980
 3rd Qu.:23.0   WALKING_DOWNSTAIRS:1980
 Max.   :30.0   WALKING_UPSTAIRS  :1980
 featJerk      featMagnitude  featVariable featAxis      count          average
 NA  :7200   NA       :8640   Mean:5940    NA:3240   Min.   :36.00   Min.   :-0.99767
 Jerk:4680   Magnitude:3240   SD  :5940    X :2880   1st Qu.:49.00   1st Qu.:-0.96205
                                           Y :2880   Median :54.50   Median :-0.46989
                                           Z :2880   Mean   :57.22   Mean   :-0.48436
                                                     3rd Qu.:63.25   3rd Qu.:-0.07836
                                                     Max.   :95.00   Max.   : 0.97451

```

## Head rows of the tidy data set

```R

> head(dtTidy)

   subject activity featDomain featAcceleration featInstrument featJerk featMagnitude featVariable
1:       1   LAYING       Time               NA      Gyroscope       NA            NA         Mean
2:       1   LAYING       Time               NA      Gyroscope       NA            NA         Mean
3:       1   LAYING       Time               NA      Gyroscope       NA            NA         Mean
4:       1   LAYING       Time               NA      Gyroscope       NA            NA           SD
5:       1   LAYING       Time               NA      Gyroscope       NA            NA           SD
6:       1   LAYING       Time               NA      Gyroscope       NA            NA           SD
   featAxis count     average
1:        X    50 -0.01655309
2:        Y    50 -0.06448612
3:        Z    50  0.14868944
4:        X    50 -0.87354387
5:        Y    50 -0.95109044
6:        Z    50 -0.90828466

```
