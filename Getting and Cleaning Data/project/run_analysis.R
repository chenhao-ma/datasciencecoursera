#load packages

package <- c("data.table", "reshape2")
sapply(package, require, character.only = TRUE, quietly = TRUE)

#set path
path <- getwd()
path

#list the files in the Dataset
pathData <- file.path(path, "UCI HAR Dataset")
list.files(pathData, recursive = TRUE)

#read the subject files
dtSubjectTrain <- fread(file.path(pathData, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathData, "test", "subject_test.txt"))

#read activity files
dtActivityTrain <- fread(file.path(pathData, "train", "y_train.txt"))
dtActivityTest <- fread(file.path(pathData, "test", "y_test.txt"))

#read data files
fileToDataTable <- function(f) {
    df <- read.table(f)
    dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathData, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(pathData, "test", "X_test.txt"))

#merge data tables

dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")

dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")

dt <- rbind(dtTrain, dtTest);

#merge columns

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

#set key
setkey(dt, subject, activityNum)

#read feature numbers and names
dtFeatrues <- fread(file.path(pathData, "features.txt"))
setnames(dtFeatrues, names(dtFeatrues), c("featureNum", "featureName"))

#subset dtFeatures
dtFeatrues <- dtFeatrues[grepl("mean\\(\\)|std\\(\\)", featureName)]

#add featureCode to dtFeatures
dtFeatrues$featureCode <- dtFeatrues[, paste0("V", featureNum)]
head(dtFeatrues)
dtFeatrues$featureCode

#subset variables using variable names
select <- c(key(dt), dtFeatrues$featureCode)
dt <- dt[, select, with = FALSE]

#read activity_labels.txt
dtActivityNames <- fread(file.path(pathData, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#label with descriptive activity names

dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)

#add activityNames as a key
setkey(dt, subject, activityNum, activityName)

#melt the data table to reshape it from a short and wide format to a tall and narrow format

dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

#merge activity name

dt <- merge(dt, dtFeatrues[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)

#create new variables, activity that is equivalent to activityName as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#add new varibles to dt
grepthis <- function (regex) {
    grepl(regex, dt$feature)
}

# features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, label = c("Time", "Freq"))

x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, label = c("Accelerometer", "Gyroscope"))

x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))

x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))

# features with 1 categories
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))

# features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

#get tidy data set
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

#write tidy data set to the output.txt
write.table(dtTidy, "output.txt", row.name = FALSE)



