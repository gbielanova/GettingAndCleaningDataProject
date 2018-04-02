# Task

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.

# load the packages
packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# please set up working directory to the folder where git was cloned, 
# uncomment next line and add your path if setwd wasn't run before
#setwd("c:\\...\\GettingAndCleaningDataProject\\")
path = getwd()

# check that original files exist
if (length(list.files(file.path(path, "UCI HAR Dataset"), recursive=TRUE)) != 28){
  stop("Please check if working directory set up correctly (setwd) or repo cloned fully.")
}

# read subject files
dtSubjectTrain <- data.table(read.table(file.path(path, "UCI HAR Dataset\\train", "subject_train.txt")))
dtSubjectTest <- data.table(read.table(file.path(path, "UCI HAR Dataset\\test", "subject_test.txt")))

# read activity files
dtActivityTrain <- data.table(read.table(file.path(path, "UCI HAR Dataset\\train", "Y_train.txt")))
dtActivityTest <- data.table(read.table(file.path(path, "UCI HAR Dataset\\test", "Y_test.txt")))

# read measures files
dtMeasuresTrain <- data.table(read.table(file.path(path, "UCI HAR Dataset\\train", "X_train.txt")))
dtMeasuresTest <- data.table(read.table(file.path(path, "UCI HAR Dataset\\test", "X_test.txt")))

# merge data tables 
dtSubjects <- rbind(dtSubjectTrain, dtSubjectTest)
dtActivities <- rbind(dtActivityTrain, dtActivityTest)
dtMeasures <- rbind(dtMeasuresTrain, dtMeasuresTest)

# set names
setnames(dtSubjects, "V1", "subject")
setnames(dtActivities, "V1", "activityNum")

# merge columns
dtSubjectActivities <- cbind(dtSubjects, dtActivities)
dt <- cbind(dtSubjectActivities, dtMeasures)

# set key
setkey(dt, subject, activityNum)

# read features
dtFeatures <- data.table(read.table(file.path(path, "UCI HAR Dataset", "features.txt")))
setnames(dtFeatures, c("V1", "V2"), c("measureNum", "measureName"))

# Use grepl to just get features/measures related to mean and std
dtMeanStd <- dtFeatures[grepl("(mean|std)\\(\\)", measureName)]
# Create a column to 'index/cross reference' into the 'measure' headers
dtMeanStd$code <- dtMeanStd[, paste0("V", measureNum)]

# Build up the columns to select from the data.table,
# dtSubjectActivitiesWithMeasures
columns <- c(key(dt), dtMeanStd$code)
# Just take the rows with the columns of interest ( std() and mean() )
dtMeanStd <- subset(dt, select = columns)

# Read in the activity names and give them more meaningful names
dtActivityNames <- data.table(read.table(file.path(path, "UCI HAR Dataset", "activity_labels.txt")))
setnames(dtActivityNames, c("V1", "V2"), c("activityNum", "activityName"))

# Merge the activities names with the dtMeanStd
dtMeanStd <- merge(dtMeanStd, dtActivityNames, by = "activityNum", all.x = TRUE)

# Sort the dtMeanStd
setkey(dtMeanStd, subject, activityNum, activityName)

# Convert from a wide to narrow data.table using the keys created earlier
dtMeanStd <- data.table(melt(dtMeanStd, id=c("subject", "activityName"), measure.vars = c(3:68),
                             variable.name = "code", value.name="measureValue"))

# Merge measure codes
dtMeanStd <- merge(dtMeanStd, dtMeanStdMeasures[, list(measureNum, code, measureName)], by="code", all.x=TRUE)

# Convert activityName and measureName to factors
dtMeanStd$activityName <- factor(dtMeanStd$activityName)
dtMeanStd$measureName <- factor(dtMeanStd$measureName)

# Reshape the data to get the averages 
measureAvgerages <- dcast(dtMeanStd, subject + activityName ~ measureName, mean, value.var="measureValue")

# Write the tab delimited file
write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")






