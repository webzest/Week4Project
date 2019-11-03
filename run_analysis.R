#########################################################################################
# Getting and Cleaning Data Course Project
# Johnny Sandaire
# 30 Oct 2019

# Description of runAnalysis.r:

# This script will perform the following steps on the UCI HAR Dataset downloaded from
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# Download data and unzip it into a directory of your choice.
# Then, run the code to perform the following steps to prepare for a final tidy data set
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the
#    average of each variable for each activity and each subject.

########################################################################################

#set working directory to the location where the UCI HAR Dataset was unzipped
workDir <- getwd()
setwd(workDir);
library(dplyr)
library(readr)
# 1. Merges the training and the test sets to create one data set.

# Import features and activity data from files
features     = read.table('./features.txt',header=F, col.names = c("N","function"));
activityLabel = read.table('./activity_labels.txt',header=F, col.names = c("activityId","activityType"));

# Import Training data from files and assign new headers
subjectTrain = read.table('./train/subject_train.txt',header=F, col.names = "subjectId");
xTrain       = read.table('./train/x_train.txt',header=F, col.names = features[,2]);
yTrain       = read.table('./train/y_train.txt',header=F, col.names = "activityId");


# Import test data  and assign new headers
subjectTest = read.table('./test/subject_test.txt',header=F, col.names = "subjectId");
xTest       = read.table('./test/x_test.txt',header=F, col.names = features[,2]);
yTest       = read.table('./test/y_test.txt',header=F, col.names = "activityId");

# Combine training and test data to create a final data set
finalData = rbind(cbind(subjectTrain,xTrain,yTrain),cbind(subjectTest,xTest,yTest));
# colnames <- colnames(finalData);colnames

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
finalData <- finalData %>% select(subjectId, activityId, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set
finalData = merge(finalData,activityType,by='activityId',all.x=T);
finalData$activityId <- activityLabel[finalData$activityType, 2]
finalData <- finalData %>% select(subjectId, activityType, contains("mean"), contains("std"))


# 4. Appropriately labels the data set with descriptive variable names.
# Utilizing gsub to scrub or replace criptic with descriptive names

 colnames(finalData) <- gsub("\\()","", colnames(finalData))
 colnames(finalData) <- gsub("\\()","",colnames(finalData))
 colnames(finalData) <- gsub("std","StdDeviation",colnames(finalData))
 colnames(finalData) <- gsub("mean","Mean",colnames(finalData))
 colnames(finalData) <- gsub("[Ff]req","Frequency",colnames(finalData))
 colnames(finalData) <- gsub("([Gg]ravity)","Gravity",colnames(finalData))
 colnames(finalData) <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames(finalData))
 colnames(finalData) <- gsub("[Gg]yro","Gyro",colnames(finalData))
 colnames(finalData) <- gsub("AccMag","AccMagnitude",colnames(finalData))
 colnames(finalData) <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames(finalData))
 colnames(finalData) <- gsub("JerkMag","JerkMagnitude",colnames(finalData))
 colnames(finalData) <- gsub("GyroMag","GyroMagnitude",colnames(finalData))
 colnames(finalData) <- gsub("^(f)","Frequency",colnames(finalData))
 colnames(finalData) <- gsub("^(t)","Time",colnames(finalData))
#colnames <- colnames(finalData);colnames


# 5. From the data set in step 4, creates a second, independent tidy data set with the
#    average of each variable for each activity and each subject.

tidyData <- finalData %>% group_by(subjectId, activityType) %>% summarise_all(funs(mean))
write.table(tidyData, "tidyDataSet.txt", row.name=FALSE)


