#########################################################################################
# Getting and Cleaning Data Course Project
# Johnny Sandaire
# 30 Oct 2019

# Description of runAnalysis.r:

# This script will perform the following steps on the UCI HAR Dataset downloaded from
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge training and test sets.
# 2. Extract measurements on mean and standard deviation on each observation
# 3. Use descriptive names on data set activities
# 4. Label data sets with descriptive activity names.
# 5. Create a second, independent tidy data set with averages, activity and subject.

#########################################################################################

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("X:/DataScience/JohnHopkinsUniversity/Week4Project10312019-master/Week4Project");


# 1. Merge training and test sets to create one data set.

# Import features and activity data from files
features     = read.table('./features.txt',header=F);
activityLabel = read.table('./activity_labels.txt',header=F);

# Import Training data from files
subjectTrain = read.table('./train/subject_train.txt',header=F);
xTrain       = read.table('./train/x_train.txt',header=F);
yTrain       = read.table('./train/y_train.txt',header=F);

# Assign Headers to Training and features/activity data
colnames(activityLabel)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2];
colnames(yTrain)        = "activityId";

# Merging yTrain, subjectTrain, and xTrain dtat tables into a single trainData table
trainData = cbind(yTrain,subjectTrain,xTrain);

# Import test data
subjectTest = read.table('./test/subject_test.txt',header=F);
xTest       = read.table('./test/x_test.txt',header=F);
yTest       = read.table('./test/y_test.txt',header=F);

# Assign Headers to test data
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2];
colnames(yTest)       = "activityId";


# Merging xTest, yTest and subjectTest data into a single testData table
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
finalData = rbind(trainData,testData);

# Create a vector of column names from finalData for getting mean() & stddev()
colNames  = colnames(finalData);

# 2. Extract measurements on mean and standard deviation for each observation

# Built TFVector with T & F values for the ID, mean() & stddev() columns
# Use grepl() to generate a vector of logicals
TFVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the TFVector to keep only desired columns
finalData = finalData[TFVector==T];

# 3. Use descriptive names on data set activities, merging finalData with
# activityType by activityId

finalData = merge(finalData,activityType,by='activityId',all.x=T);

# Update colNames vector with finalData
colNames  = colnames(finalData);

# 4. Label data sets with descriptive activity names.

for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with averages, activity and subject.

# Create a new finalDataNoActivityType table, excluding the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include only the variables' mean of each activity and participant
tidyData    = aggregate(
  finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],
                        by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),
  mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=T);

# Export the tidyData set
write.table(tidyData, './tidyDataSet.txt', row.name=FALSE)
