#Getting data--------

path <- "C:/Rprojects/Week4_project"
setwd(path)
if(!file.exists("./data")){dir.create("./data")}
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url,destfile="./data/Dataset.zip",method="wininet")

#Unzipping files------

unzip(zipfile="./data/Dataset.zip",exdir="./data")

#R packages-----------

library(dplyr)
library(data.table)
library(tidyr)

#Creating data tables

path <- "C:/Rprojects/Week4_project/data/UCI HAR Dataset"

#DataSubjects Train/Test reading & tables creation

dataSTrain <- tbl_df(read.table(file.path(path, "train", "subject_train.txt")))
dataSTest  <- tbl_df(read.table(file.path(path, "test" , "subject_test.txt" )))

#Actvityfiles  reading & tables creation

dataATrain <- tbl_df(read.table(file.path(path, "train", "Y_train.txt")))
dataATest  <- tbl_df(read.table(file.path(path, "test" , "Y_test.txt" )))

#Datafiles  reading & tables creation

dataTrain <- tbl_df(read.table(file.path(path, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(path, "test" , "X_test.txt" )))

#Merging Actitivity and Subjects files for Training & test sets and renaming
#variables Subject and ActivityNum

alldataSubject <- rbind(dataSTrain, dataSTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataATrain, dataATest)
setnames(alldataActivity, "V1", "activityNum")

#combining data training & test files
TableData <- rbind(dataTrain, dataTest)

#Variable number by feature

dataFeatures <- tbl_df(read.table(file.path(path, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(TableData) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(path, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merging columns
alldataSubj <- cbind(alldataSubject, alldataActivity)
TableData <- cbind(alldataSubj, dataTable)

# Reading "features.txt" and extracting only the mean and standard deviation

dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
TableData<- subset(TableData,select=dataFeaturesMeanStd) 

# entering activity name dataTable
TableData <- merge(activityLabels, TableData , by="activityNum", all.x=TRUE)
TableData$activityName <- as.character(TableData$activityName)

# dataTable with variable means sorted by subject and Activity
TableData$activityName <- as.character(TableData$activityName)
dataAggregate<- aggregate(. ~ subject - activityName, data = TableData, mean) 
TableData <- tbl_df(arrange(dataAggregate,subject,activityName))

# Appropriately labels the data set with descriptive variable names

#leading t or f is based on time or frequency measurements.
#Body = related to body movement.
#Gravity = acceleration of gravity
#Acc = accelerometer measurement
#Gyro = gyroscopic measurements
#Jerk = sudden movement acceleration
#Mag = magnitude of movement
#mean and SD are calculated for each subject for each activity for each mean and SD measurements.
#The units given are g's for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec 
#for the corresponding jerks.

names(TableData)<-gsub("std()", "SD", names(TableData))
names(TableData)<-gsub("mean()", "MEAN", names(TableData))
names(TableData)<-gsub("^t", "time", names(TableData))
names(TableData)<-gsub("^f", "frequency", names(TableData))
names(TableData)<-gsub("Acc", "Accelerometer", names(TableData))
names(TableData)<-gsub("Gyro", "Gyroscope", names(TableData))
names(TableData)<-gsub("Mag", "Magnitude", names(TableData))
names(TableData)<-gsub("BodyBody", "Body", names(TableData))

head(str(TableData),6)
write.table(TableData, "TidyData.txt", row.name=FALSE)






