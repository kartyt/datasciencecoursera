library(dplyr)

# Loads data
path <- getwd()
url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path,"dataset.zip"))
unzip(zipfile = "dataset.zip")

trainX <- read.table(file.path(path, "UCI HAR Dataset/train/X_train.txt"))
trainy <- read.table(file.path(path, "UCI HAR Dataset/train/y_train.txt"))
trainsub <- read.table(file.path(path, "UCI HAR Dataset/train/subject_train.txt"))
testX <- read.table(file.path(path, "UCI HAR Dataset/test/X_test.txt"))
testy <- read.table(file.path(path, "UCI HAR Dataset/test/y_test.txt"))
testsub <- read.table(file.path(path, "UCI HAR Dataset/test/subject_test.txt"))
features<- read.table(file.path(path, "UCI HAR Dataset/features.txt"))
activities<- read.table(file.path(path, "UCI HAR Dataset/activity_labels.txt"))

## Changes column names for measurements
features <- unlist(list(features[,2]))
colnames(testX) <- features
colnames(trainX) <- features

## Uses descriptive activity names to name the activities in the data set
activities[,2]<-tolower(gsub("_", " ", activities[,2])) # formats values to lower cases and removes underscores
trainy<-trainy[] %>% mutate_all(funs(setNames(activities$V2, activities$V1)[.]))
testy<-testy[] %>% mutate_all(funs(setNames(activities$V2, activities$V1)[.]))

## Merges the training and the test sets to create one data set.
test<-cbind(testsub, testy, testX)
train<-cbind(trainsub, trainy, trainX)
data<-rbind(train, test)

# Removes temporary variables
rm(test,testsub,testX,testy,train,trainsub,trainX,trainy,activities,features)

## Extracts only the measurements on the mean and standard deviation for each measurement. 
col_indexes<-sort(c(grep(colnames(data), pattern="mean|std"),1,2)) #find indexes of the columns containing mean and standard deviation, adds to the vector 1st and 2nd column (our subjects and labels), then sorts the vector
data<-data[,col_indexes] #subtract data frame

## Appropriately labels the data set with descriptive variable names. 
colnames(data)<-gsub("^t","Time", colnames(data))
colnames(data)<-gsub("^f","Freq", colnames(data))
colnames(data)<-gsub("\\(\\)","", colnames(data))
colnames(data)<-gsub("mean","Mean", colnames(data))
colnames(data)<-gsub("std","Std", colnames(data))
colnames(data)<-gsub("-","", colnames(data))
colnames(data)[1]<- "Subject"
colnames(data)[2] <- "Activity"

# From the data, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
group_cols <- c("Subject", "Activity")
tidy_data <- data %>% group_by(across(all_of(group_cols)))%>%summarize(across(1:79,mean))
rm(group_cols,col_indexes)

# Saves tidy_data to the file
write.csv(tidy_data, "tidy_data.csv", row.names=FALSE)