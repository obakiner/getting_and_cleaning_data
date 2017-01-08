##Uploading relevant packages##

library(dplyr)
library(data.table)
library(tidyr)

##Setting the working directory, downloading the database, and creating readable data frames for each .txt file##

setwd('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/')
download.file('https://d396qusza40orc.cloudfront.net/getdata/projectfiles%2FUCI%20HAR%20Dataset.zip', destfile = 'dataset.zip', method='curl')
unzip(zipfile='dataset.zip', exdir = './data')
subject_train <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/', "train", "subject_train.txt"), header=FALSE)

subject_test <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/', "test", "subject_test.txt"), header=FALSE)

Y_train <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/', "train", "Y_train.txt"), header=FALSE)

Y_test <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/', "test", "Y_test.txt"), header=FALSE)

X_train <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/', "train", "X_train.txt"), header=FALSE)

X_test <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/', "test", "X_test.txt"), header=FALSE)

features <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/features.txt'))

activity_labels <- read.table(file.path('/Users/onRu/Documents/Methodology/Data Science courses/Getting and Cleaning Data/data/UCI HAR dataset/activity_labels.txt'), header=FALSE)

## 1. Merges the training and the test sets to create one data set##

##Renaming the variables in each individual dataset in order to make merging and tidying easy later##

colnames(subject_train) <- 'subjectID'
colnames(subject_test) <- 'subjectID'
colnames(activity_labels) <- c('activityID','activityType')
colnames(X_train) <- features[, 2]
colnames(X_test) <- features[, 2]
colnames(Y_train) <- 'activityID'
colnames(Y_test) <- 'activityID'

##Combining the datasets as train and test groups##
data_train <- cbind(Y_train, subject_train, X_train)
data_test <- cbind(Y_test, subject_test, X_test)

##Combining the train and test datasets to create the final dataset##
dataset_final <- rbind(data_train, data_test)

##2. Extracts only the measurements on the mean and standard deviation for each measurement.##

##using the 'grepl' function to get TRUE statements for column names that include the words 'mean' and 'std'## 
extracted_mean_std <- grep("(mean|std)\\(\\)", names(dataset_final))
 
##creating a new dataset in which the 'subjectID', 'activityID', and extracted mean and st columns remain##
dataset_final2 <- dataset_final[,c(1,2, extracted_mean_std)]


##3. Uses descriptive activity names to name the activities in the data set##

##merging two datasets with shared columns into one by the activityID column##
dataset_final3 <- merge(x=dataset_final2, y=activity_labels, by="activityID")


##4. Appropriately labels the data set with descriptive variable names.##
##checking the variable names to see what can be change##
names(dataset_final3)

##using the gsub function to turn non-intuitive titles into descriptive ones##

names(dataset_final3) <- gsub('^t', 'Time', names(dataset_final3))
names(dataset_final3) <- gsub('^f', 'Frequency', names(dataset_final3))
names(dataset_final3) <- gsub('-mean\\(\\)', 'Mean', names(dataset_final3))
names(dataset_final3) <- gsub('-std\\(\\)', 'Standard Deviation', names(dataset_final3))
names(dataset_final3) <- gsub('-', ' ', names(dataset_final3))
names(dataset_final3) <- gsub('BodyBody', 'Body', names(dataset_final3))

##checking the variable names again to see if the changes have taken place##
names(dataset_final3)


##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.##

##Turning the integer variable subjectID into a factor variable and making a data table from the dataset to be able to use it for tidyr##

dataset_final3$subjectID <- as.factor(dataset_final3$subjectID)
dataset_final3 <- data.table(dataset_final3)

##using the aggregate function for grouping the dataset along subjectID and activityID and taking each segment's mean##
dataset_tidy <- aggregate(.~subjectID + activityType, dataset_final3, mean)

##sorting rows along the lines of subjectID and activityID in ascending order, respectively, and turning the dataset into a .text file##
dataset_tidy <- dataset_tidy[order(dataset_tidy$subjectID, dataset_tidy$activityType),]
write.table(dataset_tidy, file = 'tidy.txt', row.names = FALSE)