setwd("~/R_working_directory/GettingAndCleaningData")

## 1. Combine subjects, activities and measurements for train and test set and then merge them into one dataset

##read and combine train data
train_subject <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_labels <- read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
train_set <- read.table("./UCI HAR Dataset/train/x_train.txt", header = FALSE, sep = "")
train_df <- cbind(train_subject, train_labels, train_set)

##read and combine test data
test_subject <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_labels <- read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
test_set <- read.table("./UCI HAR Dataset/test/x_test.txt", header = FALSE, sep = "")
test_df <- cbind(test_subject, test_labels, test_set)

##merge the two sets
df <- rbind(train_df, test_df)


## 4. label the data set with descriptive variable names

colLabels <- c("subject", "activity")
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
colnames(df)[1:2] <- c("subject", "activity")
colnames(df)[-(1:2)] <- features[,2]
colnames(df)[-(1:2)] <- as.vector(features[,2])

## 2. label the measurements and extract only those about mean and standard deviation
install.packages("gdata")
library("gdata")
extractedColumns <- c("subject", "activity", matchcols(df, with=c("mean")), matchcols(df, with=c("std")))
extr_df <- df[,extractedColumns]

## 3. Use descriptive activity names to name the activities in the data set
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")
activities <- as.vector(activities[,2])

for(i in 1:nrow(extr_df)) {
    extr_df[i,"activity"] <- activities[as.numeric(extr_df[i,"activity"])]
}

## 4. remove () from the column names and replace - with _
colnames(extr_df) <- gsub("()", "", colnames(extr_df), fixed = TRUE)
colnames(extr_df) <- gsub("-", "_", colnames(extr_df), fixed = TRUE)

## sort data by subject and activity
extr_df <- extr_df[order(extr_df$subject, extr_df$activity),]

## 5. create a second, independent tidy data set with the average of each variable for each activity and each subject 
## and export it to a file
install.packages("dplyr")
library("dplyr")

means <- extr_df %>% group_by(subject, activity) %>% summarise_each( funs(mean))
write.table(means, "./analysis_results.txt", row.name=FALSE)
