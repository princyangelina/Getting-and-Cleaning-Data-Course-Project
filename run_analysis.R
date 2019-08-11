library(dplyr)

#Getting the file
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
Zip_file <- "UCI HAR Dataset.zip"

if (!file.exists(Zip_file)) 
{
  download.file(Url, Zip_file, mode = "wb")
}


# unzip zip file
if (!file.exists("UCI HAR Dataset")) 
{
  unzip(zipFile)
}


########################################################################
#Reading the data files from the unzipped files
########################################################################

#Train data
train_subject <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))
train_values <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
train_activity <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))

#Test data
test_subject <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))
test_values <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
test_activity <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))

#Reading "features.txt"
features <- read.table("features.txt", as.is = TRUE)

#selecting only the features column
features <- features[,2]

#Reading the "activity_labels.txt"
activities <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##############################################################################
#1. Merges the training and the test sets to create one data set.
##############################################################################
train <- cbind(train_subject, train_values, train_activity)
test <- cbind(test_subject, test_values, test_activity)

#Concatenating the train and test data
activity_data <- rbind(train, test)

#Assigning the column names
colnames(activity_data) <- c("subject", features, "activity")


##############################################################################
# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement.
##############################################################################

#Considering the columns needed
columnsToKeep <- grepl("subject|activity|mean|std", colnames(activity_data))
activity_data <- activity_data[, columnsToKeep]


##############################################################################
# 3. Uses descriptive activity names to name the activities in the data set
##############################################################################

#Labeling the activities mentioned in the data
activity_data$activity <- activity_data$activity %>% factor(levels = activities[, 1], 
                                                            labels = activities[, 2])


##############################################################################
# 4. Appropriately labels the data set with descriptive variable names.
##############################################################################

#Considering the column names to modify
columnsToModify <- colnames(activity_data)

#Removing '-' and '()' characters from the column names
columnsToModify <- gsub("[\\(\\)-]", "", columnsToModify)

#Expanding the abbrevations and correcting them if needed
columnsToModify <- gsub("^f", "frequencyDomain", columnsToModify)
columnsToModify <- gsub("^t", "timeDomain", columnsToModify)
columnsToModify <- gsub("Acc", "Accelerometer", columnsToModify)
columnsToModify <- gsub("Gyro", "Gyroscope", columnsToModify)
columnsToModify <- gsub("Mag", "Magnitude", columnsToModify)
columnsToModify <- gsub("Freq", "Frequency", columnsToModify)
columnsToModify <- gsub("mean", "Mean", columnsToModify)
columnsToModify <- gsub("std", "StandardDeviation", columnsToModify)
columnsToModify <- gsub("BodyBody", "Body", columnsToModify)

#Assigning the modified columns to the data
colnames(activity_data) <- columnsToModify


##############################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
##############################################################################

#grouping the data by subject and activity and calculating their mean
activity_mean <- activity_data %>% group_by(subject, activity) %>%
  summarise_each(mean)

#Saving the tidy data in tidy_data.txt file
write.table(activity_mean, "tidy_data.txt", row.names = FALSE, quote = FALSE)
