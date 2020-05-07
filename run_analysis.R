# Getting and Cleansing Activity Dataset

# Import project data

library(readxl)

subject_test_file <- "UCI HAR Dataset/test/subject_test.txt"
test_file_x <- "UCI HAR Dataset/test/X_test.txt"
test_file_y <- "UCI HAR Dataset/test/Y_test.txt"

subject_train_file <- "UCI HAR Dataset/train/subject_train.txt"
train_file_x <- "UCI HAR Dataset/train/X_train.txt"
train_file_y <- "UCI HAR Dataset/train/Y_train.txt"

features_file <-  "UCI HAR Dataset/features.txt"

activity_file <- "UCI HAR Dataset/activity_labels.txt"

# Listing for measure descriptions. Export file from GitHub and save in working directory
feature_labels <- read_excel("feature_labels_for_project.xlsx", sheet = "Sheet1")


library(dplyr)

# Test

subject_test <- read.table(subject_test_file, sep="", quote="\"", comment.char="")
x_test <- read.table(test_file_x, sep="",  quote="\"", comment.char="")
y_test <- read.table(test_file_y, sep="", quote="\"", comment.char="")

# Training

subject_train <- read.table(subject_train_file, sep="", quote="\"", comment.char="")
x_train <- read.table(train_file_x, sep="", quote="\"", comment.char="")
y_train <- read.table(train_file_y, sep="", quote="\"", comment.char="")

# Headers

features <- read.table(features_file, sep="", quote="\"", comment.char="")

# Activities

activities <- read.table(activity_file, sep="", quote="\"", comment.char="")

y_test_a <- left_join(y_test, activities, by = c("V1", "V1"))
y_train_a <- left_join(y_train, activities, by = c("V1", "V1"))

# set column names on data

colnames(subject_test) <- "Subject"
colnames(subject_train) <- "Subject"

colnames(x_test) <- features$V2
colnames(x_train) <- features$V2

colnames(y_test_a) <- c("A1", "Activity")
colnames(y_train_a) <- c("A1", "Activity")

#Combine x and y tables

test <- cbind(subject_test, y_test_a, x_test)
training <- cbind(subject_train, y_train_a, x_train)

# Mark source for reference

test$Source <- "test"
training$Source <- "training"

# Combine test and training data sets

combine_data <- rbind(test, training)

combine_data_m_sd <- combine_data[, grepl("Subject", colnames(combine_data)) | 
                                    grepl("Activity", colnames(combine_data)) | 
                                    grepl("mean()", colnames(combine_data)) | 
                                    grepl("std()", colnames(combine_data)) | 
                                    grepl("Source", colnames(combine_data))]

combine_names <- data.frame(colnames(combine_data_m_sd))
colnames(combine_names) <- "V2"
combine_names$V2 <- as.character(combine_names$V2)
feature_labels$V2 <- as.character(feature_labels$V2)
combine_names <- left_join(combine_names, feature_labels, by = c("V2", "V2"))

combine_act_sub <- combine_data_m_sd
colnames(combine_act_sub) <- combine_names$Activity_Tracking_Measures

combine_data_m_sd$Subject <- as.character(combine_data_m_sd$Subject)

#Create tidy data summary by activity and subject

activity_summary <- combine_data_m_sd %>%
  group_by(Activity, Subject) %>%
  summarize(
            "Mean. Body Acc (sec) - X" = mean(`tBodyAcc-mean()-X`),
            "Mean. Body Acc (sec) - Y" = mean(`tBodyAcc-mean()-Y`),
            "Mean. Body Acc (sec) - Z" = mean(`tBodyAcc-mean()-Z`),
            "MeanStd. Body Acc (sec) - X" = mean(`tBodyAcc-std()-X`),
            "MeanStd. Body Acc (sec) - Y" = mean(`tBodyAcc-std()-Y`),
            "MeanStd. Body Acc (sec) - Z" = mean(`tBodyAcc-std()-Z`),
            "Mean. Gravity Acc (sec) - X" = mean(`tGravityAcc-mean()-X`),
            "Mean. Gravity Acc (sec) - Y" = mean(`tGravityAcc-mean()-Y`),
            "Mean. Gravity Acc (sec) - Z" = mean(`tGravityAcc-mean()-Z`),
            "MeanStd. Gravity Acc (sec) - X" = mean(`tGravityAcc-std()-X`),
            "MeanStd. Gravity Acc (sec) - Y" = mean(`tGravityAcc-std()-Y`),
            "MeanStd. Gravity Acc (sec) - Z" = mean(`tGravityAcc-std()-Z`),
            "Mean. Body Jerk Acc (sec) - X" = mean(`tBodyAccJerk-mean()-X`),
            "Mean. Body Jerk Acc (sec) - Y" = mean(`tBodyAccJerk-mean()-Y`),
            "Mean. Body Jerk Acc (sec) - Z" = mean(`tBodyAccJerk-mean()-Z`),
            "MeanStd. Body Jerk Acc (sec) - X" = mean(`tBodyAccJerk-std()-X`),
            "MeanStd. Body Jerk Acc (sec) - Y" = mean(`tBodyAccJerk-std()-Y`),
            "MeanStd. Body Jerk Acc (sec) - Z" = mean(`tBodyAccJerk-std()-Z`),
            "Mean. Body Gyro (sec) - X" = mean(`tBodyGyro-mean()-X`),
            "Mean. Body Gyro (sec) - Y" = mean(`tBodyGyro-mean()-Y`),
            "Mean. Body Gyro (sec) - Z" = mean(`tBodyGyro-mean()-Z`),
            "MeanStd. Body Gyro (sec) - X" = mean(`tBodyGyro-std()-X`),
            "MeanStd. Body Gyro (sec) - Y" = mean(`tBodyGyro-std()-Y`),
            "MeanStd. Body Gyro (sec) - Z" = mean(`tBodyGyro-std()-Z`),
            "Mean. Body Jerk Gyro (sec) - X" = mean(`tBodyGyroJerk-mean()-X`),
            "Mean. Body Jerk Gyro (sec) - Y" = mean(`tBodyGyroJerk-mean()-Y`),
            "Mean. Body Jerk Gyro (sec) - Z" = mean(`tBodyGyroJerk-mean()-Z`),
            "MeanStd. Body Jerk Gyro (sec) - X" = mean(`tBodyGyroJerk-std()-X`),
            "MeanStd. Body Jerk Gyro (sec) - Y" = mean(`tBodyGyroJerk-std()-Y`),
            "MeanStd. Body Jerk Gyro (sec) - Z" = mean(`tBodyGyroJerk-std()-Z`),
            "Mean. Body Acc. (sec) Magnitude" = mean(`tBodyAccMag-mean()`),
            "MeanStd. Body Acc. (sec) Magnitude" = mean(`tBodyAccMag-std()`),
            "Mean. Gravity Acc. (sec) Magnitude" = mean(`tGravityAccMag-mean()`),
            "MeanStd. Gravity Acc. (sec) Magnitude" = mean(`tGravityAccMag-std()`),
            "Mean. Body Jerk Acc. (sec) Magnitude" = mean(`tBodyAccJerkMag-mean()`),
            "MeanStd. Body Jerk Acc. (sec) Magnitude" = mean(`tBodyAccJerkMag-std()`),
            "Mean. Body Gyro Magnitude" = mean(`tBodyGyroMag-mean()`),
            "MeanStd. Body Gyro (sec) Magnitude" = mean(`tBodyGyroMag-std()`),
            "Mean. Body Gyro Jerk Magnitude" = mean(`tBodyGyroJerkMag-mean()`),
            "MeanStd. Body Gyro Jerk Magnitude" = mean(`tBodyGyroJerkMag-std()`),
            "Mean. Body Acc (Hz) - X" = mean(`fBodyAcc-mean()-X`),
            "Mean. Body Acc (Hz) - Y" = mean(`fBodyAcc-mean()-Y`),
            "Mean. Body Acc (Hz) - Z" = mean(`fBodyAcc-mean()-Z`),
            "MeanStd. Body Acc (Hz) - X" = mean(`fBodyAcc-std()-X`),
            "MeanStd. Body Acc (Hz) - Y" = mean(`fBodyAcc-std()-Y`),
            "MeanStd. Body Acc (Hz) - Z" = mean(`fBodyAcc-std()-Z`),
            "MeanFreq. Body Acc (Hz) - X" = mean(`fBodyAcc-meanFreq()-X`),
            "MeanFreq. Body Acc (Hz) - Y" = mean(`fBodyAcc-meanFreq()-Y`),
            "MeanFreq. Body Acc (Hz) - Z" = mean(`fBodyAcc-meanFreq()-Z`),
            "Mean. Body Jerk Acc (Hz) - X" = mean(`fBodyAccJerk-mean()-X`),
            "Mean. Body Jerk Acc (Hz) - Y" = mean(`fBodyAccJerk-mean()-Y`),
            "Mean. Body Jerk Acc (Hz) - Z" = mean(`fBodyAccJerk-mean()-Z`),
            "MeanStd. Body Jerk Acc (Hz) - X" = mean(`fBodyAccJerk-std()-X`),
            "MeanStd. Body Jerk Acc (Hz) - Y" = mean(`fBodyAccJerk-std()-Y`),
            "MeanStd. Body Jerk Acc (Hz) - Z" = mean(`fBodyAccJerk-std()-Z`),
            "MeanFreq. Body Jerk Acc (Hz) - X" = mean(`fBodyAccJerk-meanFreq()-X`),
            "MeanFreq. Body Jerk Acc (Hz) - Y" = mean(`fBodyAccJerk-meanFreq()-Y`),
            "MeanFreq. Body Jerk Acc (Hz) - Z" = mean(`fBodyAccJerk-meanFreq()-Z`),
            "Mean. Body Gyro (Hz) - X" = mean(`fBodyGyro-mean()-X`),
            "Mean. Body Gyro (Hz) - Y" = mean(`fBodyGyro-mean()-Y`),
            "Mean. Body Gyro (Hz) - Z" = mean(`fBodyGyro-mean()-Z`),
            "MeanStd. Body Gyro (Hz) - X" = mean(`fBodyGyro-std()-X`),
            "MeanStd. Body Gyro (Hz) - Y" = mean(`fBodyGyro-std()-Y`),
            "MeanStd. Body Gyro (Hz) - Z" = mean(`fBodyGyro-std()-Z`),
            "MeanFreq. Body Gyro (Hz) - X" = mean(`fBodyGyro-meanFreq()-X`),
            "MeanFreq. Body Gyro (Hz) - Y" = mean(`fBodyGyro-meanFreq()-Y`),
            "MeanFreq. Body Gyro (Hz) - Z" = mean(`fBodyGyro-meanFreq()-Z`),
            "Mean. Body Acc Magnitude (Hz)" = mean(`fBodyAccMag-mean()`),
            "MeanStd. Body Acc Magnitude (Hz)" = mean(`fBodyAccMag-std()`),
            "MeanFreq. Body Acc Magnitude (Hz)" = mean(`fBodyAccMag-meanFreq()`),
            "Mean. Body Body Acc Jerk Magnitude (Hz)" = mean(`fBodyBodyAccJerkMag-mean()`),
            "MeanStd. Body Body Acc Jerk Magnitude (Hz)" = mean(`fBodyBodyAccJerkMag-std()`),
            "MeanFreq. Body Body Acc Jerk Magnitude (Hz)" = mean(`fBodyBodyAccJerkMag-meanFreq()`),
            "Mean. Body Body Gyro Magnitude (Hz)" = mean(`fBodyBodyGyroMag-mean()`),
            "MeanStd. Body Body Gyro Magnitude (Hz)" = mean(`fBodyBodyGyroMag-std()`),
            "MeanFreq. Body Body Gyro Magnitude (Hz)" = mean(`fBodyBodyGyroMag-meanFreq()`),
            "Mean. Body Body Gyro Jerk Magnitude (Hz)" = mean(`fBodyBodyGyroJerkMag-mean()`),
            "MeanStd. Body Body Gyro Jerk Magnitude (Hz)" = mean(`fBodyBodyGyroJerkMag-std()`),
            "MeanFreq. Body Body Gyro Jerk Magnitude (Hz)" = mean(`fBodyBodyGyroJerkMag-meanFreq()`)
            
  ) %>%
  arrange(Activity, Subject)

# Export files to working directory
summary_export <- "activities_mean_std.txt"
tidy_export <- "tidy_activity_means.txt"

write.table(combine_act_sub, summary_export, append = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)
write.table(activity_summary, tidy_export, append = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)
  