##This is for making objects that are for reading tables in UCI HAR Dataset
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

## (1) I merged test and train with the variable 'test_train'
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject1 <- rbind(subject_train, subject_test)
test_train <- cbind(subject1, Y, X)

## (2)  Extracts the mean and standard deviation for each measurement.
  tidyset <- select(test_train, subject, code,contains("mean"),contains("std"))
  
## (3) Uses descriptive activity names to name the activities in the data set.
  
  tidyset$code <- activities[tidyset$code, 2]
  
  
  
  
  
  
## (4) Appropriately labelling the data set with descriptive variable names.  
  names(tidyset)[2] = "activity"
  names(tidyset)<-gsub("Acc", "Accelerometer", names(tidyset))
  names(tidyset)<-gsub("Gyro", "Gyroscope", names(tidyset))
  names(tidyset)<-gsub("BodyBody", "Body", names(tidyset))
  names(tidyset)<-gsub("Mag", "Magnitude", names(tidyset))
  names(tidyset)<-gsub("^t", "Time", names(tidyset))
  names(tidyset)<-gsub("^f", "Frequency", names(tidyset))
  names(tidyset)<-gsub("tBody", "TimeBody", names(tidyset))
  names(tidyset)<-gsub("-mean()", "Mean", names(tidyset), ignore.case = TRUE)
  names(tidyset)<-gsub("-std()", "STD", names(tidyset), ignore.case = TRUE)
  names(tidyset)<-gsub("-freq()", "Frequency", names(tidyset), ignore.case = TRUE)
  names(tidyset)<-gsub("angle", "Angle", names(tidyset))
  names(tidyset)<-gsub("gravity", "Gravity", names(tidyset))
  
  
  ## (5) Independent tidy data set with the average of each variable for each activity and each subject.
  final_dataset<- tidyset %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
  write.table(final_dataset, "final_dataset.txt", row.name=FALSE)
  
  