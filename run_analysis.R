## 1. Loading and merging train and test data in R
## set the wd
setwd("/Users/xiaoweizhao/data/UCI_HAR_Dataset/")
## load the train data and merge the data
train_1 <- read.table("./train/X_train.txt")
train_2 <- read.table("./train/y_train.txt")
train_s <- read.table("./train/subject_train.txt")
train <- cbind(train_1, train_2, train_s)
## load the test data and merge the data
test_1 <- read.table("./test/X_test.txt")
test_2 <- read.table("./test/y_test.txt")
test_s <- read.table("./test/subject_test.txt")
test <- cbind(test_1, test_2, test_s)

## 2. Merging train and test data together
data <- rbind(test, train)

## 3. Extracting only the measurements on the mean and standard 
## deviation for each measurement.
library(dplyr)
features <- read.table("./features.txt")[,2]
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), features)
finalData <- data[,c(featureIndex, 562,563)]
colnames(finalData) <- c(as.character(features[featureIndex]), 
                         "activity", "subject")

## 4. Using descriptive activity names to name the activities in the 
## data set
finalData$activity[finalData$activity == 1] <- "WALKING"
finalData$activity[finalData$activity == 2] <- "WALKING_UPSTAIRS"
finalData$activity[finalData$activity == 3] <- "WALKING_DOWNSTAIRS"
finalData$activity[finalData$activity == 4] <- "SITTING"
finalData$activity[finalData$activity == 5] <- "STANDING"
finalData$activity[finalData$activity == 6] <- "LAYING"

## 5. Appropriately labelling the data set with descriptive variable 
## names.
names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequence", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))
names(finalData) <- gsub("Acc", "Accelerometer", names(finalData))
names(finalData)<-gsub("Gyro", "Gyroscope", names(finalData))
names(finalData)<-gsub("BodyBody", "Body", names(finalData))
names(finalData)<-gsub("Mag", "Magnitude", names(finalData))


## 6. From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject.
library(reshape2)
datamelt <- melt(finalData, id = c("subject","activity"), 
                 measure.vars = c(as.character(names(finalData[,1:66]))))
tidyData <- dcast(datamelt, subject+activity ~ variable, mean)
write.table(tidyData, file = "tidyData.txt", row.names = FALSE)