# 1. Merges the training and the test sets to create one data set
# setwd("c:\\users\\zhu\\documents\\coursera\\getdata\\project")
trainData <- read.table('./UCI HAR Dataset/train/X_train.txt')
dim(trainData)
trainLabel <- read.table('./UCI HAR Dataset/train/y_train.txt')
trainSubject <- read.table('./UCI HAR Dataset/train/subject_train.txt')
testData <- read.table('./UCI HAR Dataset/test/X_test.txt')
dim(testData)
testLabel <- read.table('./UCI HAR Dataset/test/y_test.txt')
testSubject <- read.table('./UCI HAR Dataset/test/subject_test.txt')
combineData <- rbind(trainData, testData)
dim(combineData)
combineLabel <- rbind(trainLabel, testLabel)
dim(combineLabel)
combineSubject <- rbind(trainSubject, testSubject)
dim(combineSubject)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table('./UCI HAR Dataset/features.txt')
dim(features) 
# search for all feature with mean() or std() in the name, return the indices
indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
mean_std_data <- combineData[, indices]

# 3. Use descriptive activity names to name the activities in the data set
activity <- read.table('./UCI HAR Dataset/activity_labels.txt')
activityLabel <- activity[combineLabel[, 1], 2]
combineLabel[, 1] <- activityLabel
names(combineLabel) <- 'activity'

# 4. Appropriately labels the data set with descriptive activity
names(combineSubject) <- 'subject'
cleanedData <- cbind(combineSubject, combineLabel, combineData)
dim(cleanedData)
write.table(cleanedData, 'merged_data.txt')

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(combineSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(combineSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "subject_mean_data.txt")
