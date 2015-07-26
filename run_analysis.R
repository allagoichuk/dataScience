run_analysis <- function() {
    
    #read training and test data
    trainingData <- read.table("UCI HAR Dataset/train/x_train.txt")
    testData <- read.table("UCI HAR Dataset/test/x_test.txt")
    
    #step 1. Merge training and test data
    allData <- rbind(trainingData, testData)

    colNames <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[[2]]
    filteredColumns <- colNames[grep("mean\\(\\)|std\\(\\)", colNames)]
    #step 4. Appropriately labels the data set with descriptive variable names.
    colnames(allData) <- colNames
    #step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    allData <- allData[, filteredColumns]

    #step 3. Uses descriptive activity names to name the activities in the data set
    #read activities data and add it to the allData
    trainingActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
    testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")
    activity <- append(trainingActivities[[1]], testActivities[[1]])
    allData <- cbind(activity, allData)
    #replace activity number with activity name
    activityNames <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)[[2]]
    allData[, "activity"] <- activityNames[allData[["activity"]]]

    #read subjects data and add it to the allData
    trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
    testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
    subject <- append(trainingSubjects[[1]], testSubjects[[1]])
    allData <- cbind(subject, allData)
}

#step 5. From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
getSummary <- function(dataFrame) {
    
    #split dataframe by subject and activity
    ncol <- ncol(dataFrame)
    
    summary <- aggregate(dataFrame[, 3:ncol], list("subject" = data$subject, "activity" = data$activity), mean)
    write.table(summary, file = "tidydata.txt", row.names = FALSE)
    
    summary
}
