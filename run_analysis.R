#Cleaning project 

#Max Tsukanov
#25.10.2014

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.




#Read training set of data: training set, labels and subjects into one dataset
training = read.csv("cleaningProject/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("cleaningProject/UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("cleaningProject/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
message("Training set proceeded")

#read testing set of data: testing set, labels and subjects into one dataset
testing = read.csv("cleaningProject/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("cleaningProject/UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("cleaningProject/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
message("Testing set proceeded")

#read lables
activityLabels = read.csv("cleaningProject/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)

#read features
features = read.csv("cleaningProject/UCI HAR Dataset/features.txt", sep="", header=FALSE)

message("Labels and features read")

#change names a little 

#mean into Mean
features[,2] = gsub('-mean', 'Mean', features[,2])
#std into Std
features[,2] = gsub('-std', 'Std', features[,2])
#no record into blank
features[,2] = gsub('[-()]', '', features[,2])

# Merge training and test sets together
MergedData = rbind(training, testing)
message("Sets merged")

# Get only the data on mean and std. dev.
cols <- grep(".*Mean.*|.*Std.*", features[,2])
# First reduce the features table to what we want
features <- features[cols,]
# Now add the last two columns (subject and activity)
cols <- c(cols, 562, 563)
# And remove the unwanted columns from allData
MergedData <- MergedData[,cols]
# Add the column names (features) to allData
colnames(MergedData) <- c(features$V2, "Activity", "Subject")
colnames(MergedData) <- tolower(colnames(MergedData))


#Name activites with meaningful words instead of numbers
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  MergedData$activity <- gsub(currentActivity, currentActivityLabel, MergedData$activity)
  currentActivity <- currentActivity + 1
}
message("Set names fixed")

#Make activity and subject factors
MergedData$activity <- as.factor(MergedData$activity)
MergedData$subject <- as.factor(MergedData$subject)

#Prepare data for output. 
#Perform an agregation by activity and subject for mean calculation
tidyData = aggregate(MergedData, by=list(activity = MergedData$activity, subject=MergedData$subject), mean)
# Remove the subject and activity column
tidyData[,90] = NULL
tidyData[,89] = NULL
message("Data is ready for export")
#Export data into file
write.table(tidyData, "cleaningProject/tidyData.txt", row.name=FALSE, sep="\t")
message("Export successful")
message("Please see file 'your_working_directory/cleaningProject/tidyData.txt'")