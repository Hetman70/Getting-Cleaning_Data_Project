##############################################################################
#
# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#

library(dplyr)

##############################################################################
# STEP 0A - Get data
##############################################################################

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

##############################################################################
# STEP 0B - Read data
##############################################################################

# read training data
trainSub <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainVal <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainAct <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSub <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testVal <- read.table(file.path(dataPath, "test", "X_test.txt"))
testAct <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
feat <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
## note: feature names (in features[, 2]) are not unique
##       e.g. fBodyAcc-bandsEnergy()-17,34

# read activity labels
activ <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activ) <- c("activityId", "activityLabel")


##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

# concatenate individual data tables to make single data table
humAct <- rbind(
  cbind(trainSub, trainVal, trainAct),
  cbind(testSub, testVal, testAct)
)

# remove individual data tables to save memory
rm(trainSub, trainVal, trainAct, 
   testSub, testVal, testAct)

# assign column names
colnames(humAct) <- c("subject", feat[, 2], "activity")

##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

# determine columns of data set to keep based on column name...
colToKeep <- grepl("subject|activity|mean|std", colnames(humAct))

# ... and keep data in these columns only
humAct <- humAct[, colToKeep]

##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################

# replace activity values with named factor levels
humAct$activity <- factor(humAct$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

# get column names
humActCols <- colnames(humAct)

# remove special characters
humActCols <- gsub("[\\(\\)-]", "", humActCols)

# expand abbreviations and clean up names
humActCols <- gsub("^f", "frequencyDomain", humActCols)
humActCols <- gsub("^t", "timeDomain", humActCols)
humActCols <- gsub("Acc", "Accelerometer", humActCols)
humActCols <- gsub("Gyro", "Gyroscope", humActCols)
humActCols <- gsub("Mag", "Magnitude", humActCols)
humActCols <- gsub("Freq", "Frequency", humActCols)
humActCols <- gsub("mean", "Mean", humActCols)
humActCols <- gsub("std", "StandardDeviation", humActCols)

# correct typo
humActCols <- gsub("BodyBody", "Body", humActCols)

# use new labels as column names
colnames(humAct) <- humActCols


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean
humActMeans <- humAct %>% 
  group_by(subject, activity) %>%
  summarise_at(2:80,funs(mean))

# output to file "tidy_data.txt"
write.table(humActMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

