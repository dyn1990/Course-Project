library(dplyr)
library(tidyr)
library(plyr)

filename <- "getdata_dataset.zip"

# download and unzip the dataset:
if (!file.exists(filename)){
        fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, filename, mode = "wb")
        ## I have to add mode = "wb" to be able to unpack the zip file
}
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}

# loading features, separate the column by the
## number label and the corresponding features
features <- read.table("features.txt", sep = "\n", stringsAsFactors = FALSE)
features <- features %>%  separate(V1, c("featurelabel", "features"), sep = " ")
features$features <- gsub("\\()", "", features$features)
act_labels <- read.table("activity_labels.txt", sep = "\n", stringsAsFactors = FALSE)
act_labels <- act_labels %>%  separate(V1, c("actlabel", "activity"), sep = " ")

## loading train and test sets
subject_train <- read.table("./train/subject_train.txt", sep = "\n", stringsAsFactors = FALSE)
y_train <- read.table("./train/y_train.txt", sep = "\n", stringsAsFactors = FALSE)
X_train <- read.table("./train/X_train.txt", sep = "", stringsAsFactors = FALSE)
subject_test <- read.table("./test/subject_test.txt", sep = "\n", stringsAsFactors = FALSE)
y_test<- read.table("./test/y_test.txt", sep = "\n", stringsAsFactors = FALSE)
X_test <- read.table("./test/X_test.txt", sep = "", stringsAsFactors = FALSE)

## assgin column names by the corresponding features
colnames(X_train) <- features$features
colnames(X_test) <- features$features

## convert the data frame to tibble for dplyr functions
## the function won't work without specifing indices. (strange)
X_train <- as_tibble(X_train[1:561])
X_test <- as_tibble(X_test[1:561])

## add to each set two columns of subject and activity
train <- X_train %>% mutate(personID = subject_train$V1, actID = y_train$V1)
test <- X_test %>% mutate(personID = subject_test$V1, actID = y_test$V1)

## merge the data to a full data set and create a new
## tidy date containing the mean of each requested variables
full <- full_join(train, test)
useful <- full[grepl("mean", colnames(full)) | grepl("std", colnames(full))]
tidy_data <- useful %>%
        mutate(subject = full$personID, activity = full$actID) %>%
        group_by(.dots = c("subject", "activity")) %>%
        summarize_all(mean)
tidy_data$activity <- mapvalues(tidy_data$activity,
        from = act_labels$actlabel, to =  tolower(act_labels$activity))





