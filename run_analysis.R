# Get zip file and extract
library(RCurl)
dataBin <- getBinaryURL("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")

## write binary data to zip file
con <- file("dataset.zip", open = "wb")
writeBin(dataBin, con)
close(con)

## unpack zipfile
unzip("dataset.zip")

# Step 1: Merges the training and the test sets to create one data set.

library(dplyr)

## get the train set
train_subject<-
  read.table("UCI HAR Dataset/train/subject_train.txt", comment.char = "", header = F)

train_x<-
  read.table("UCI HAR Dataset/train/X_train.txt", comment.char = "", header = F)

train_y<-
  read.table("UCI HAR Dataset/train/y_train.txt", comment.char = "", header = F)

training_set <-
  train_subject %>%
  cbind(train_x) %>%
  cbind(train_y)

## get the test set
test_subject<-
  read.table("UCI HAR Dataset/test/subject_test.txt", comment.char = "", header = F)

test_x<-
  read.table("UCI HAR Dataset/test/X_test.txt", comment.char = "", header = F)

test_y<-
  read.table("UCI HAR Dataset/test/y_test.txt", comment.char = "", header = F)

testing_set <-
  test_subject %>%
  cbind(test_x) %>%
  cbind(test_y)

full_set<-
  rbind(training_set,
        testing_set)

feature_names <-
  read.table("UCI HAR Dataset/features.txt", header=F)

names(full_set)<-
  c("subject", as.character(feature_names[,2]), "Y")


# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement

## do grep to extract column indices
regex_str<- "-(std|mean)\\(\\)"
cols_i<-
  grep(regex_str, names(full_set))  

mean_std_subset <-
  full_set[, cols_i]

# Step 3: Uses descriptive activity names to name the activities in the data set

activity_labels <-
  read.table("UCI HAR Dataset/activity_labels.txt", header = F)

full_set$Y_activity_labels <-
  factor(full_set$Y, levels = activity_labels[,1], labels = activity_labels[,2])

# Step 4: Appropriately labels the data set with descriptive variable names
# colnames are already given earlier, only cleaning up here

colLabels <- 
  colnames(full_set) %>%
  gsub("(-)", "_", .) %>%
  gsub("(\\(\\))", "", .) %>%
  gsub("(,)", "|", .)

nonUniqueLabels<-
  colLabels[duplicated(colLabels)]

nonUniqueLabels_indices <-
  c(1:length(colLabels))[duplicated(colLabels)]

uniqueLabels <-
  nonUniqueLabels # for now :)


for(lab in unique(nonUniqueLabels)){
  count = 0
  
  for(lab_i in c(1:length(nonUniqueLabels))[nonUniqueLabels == lab]){
    
    uniqueLabels[lab_i] <- paste0(nonUniqueLabels[lab_i], "_", count)    
    count = count + 1
  }
}

colLabels[nonUniqueLabels_indices] <- uniqueLabels


colnames(full_set) <-
  colLabels

# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(tidyr)

full_set_average <-
  full_set %>%
  select(-Y) %>%
  group_by(subject, Y_activity_labels) %>%
  summarise_each(funs(mean))

write.table(full_set_average, "full_set_average.txt", row.names=F)

