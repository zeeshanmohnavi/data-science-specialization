
## 
## @Author: Md Zeeshan Mohnavi (https://github.com/zeeshanmohnavi)
##
## This utility is created as a helper file for the assignment
## of Course #3: Getting and Cleaning Data offered on Coursera
## by John Hopkins University. It is intended for personal use only,
## and does not intend to violate Coursera Honor Code.
##


library(readr)
library(dplyr)

# Load the helper files

source(paste0(getwd(), "/util/_util_extract_zip.R"))
source(paste0(getwd(), "/course-3/project/scripts/_util_create_header.R"))

# Extract the zip archive containing the data files

file_name <- "UCI HAR Dataset.zip"
target_dir <- paste(getwd(), "/course-3/project/data", sep = "")
extract_then_delete_zip(file_name, target_dir)


# Initialize the paths for the training and test directories

parent_dir <- paste0(getwd(), "/course-3/project/data/UCI HAR Dataset/")
test_dir <- paste0(parent_dir, "test/")
train_dir <- paste0(parent_dir, "train/")


# Read the list of features present in the observations.
# `features` contain a list of 561 elements, each element
# representing one variable in the observation.

feature_filename <- "features.txt"
features <- read.table(file = paste0(parent_dir, feature_filename),
                       sep = " ")[, 2] %>%
  sapply(function(x) {t <- strsplit(x, " ")})


# Read the activity names, which consists of the activity
# captured along with a numerical id corresponding to activity.
# Set the column names of the data frame accordingly.

activity_labels_filename <- "activity_labels.txt"
activity <- read_table(file = paste0(parent_dir, activity_labels_filename),
                       col_names = FALSE)
colnames(activity) <- c("activity_id", "activity")


# Read from the data files and create a labeled test data frame.
# The resulting data frame also captures the subject who captured
# the observation, and the activity id of the corresponding observation.

test_label_filename <- "y_test.txt"
test_data_filename <- "X_test.txt"
test_subject_filename <- "subject_test.txt"

test_labels <- read_table(file = paste0(test_dir, test_label_filename),
                          col_names = FALSE)
colnames(test_labels) <- "activity"

test_subject <- read_table(file = paste0(test_dir, test_subject_filename),
                           col_names = FALSE)
colnames(test_subject) <- "subject"

test_data <- read_table(file = paste0(test_dir, test_data_filename),
                        col_names = FALSE)
colnames(test_data) <- features
test_data <- bind_cols(test_subject, test_labels, test_data)


# Read from the data files and create a labeled training data frame.
# The resulting data frame also captures the subject on who captured
# the observation, and the activity id of the corresponding observation.

train_label_filename <- "y_train.txt"
train_data_filename <- "X_train.txt"
train_subject_filename <- "subject_train.txt"

train_labels <- read_table(file = paste0(train_dir, train_label_filename),
                           col_names = FALSE)
colnames(train_labels) <- "activity"

train_subject <- read_table(file = paste0(train_dir, train_subject_filename),
                            col_names = FALSE)
colnames(train_subject) <- "subject"

train_data <- read_table(file = paste0(train_dir, train_data_filename),
                         col_names = FALSE)
colnames(train_data) <- features
train_data <- bind_cols(train_subject, train_labels, train_data)

# Merge the training and the test data into a single data frame
merged_raw <- bind_rows(train_data, test_data)

## ---------------------------------------------------------------------------
#  Requirement 1 of the assignment completed here
#  Requirement 1: Merges the training and the test sets to create one data set
## ---------------------------------------------------------------------------


# Subset the merged data taking into account only the
# measurements on mean and standard deviation

headers <- colnames(train_data)
header_pattern <- "^activity|^subject|([\\.]*-(mean|std)\\(\\))-?[A-Z]?$"
header_subset_criteria <- grep(header_pattern, headers)

merged_data <- select(merged_raw, header_subset_criteria)

## ---------------------------------------------------------------------------
#  Requirement 2 of the assignment completed here
#  Requirement 2: Extracts only the measurements on the mean and
#  standard deviation for each measurement.
## ---------------------------------------------------------------------------


# Replace activity's ids by activity's names in the merged data set

merged_data <- merge(activity, merged_data,
                     by.x = "activity_id",
                     by.y = "activity", sort = FALSE) %>% 
  tbl_df %>%
  mutate(activity_id = NULL)

# Reorder the column of merged data set to make the subject 
# variable as the first column and the activity_name variable
# to be the second column of the data frame.

merged_data <- merged_data[, c(2, 1, 3:66)] %>%
  group_by(subject, activity)

# table(merged_data$activity)  # For testing
# table(merged_data$subject, merged_data$activity)  # For testing
# sum(table(merged_data$subject, merged_data$activity))  # For testing

## ---------------------------------------------------------------------------
#  Requirement 3 of the assignment completed here
#  Requirement 3: Use descriptive activity names to name the
#  activities in the data set.
## ---------------------------------------------------------------------------

# Rename the header to contain self-explanatory names

col_header <- create_header(colnames(merged_data))
col_header[1:2] <- c("subject", "activity")

colnames(merged_data) <- col_header

## ---------------------------------------------------------------------------
#  Requirement 4 of the assignment completed here
#  Requirement 4: Appropriately labels the data set with descriptive
#  variable names
## ---------------------------------------------------------------------------


## Create an independent tidy data set with the average
## of each variable for each activity and each subject

final_tidy_data <- merged_data %>%
  summarise_all(mean)

# View(final_tidy_data)  # For testing

# Write the tidy data set to a directory
write.table(final_tidy_data,
            file = paste0(getwd(), "/course-3/project/output/tidy.txt"),
            row.names = FALSE)

## ---------------------------------------------------------------------------
#  Requirement 5 of the assignment completed here
#  Requirement 5: from the merged data set, create another,
#  independent "tidy" data set with the average of each
#  variable for each activity and each subject.
## ---------------------------------------------------------------------------


## ---------------------------------------------------------------------------
#  View the tidy data set in R. The below lines are only for sanity check.
## ---------------------------------------------------------------------------

# Read tidy data from directory
tidy_loaded <- read.table(paste0(getwd(), "/course-3/project/output/tidy.txt"),
                          header = TRUE, sep = " ")
View(tidy_loaded)
