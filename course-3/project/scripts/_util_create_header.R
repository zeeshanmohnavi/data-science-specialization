
## 
## @Author: Md Zeeshan Mohnavi (https://github.com/zeeshanmohnavi)
##
## This utility is created as a helper file for the assignment
## of Course #3: Getting and Cleaning Data offered on Coursera
## by John Hopkins University. It is intended for personal use only,
## and does not intend to violate Coursera Honor Code.
##

library(stringr)

## Create descriptive header values for the column names of a data 
## frame. There are no specific rules used to create the new headers;
## the only objective is to make them self explanatory.
##
## Arguments: old_header - a list of old header values
## Return Value: a list of new header values of the length as
##               the old header list
##

create_header <- function(old_header) {
  
  # In some of the header values, the substring "Body" gets
  # repeated twice, which seems to be a typographical mistake.
  # Example: fBodyBodyGyroMag-mean()
  # The extra token (Body) is removed from the headers to 
  # ease downstream processing of data.
  # The `old_header_refined` variable contains the updated list.
  
  repeat_word_pattern <- "^(t|f)(Body){1}\\2(.*)$"
  
  old_header_refined <- c()
  for (i in old_header) {
    if (!grepl(repeat_word_pattern, i)) {
      old_header_refined <- append(old_header_refined, i)
    } else {
      t <- str_match(i, repeat_word_pattern)
      temp <- paste0(t[, c(2, 3, 4)], collapse = "")
      old_header_refined <- append(old_header_refined, temp)
    }
  }
  
  # The header values contains concatenation of abbreviated
  # terms which have their usual meanings. The variable 
  # `token_name` contains a list of the abbreviated tokens used
  # for the headers. The variable `token_value` contains a
  # slightly long version of the tokens which replaces the
  # corresponding shorter abbreviations.
  
  header_pattern <- "^(t|f)$?(Body|Gravity)(Acc|Gyro)(Jerk)?(Mag)?-(mean|std)\\(\\)-?([XYZ])?$"
  matched_header <- str_match(old_header_refined, header_pattern)
  
  token_name <- c("t", "f", "Body", "Gravity", "Acc",
                  "Gyro", "Jerk", "Mag", "mean", "std",
                  "X", "Y", "Z")
  token_value <- c("time", "freq", "_body", "_gravity",
                   "_accelerometer", "_gyrometer",
                   "_jerk", "_magnitude", "_mean",
                   "_sd", "_X_axis", "_Y_axis", "_Z_axis")
  
  new_header <- c()
  for (i in 1:nrow(matched_header)) {
    var_name <- ""
    for (token in matched_header[i, 2:8]) {
      if (!is.na(token)) {
        var_name <- paste0(var_name,
                           token_value[which(token == token_name)],
                           collapse = "")
      }
      next
    }
    new_header <- append(new_header, var_name)
  }
  new_header
}


# ## ----------------------------------------------------------------
# To be used for testing
#
# old_header <- c("tBodyAcc-mean()-X",
#                 "tBodyAcc-std()-Y",
#                 "tBodyAcc-std()-Z",
#                 "tGravityAcc-mean()-Z",
#                 "tGravityAcc-std()-X",
#                 "tBodyAccJerk-mean()-X",
#                 "tBodyAccJerk-std()-X",
#                 "tBodyAccJerkMag-mean()",
#                 "tBodyAccJerkMag-std()",
#                 "tBodyGyroMag-mean()",
#                 "tBodyGyroMag-std()",
#                 "tBodyGyroJerkMag-mean()",
#                 "tBodyGyroJerkMag-std()",
#                 "fBodyBodyGyroMag-mean()",
#                 "fBodyAccJerk-std()-X")
# 
# new_header <- create_header(old_header)
# new_header
