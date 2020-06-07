
## 
## @Author: Md Zeeshan Mohnavi (https://github.com/zeeshanmohnavi)
##
## Extracts a ZIP archive file (with a .zip extension) into
## a user provided target directory. The source ZIP is then
## deleted from the directory.
## This function has no exception handling in its implementation.
## So, use it carefully.
##
## Arguments: file_name - name of the zip file
##            target_dir - directory where file is to be unzipped
## Return Value: TRUE, if file.remove() successfully deletes the
##                     zip file
##               FALSE, otherwise
##


extract_then_delete_zip <- function(file_name, target_dir) {
  if (!file.exists(target_dir)) {
    dir.create(target_dir)
  }
  source_zip_file <- paste(target_dir, "/", file_name, sep = "")
  unzip(source_zip_file, exdir = target_dir)
}

## ----------------------------------------------------------------
# # For standalone testing of the function
# 
# file_name <- "UCI HAR Dataset.zip"
# target_dir <- paste(getwd(), "/course-3/project/data", sep = "")
# extract_then_delete_zip(file_name, target_dir)
