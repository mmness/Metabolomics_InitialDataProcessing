##here is an R code that first renames files in a document. 
#you first have to make a .csv file that has two columns named "oldname" and "newname". fill out these columns with the names you want to replace
#making sure the .csv naming guide has .csv/.mzML/.whatever at the end of each file name

#make sure these packages are installed before running. type "install.packages("dplyr")" or whatever you are missing 
library(dplyr)
library(stringr)
library(readr)

#rename as needed
naming_guide <- read.csv("namingGuide.csv")

for (i in 1:nrow(naming_guide)) {
  old_name <- naming_guide$oldname[i]
  new_name <- naming_guide$newname[i]
    old_file_path <- list.files(pattern = old_name)
    if (length(old_file_path) > 0) {
    new_file_path <- str_replace(old_file_path, pattern = old_name, replacement = new_name)
    file.rename(old_file_path, new_file_path)
  } else {
    cat("File", old_name, "not found.\n") #prints files not renamed from the naming guide
  }
}



#################part 2-- sorting files
##########if you want to sort some files


partial_names <- read.csv("filename.csv", header = TRUE, stringsAsFactors = FALSE)
partial_names <- partial_names$sampleid #Put in column name you want to move

# Define the source and destination folders
source_folder <- "PATH/TO/FOLDER/WHERE/THEY/ARE"
destination_folder <- "PATH/TO/FOLDER/YOU/WANT/THEM/TO/GO"

# List files in the source folder
files <- list.files(source_folder, full.names = TRUE)

# Loop through
for (partial_name in partial_names) {
  matching_files <- grep(partial_name, files, value = TRUE, fixed = TRUE)
  for (matching_file in matching_files) {
    destination_path <- file.path(destination_folder, basename(matching_file))
    file.copy(matching_file, destination_path, overwrite = FALSE)
  }
}






