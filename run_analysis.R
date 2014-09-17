require("data.table")
require("reshape2")
require("plyr")

#have an error with fread, use read.table and convert to data.table
openData <- function(path, colClasses="numeric", col_name = NULL) {
  if (is.null(col_name)) {
    data <- read.table(path, header = FALSE, sep = "" , colClasses = colClasses)  
  } else {
    data <- read.table(path, header = FALSE, sep = "" , colClasses = colClasses, col.names = col_name)    
  }
  data.table(data)
}

#set path
#setwd("/your/working/directory")
path <- getwd()

#features
features_path  <- file.path(path, "UCI HAR Dataset/features.txt")
#activity label
activity_labels_path <- file.path(path, "UCI HAR Dataset/activity_labels.txt")
#train
X_train_path <- file.path(path, "UCI HAR Dataset/train/X_train.txt")
y_train_path <- file.path(path, "UCI HAR Dataset/train/y_train.txt")
subject_train_path <- file.path(path, "UCI HAR Dataset/train/subject_train.txt")
#test
X_test_path <- file.path(path, "UCI HAR Dataset/test/X_test.txt")
y_test_path <- file.path(path, "UCI HAR Dataset/test/y_test.txt")
subject_test_path <- file.path(path, "UCI HAR Dataset/test/subject_test.txt")

#open and concat train
data_train <- openData(X_train_path)
data_train <- cbind(data_train, openData(y_train_path,"integer","activity_id"))
data_train <- cbind(data_train, openData(subject_train_path,"integer","subject"))

#open and concat test
data_test <- openData(X_test_path)
data_test <- cbind(data_test, openData(y_test_path,"integer","activity_id"))
data_test <- cbind(data_test, openData(subject_test_path,"integer","subject"))

#concat test and train and get keys
data <- rbind(data_train, data_test)
setkey(data, subject, activity_id)

#features : open and extract mean and standart deviation
data_features <- openData(features_path, c("integer","character"),c("feature_id","feature"))
data_features <- data_features[grepl("mean\\(\\)|std\\(\\)", data_features$feature),]

#get mean, standart deviation cols (feature_id)  and key cols (subject, activity_id)
data_features$feature_id <- sapply("V", paste0,data_features$feature_id)
data <- data[,c(data_features$feature_id,key(data)), with=FALSE]

#get activity labels
data_labels <- openData(activity_labels_path, c("integer","character"), c("activity_id", "activity"))

#merge data with labels
data <- merge(data, data_labels, by = "activity_id", all.x = TRUE)
setkey(data, subject, activity_id, activity)

#merge features with data
d <- melt(data, key(data), variable.name="feature_id")
d <- merge(d, data_features, by = "feature_id", all.x = TRUE)
setkey(d, feature, activity, subject)

#get a tidy_data
tidy_data <- ddply(d, key(d), summarise, mean=mean(value))

#write tidy data
write.table(tidy_data, file = "tidy_data.txt",row.name=FALSE)

