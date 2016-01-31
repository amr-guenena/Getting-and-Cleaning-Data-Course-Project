# Set Work Directory
#setwd("C:/____Courses/Data_Science/03_Getting and cleaning Data/Project")

# Create Data subfolder
if(!file.exists("./data")){dir.create("./data")}

# Download data file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Proj_Data.zip")

#Unzip DataSet to /data directory
unzip(zipfile="./data/Proj_Data.zip",exdir="./data")


# Load libraries
library(dplyr)
library(data.table)
library(tidyr)
library(plyr)
# load tables

# subject
tdf_subject_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/subject_train.txt"))
tdf_subject_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/subject_test.txt"))

# Activity
tdf_activity_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/y_train.txt"))
tdf_activity_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/y_test.txt"))

# data
tdf_data_train <- tbl_df(read.table("./data/UCI HAR Dataset/train/x_train.txt"))
tdf_data_test <- tbl_df(read.table("./data/UCI HAR Dataset/test/x_test.txt"))

# Merge Subject Training and Test Tdfs
tdf_subject_all<-rbind(tdf_subject_train,tdf_subject_test)
# rename field
setnames(tdf_subject_all,"V1","subject")

# Merge activity Training and Test Tdf
tdf_activity_all<-rbind(tdf_activity_train,tdf_activity_test)
# rename field
setnames(tdf_activity_all,"V1","activity_num")

# Merge data Training and Test Tdf
tdf_data_all<-rbind(tdf_data_train,tdf_data_test)

# features 
tdf_features <- tbl_df(read.table("./data/UCI HAR Dataset/features.txt"))
# rename fields
setnames(tdf_features, names(tdf_features), c("featureNum", "featureName"))
colnames(tdf_data_all) <- tdf_features$featureName

#column names for activity labels
#Activity labels
tdf_activity_labels<- tbl_df(read.table("./data/UCI HAR Dataset/activity_labels.txt"))
# rename fields
setnames(tdf_activity_labels, names(tdf_activity_labels), c("activity_num","activity_name"))


# Merge columns
tdf_data_Subj_act<- cbind(tdf_subject_all, tdf_activity_all)
tdf_data_all <- cbind(tdf_data_Subj_act, tdf_data_all)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# get field names for Mean and Std 
fields_mean_std <- grep("mean\\(\\)|std\\(\\)",tdf_features$featureName,value=TRUE) 
# add subject activity to previous field list
fields_sub_act_mean_std <- union(c("subject","activity_num"), fields_mean_std )
#create a new data table for relevant fields
data_sub_act_mean_std<-subset(tdf_data_all,select=fields_sub_act_mean_std)

#3. Uses descriptive activity names to name the activities in the data set
## add activity name to data table
data_sub_act_mean_std <- merge(tdf_activity_labels, data_sub_act_mean_std , by="activity_num", all.x=TRUE)
data_sub_act_mean_std$activity_name <- as.character(data_sub_act_mean_std$activity_name)

## create data.Table with variable means sorted by subject and Activity
data_aggr<- aggregate(. ~subject + activity_name, data = data_sub_act_mean_std, mean)
data_table<- tbl_df(arrange(data_aggr,subject,activity_name))

#4. Appropriately labels the data set with descriptive variable names.

names(data_table)<-gsub("^t", "time", names(data_table))
names(data_table)<-gsub("^f", "frequency", names(data_table))
names(data_table)<-gsub("Acc", "Accelerometer", names(data_table))
names(data_table)<-gsub("Gyro", "Gyroscope", names(data_table))
names(data_table)<-gsub("Mag", "Magnitude", names(data_table))
names(data_table)<-gsub("BodyBody", "Body", names(data_table))

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data<-data_table

write.table(tidy_data, file = "tidydata.txt",row.name=FALSE)
