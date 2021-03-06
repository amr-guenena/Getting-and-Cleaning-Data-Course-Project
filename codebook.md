##Source Data
A full description of the data used in this project can be found at The UCI Machine Learning Repository  
[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones]  (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)  

The source data for this project can be found here.  
[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip]
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)  

##Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities 
### Data Set Files
* 'features_info.txt': Shows information about the variables used on the feature vector.
* 'features.txt': List of all features.
* 'activity_labels.txt': Links the class labels with their activity name.
* 'train/X_train.txt': Training set.
* 'train/y_train.txt': Training labels.
* 'test/X_test.txt': Test set.
* 'test/y_test.txt': Test labels.
The following files are available for the train and test data. Their descriptions are equivalent. 
* 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
* 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
* 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
* 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

###Activity labels (activity_labels.txt)
* WALKING  (value 1): subject was walking during the test
* WALKING_UPSTAIRS  (value 2): subject was walking up a staircase during the test
* WALKING_DOWNSTAIRS (value 3): subject was walking down a staircase during the test
* SITTING (value 4): subject was sitting during the test
* STANDING  (value 5): subject was standing during the test
* LAYING  (value 6): subject was laying down during the test

wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.
###Features (features_info.txt)
* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag 

##Recipe
### Preparation
1. Download data files to data directory
2. Extract ( Unzip) the downloaded file
3. Load required libraries  
4. Load and merge tables  
..* subject (Train and test)  
..* Activity (Train and test)  
..* data  (Train and test)  
5. Rename fields  

###1. Merge the training and the test sets to create one data set.  
Assign column names and merge to create one data set. (tdf_data_all)  
  
###2. Extracts only the measurements on the mean and standard deviation for each measurement.   
1. get field names for Mean and Std  
2.  add subject activity to previous field list  
3. create a new data table for relevant fields (data_sub_act_mean_std)   
  
###3. Uses descriptive activity names to name the activities in the data set  
* add activity name to data table  
* create data.Table with variable means sorted by subject and Activity (data_table)  
  
###4. Appropriately labels the data set with descriptive variable names.  
* replace ^t with time  
* replace ^f with frequency  
* replace "Acc" with "Accelerometer"  
* replace "Gyro" with "Gyroscope"  
* replace "Mag" with "Magnitude"  
* replace "BodyBody" with "Body"  
  
###5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
tidy_data<-data_table  

### run the script run_analysis.R to generate required tidydata.txt


## Tidy Data set Structure  
str(tidy_data)  
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	180 obs. of  69 variables:  
 $ subject                                       : int  1 1 1 1 1 1 2 2 2 2 ...  
 $ activity_name                                 : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...  
 $ activity_num                                  : num  6 4 5 1 3 2 6 4 5 1 ...  
 $ timeBodyAccelerometer-mean()-X                : num  0.222 0.261 0.279 0.277 0.289 ...  
 $ timeBodyAccelerometer-mean()-Y                : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...  
 $ timeBodyAccelerometer-mean()-Z                : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...  
 $ timeBodyAccelerometer-std()-X                 : num  -0.928 -0.977 -0.996 -0.284 0.03 ...  
 $ timeBodyAccelerometer-std()-Y                 : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...  
 $ timeBodyAccelerometer-std()-Z                 : num  -0.826 -0.94 -0.98 -0.26 -0.23 ...  
 $ timeGravityAccelerometer-mean()-X             : num  -0.249 0.832 0.943 0.935 0.932 ...  
 $ timeGravityAccelerometer-mean()-Y             : num  0.706 0.204 -0.273 -0.282 -0.267 ...  
 $ timeGravityAccelerometer-mean()-Z             : num  0.4458 0.332 0.0135 -0.0681 -0.0621 ...  
 $ timeGravityAccelerometer-std()-X              : num  -0.897 -0.968 -0.994 -0.977 -0.951 ...  
 $ timeGravityAccelerometer-std()-Y              : num  -0.908 -0.936 -0.981 -0.971 -0.937 ...  
 $ timeGravityAccelerometer-std()-Z              : num  -0.852 -0.949 -0.976 -0.948 -0.896 ...  
 $ timeBodyAccelerometerJerk-mean()-X            : num  0.0811 0.0775 0.0754 0.074 0.0542 ...  
 $ timeBodyAccelerometerJerk-mean()-Y            : num  0.003838 -0.000619 0.007976 0.028272 0.02965 ...  
 $ timeBodyAccelerometerJerk-mean()-Z            : num  0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...  
 $ timeBodyAccelerometerJerk-std()-X             : num  -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...  
 $ timeBodyAccelerometerJerk-std()-Y             : num  -0.924 -0.981 -0.986 0.067 -0.102 ...  
 $ timeBodyAccelerometerJerk-std()-Z             : num  -0.955 -0.988 -0.992 -0.503 -0.346 ...  
 $ timeBodyGyroscope-mean()-X                    : num  -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...  
 $ timeBodyGyroscope-mean()-Y                    : num  -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...  
 $ timeBodyGyroscope-mean()-Z                    : num  0.1487 0.0629 0.0748 0.0849 0.0901 ...  
 $ timeBodyGyroscope-std()-X                     : num  -0.874 -0.977 -0.987 -0.474 -0.458 ...  
 $ timeBodyGyroscope-std()-Y                     : num  -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...  
 $ timeBodyGyroscope-std()-Z                     : num  -0.908 -0.941 -0.981 -0.344 -0.125 ...  
 $ timeBodyGyroscopeJerk-mean()-X                : num  -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...  
 $ timeBodyGyroscopeJerk-mean()-Y                : num  -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...  
 $ timeBodyGyroscopeJerk-mean()-Z                : num  -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...  
 $ timeBodyGyroscopeJerk-std()-X                 : num  -0.919 -0.992 -0.993 -0.207 -0.487 ...  
 $ timeBodyGyroscopeJerk-std()-Y                 : num  -0.968 -0.99 -0.995 -0.304 -0.239 ...  
 $ timeBodyGyroscopeJerk-std()-Z                 : num  -0.958 -0.988 -0.992 -0.404 -0.269 ...  
 $ timeBodyAccelerometerMagnitude-mean()         : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...  
 $ timeBodyAccelerometerMagnitude-std()          : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...  
 $ timeGravityAccelerometerMagnitude-mean()      : num  -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...  
 $ timeGravityAccelerometerMagnitude-std()       : num  -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...  
 $ timeBodyAccelerometerJerkMagnitude-mean()     : num  -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...  
 $ timeBodyAccelerometerJerkMagnitude-std()      : num  -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...  
 $ timeBodyGyroscopeMagnitude-mean()             : num  -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...  
 $ timeBodyGyroscopeMagnitude-std()              : num  -0.819 -0.935 -0.979 -0.187 -0.226 ...  
 $ timeBodyGyroscopeJerkMagnitude-mean()         : num  -0.963 -0.992 -0.995 -0.299 -0.295 ...  
 $ timeBodyGyroscopeJerkMagnitude-std()          : num  -0.936 -0.988 -0.995 -0.325 -0.307 ...  
 $ frequencyBodyAccelerometer-mean()-X           : num  -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...  
 $ frequencyBodyAccelerometer-mean()-Y           : num  -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...  
 $ frequencyBodyAccelerometer-mean()-Z           : num  -0.883 -0.959 -0.985 -0.332 -0.226 ...  
 $ frequencyBodyAccelerometer-std()-X            : num  -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...  
 $ frequencyBodyAccelerometer-std()-Y            : num  -0.834 -0.917 -0.972 0.056 -0.113 ...  
 $ frequencyBodyAccelerometer-std()-Z            : num  -0.813 -0.934 -0.978 -0.28 -0.298 ...  
 $ frequencyBodyAccelerometerJerk-mean()-X       : num  -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...  
 $ frequencyBodyAccelerometerJerk-mean()-Y       : num  -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...  
 $ frequencyBodyAccelerometerJerk-mean()-Z       : num  -0.948 -0.986 -0.991 -0.469 -0.288 ...  
 $ frequencyBodyAccelerometerJerk-std()-X        : num  -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...  
 $ frequencyBodyAccelerometerJerk-std()-Y        : num  -0.932 -0.983 -0.987 0.107 -0.135 ...  
 $ frequencyBodyAccelerometerJerk-std()-Z        : num  -0.961 -0.988 -0.992 -0.535 -0.402 ...  
 $ frequencyBodyGyroscope-mean()-X               : num  -0.85 -0.976 -0.986 -0.339 -0.352 ...  
 $ frequencyBodyGyroscope-mean()-Y               : num  -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...  
 $ frequencyBodyGyroscope-mean()-Z               : num  -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...  
 $ frequencyBodyGyroscope-std()-X                : num  -0.882 -0.978 -0.987 -0.517 -0.495 ...  
 $ frequencyBodyGyroscope-std()-Y                : num  -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...  
 $ frequencyBodyGyroscope-std()-Z                : num  -0.917 -0.944 -0.982 -0.437 -0.238 ...  
 $ frequencyBodyAccelerometerMagnitude-mean()    : num  -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...  
 $ frequencyBodyAccelerometerMagnitude-std()     : num  -0.798 -0.928 -0.982 -0.398 -0.187 ...  
 $ frequencyBodyAccelerometerJerkMagnitude-mean(): num  -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...  
 $ frequencyBodyAccelerometerJerkMagnitude-std() : num  -0.922 -0.982 -0.993 -0.103 -0.104 ...  
 $ frequencyBodyGyroscopeMagnitude-mean()        : num  -0.862 -0.958 -0.985 -0.199 -0.186 ...  
 $ frequencyBodyGyroscopeMagnitude-std()         : num  -0.824 -0.932 -0.978 -0.321 -0.398 ...  
 $ frequencyBodyGyroscopeJerkMagnitude-mean()    : num  -0.942 -0.99 -0.995 -0.319 -0.282 ...  
 $ frequencyBodyGyroscopeJerkMagnitude-std()     : num  -0.933 -0.987 -0.995 -0.382 -0.392 ...  
   
## rows of the dataset  

tidy_data  
Source: local data frame [180 x 69]  

   subject      activity_name activity_num timeBodyAccelerometer-mean()-X timeBodyAccelerometer-mean()-Y  
     (int)              (chr)        (dbl)                          (dbl)                          (dbl)  
1        1             LAYING            6                      0.2215982                   -0.040513953  
2        1            SITTING            4                      0.2612376                   -0.001308288  
3        1           STANDING            5                      0.2789176                   -0.016137590  
4        1            WALKING            1                      0.2773308                   -0.017383819  
5        1 WALKING_DOWNSTAIRS            3                      0.2891883                   -0.009918505  
6        1   WALKING_UPSTAIRS            2                      0.2554617                   -0.023953149  
7        2             LAYING            6                      0.2813734                   -0.018158740  
8        2            SITTING            4                      0.2770874                   -0.015687994  
9        2           STANDING            5                      0.2779115                   -0.018420827  
10       2            WALKING            1                      0.2764266                   -0.018594920  
..     ...                ...          ...                            ...                            ...  
Variables not shown: timeBodyAccelerometer-mean()-Z (dbl), timeBodyAccelerometer-std()-X (dbl),  
  timeBodyAccelerometer-std()-Y (dbl), timeBodyAccelerometer-std()-Z (dbl), timeGravityAccelerometer-mean()-X  
  (dbl), timeGravityAccelerometer-mean()-Y (dbl), timeGravityAccelerometer-mean()-Z (dbl),  
  timeGravityAccelerometer-std()-X (dbl), timeGravityAccelerometer-std()-Y (dbl), timeGravityAccelerometer-std()-Z  
  (dbl), timeBodyAccelerometerJerk-mean()-X (dbl), timeBodyAccelerometerJerk-mean()-Y (dbl),  
  timeBodyAccelerometerJerk-mean()-Z (dbl), timeBodyAccelerometerJerk-std()-X (dbl),  
  timeBodyAccelerometerJerk-std()-Y (dbl), timeBodyAccelerometerJerk-std()-Z (dbl), timeBodyGyroscope-mean()-X  
  (dbl), timeBodyGyroscope-mean()-Y (dbl), timeBodyGyroscope-mean()-Z (dbl), timeBodyGyroscope-std()-X (dbl),  
  timeBodyGyroscope-std()-Y (dbl), timeBodyGyroscope-std()-Z (dbl), timeBodyGyroscopeJerk-mean()-X (dbl),  
  timeBodyGyroscopeJerk-mean()-Y (dbl), timeBodyGyroscopeJerk-mean()-Z (dbl), timeBodyGyroscopeJerk-std()-X (dbl),  
  timeBodyGyroscopeJerk-std()-Y (dbl), timeBodyGyroscopeJerk-std()-Z (dbl), timeBodyAccelerometerMagnitude-mean()  
  (dbl), timeBodyAccelerometerMagnitude-std() (dbl), timeGravityAccelerometerMagnitude-mean() (dbl),  
  timeGravityAccelerometerMagnitude-std() (dbl), timeBodyAccelerometerJerkMagnitude-mean() (dbl),  
  timeBodyAccelerometerJerkMagnitude-std() (dbl), timeBodyGyroscopeMagnitude-mean() (dbl),  
  timeBodyGyroscopeMagnitude-std() (dbl), timeBodyGyroscopeJerkMagnitude-mean() (dbl),  
  timeBodyGyroscopeJerkMagnitude-std() (dbl), frequencyBodyAccelerometer-mean()-X (dbl),  
  frequencyBodyAccelerometer-mean()-Y (dbl), frequencyBodyAccelerometer-mean()-Z (dbl),  
  frequencyBodyAccelerometer-std()-X (dbl), frequencyBodyAccelerometer-std()-Y (dbl),  
  frequencyBodyAccelerometer-std()-Z (dbl), frequencyBodyAccelerometerJerk-mean()-X (dbl),  
  frequencyBodyAccelerometerJerk-mean()-Y (dbl), frequencyBodyAccelerometerJerk-mean()-Z (dbl),  
  frequencyBodyAccelerometerJerk-std()-X (dbl), frequencyBodyAccelerometerJerk-std()-Y (dbl),  
  frequencyBodyAccelerometerJerk-std()-Z (dbl), frequencyBodyGyroscope-mean()-X (dbl),  
  frequencyBodyGyroscope-mean()-Y (dbl), frequencyBodyGyroscope-mean()-Z (dbl), frequencyBodyGyroscope-std()-X  
  (dbl), frequencyBodyGyroscope-std()-Y (dbl), frequencyBodyGyroscope-std()-Z (dbl),  
  frequencyBodyAccelerometerMagnitude-mean() (dbl), frequencyBodyAccelerometerMagnitude-std() (dbl),  
  frequencyBodyAccelerometerJerkMagnitude-mean() (dbl), frequencyBodyAccelerometerJerkMagnitude-std() (dbl),  
  frequencyBodyGyroscopeMagnitude-mean() (dbl), frequencyBodyGyroscopeMagnitude-std() (dbl),  
  frequencyBodyGyroscopeJerkMagnitude-mean() (dbl), frequencyBodyGyroscopeJerkMagnitude-std() (dbl)  


