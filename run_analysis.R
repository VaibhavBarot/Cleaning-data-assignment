
   #load necessary packages
        
   library(dplyr)
   library(tidyr)
        
   #import feature and activity data:
   act_labels <- read.table(file.path('UCI HAR Dataset','activity_labels.txt'), header=FALSE, col.names=c('activity_code', 'activity_name'))
   features <- read.table(file.path('UCI HAR Dataset','features.txt'),header=FALSE, col.names=c('feature_id', 'feature_name'))
  
    #import test data 
   temp_test <- read.table(file.path('UCI HAR Dataset','test','X_test.txt'), header=FALSE, col.names=features$feature_name)

   activity_sub_test <- read.table(file.path('UCI HAR Dataset','test','Y_test.txt'), header=FALSE, col.names='activity_code')
  
   subject_test <- read.table(file.path('UCI HAR Dataset','test','subject_test.txt'), header=FALSE, col.names='subject_id')
   
   #import train data
   temp_train <- read.table(file.path('UCI HAR Dataset','train','X_train.txt'), header=FALSE, col.names=features$feature_name)
   
   activity_sub_train <- read.table(file.path('UCI HAR Dataset','train','Y_train.txt'), header=FALSE, col.names='activity_code')
   
   subject_train <- read.table(file.path('UCI HAR Dataset','train','subject_train.txt'), header=FALSE, col.names='subject_id')
   
   #bind the columns to create test and train data tables including information on subjects and activity:
   test_data <- cbind( activity_sub_test,subject_test, temp_test)
   train_data <- cbind( activity_sub_train,subject_train, temp_train)
   
   #1. Merge into one table to include test and train subjects
   data_merged <- merge(test_data, train_data, by = intersect(names(test_data), names(train_data)), all=TRUE, no.dups=TRUE)
   
   #2. Extract only the measurements on the mean and standard deviation for each measurement
   col_selection <- grepl('mean|std', names(data_merged))
   data_selected<- select( data_merged, activity_code, subject_id, names(data_merged[col_selection]))
  
   #3. Use descriptive activity names to name the activities in the data set
   #to achieve this merge the table with the table containing acitivy names
   data_clean<- merge(act_labels,data_selected, by='activity_code', all=TRUE)
   # convert activity names to lower case 
   data_clean$activity_name <- tolower( data_clean$activity_name )
   
   #4. Appropriately label the data set with descriptive variable names
   names(data_clean) <- gsub('^t','t_',names(data_clean))
   names(data_clean) <- gsub('^f','f_',names(data_clean))
   names(data_clean) <- gsub('Acc','accelometer_',names(data_clean))
   names(data_clean) <- gsub('Gyro','gyroscope_',names(data_clean))
   names(data_clean) <- gsub('Body','Body_',names(data_clean))
   names(data_clean) <- gsub('Body_Body_','Body_',names(data_clean))
   names(data_clean) <- gsub('\\.','_',names(data_clean))
   names(data_clean) <- gsub('___','_',names(data_clean))
   names(data_clean) <- gsub('__','_',names(data_clean))
   names(data_clean) <- gsub('_$','',names(data_clean))

   #5. From the data set in step 4, create a second, independent tidy data set 
   #with the average of each variable for each activity and each subject.
   final_data <- data_clean %>% mutate(-activity_code) %>% group_by(activity_code, activity_name, subject_id) %>% summarize_all(mean)
   final_table <- write.table(final_data, file='final_data.txt')

