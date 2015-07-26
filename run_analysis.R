#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of
#   each variable for each activity and each subject.


#change dir
if(getwd() != "C:/R/3/Getting-Data-project") setwd("C:/R/3/Getting-Data-project")

library(data.table)
library(plyr)

#load in "features" and "activity_labels" first - they will be the column names of the sensor data
features <- read.table("./UCI HAR Dataset/features.txt", sep="")
head(features)

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep="")
activity_labels <- activity_labels$V2
head(activity_labels)


#now load in the 3 training and 3 test data files
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep="", col.names=("subject"))

x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep="", col.names=as.character(features[,2]))

y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", sep="", col.names=("activity"))

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", col.names=("subject"))

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep="", col.names=as.character(features[,2]))

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", sep="", col.names=("activity"))


#Part 3, convert contents of "y" (which are numbers) into strings, by referencing "activity_labels"

y_test1 <- replace(y_test, y_test==1, as.character(activity_labels[1]))
y_test2 <- replace(y_test1, y_test==2, as.character(activity_labels[2]))
y_test3 <- replace(y_test2, y_test==3, as.character(activity_labels[3]))
y_test4 <- replace(y_test3, y_test==4, as.character(activity_labels[4]))
y_test5 <- replace(y_test4, y_test==5, as.character(activity_labels[5]))
y_test6 <- replace(y_test5, y_test==6, as.character(activity_labels[6]))

y_train1 <- replace(y_train, y_train==1, as.character(activity_labels[1]))
y_train2 <- replace(y_train1, y_train==2, as.character(activity_labels[2]))
y_train3 <- replace(y_train2, y_train==3, as.character(activity_labels[3]))
y_train4 <- replace(y_train3, y_train==4, as.character(activity_labels[4]))
y_train5 <- replace(y_train4, y_train==5, as.character(activity_labels[5]))
y_train6 <- replace(y_train5, y_train==6, as.character(activity_labels[6]))

#bind y and x
z_train <- cbind(y_train6, x_train)
data_train <- cbind(subject_train, z_train)

z_test <- cbind(y_test6, x_test)
data_test <- cbind(subject_test, z_test)

#Part 1, merge training and test data
data <- rbind(data_train, data_test)

#Part 2, extract only the data regarding mean and std
mean_col <- grep("mean", names(data))
std_col <- grep("std", names(data))
sum(mean_col %in% std_col*1)             #check for overlap
sum(std_col %in% mean_col*1)             #check for overlap
data_cut <- data[,sort(c(2,1,mean_col, std_col))]


#part 5
final_data<-ddply(data_cut, c("subject","activity"), numcolwise(mean))

write.table(finalData, file = "getdata_proj.txt", row.name=FALSE)