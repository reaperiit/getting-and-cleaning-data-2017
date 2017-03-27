
library(data.table)
library(dplyr)

#part 1

#download and unzip dataset
if(!file.exists("./data")){dir.create("./data")}
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "./data/Dataset.zip")
unzip(zipfile = "./data/Dataset.zip",exdir = "./data")

#reading training dataset
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
subject_train<- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#reading activity labels and feature vector
activity_labels<- read.table("./data/UCI HAR Dataset/activity_labels.txt")
features<- read.table("./data/UCI HAR Dataset/features.txt")

#reading test dataset
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
subject_test<- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#merge train and test datasets
colnames(activity_labels)<- c("activityID","activityType")

colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <-"subjectID"

colnames(x_test )<- features[,2]
colnames(y_test) <-"activityID"
colnames(subject_test)<- "subjectID"

#merge train and test datasets

train <- cbind(y_train,subject_train, x_train)
test <- cbind(y_test,subject_test, x_test)
final<- rbind(train,test)

#part 2
#set with mean and standard deviation for each measurement

desiredColumns<-grep(".*Mean.*|.*Std.*", names(final), ignore.case=TRUE)
desiredColumns<- c(1,2,desiredColumns)
meanAndStd <- final[,desiredColumns]


#part 3
#set with mean and Standard Deviations std,  with Activity Names
meanAndStd$activityID <- as.character(meanAndStd$activityID)
for(i in 1:6){
  meanAndStd$activityID[meanAndStd$activityID == i] = as.character(activity_labels[i,2])
}
meanAndStd$activityID <- as.factor(meanAndStd$activityID)

names(meanAndStd)


#part 4
#Appropriately label the dataset with descriptive variable names
names(meanAndStd)<-gsub("Acc", "Accelerometer", names(meanAndStd))
names(meanAndStd)<-gsub("Gyro", "Gyroscope", names(meanAndStd))
names(meanAndStd)<-gsub("BodyBody", "Body", names(meanAndStd))
names(meanAndStd)<-gsub("Mag", "Magnitude", names(meanAndStd))
names(meanAndStd)<-gsub("^t", "Time", names(meanAndStd))
names(meanAndStd)<-gsub("^f", "Frequency", names(meanAndStd))
names(meanAndStd)<-gsub("tBody", "TimeBody", names(meanAndStd))
names(meanAndStd)<-gsub("-mean()", "Mean", names(meanAndStd), ignore.case = TRUE)
names(meanAndStd)<-gsub("-std()", "STD", names(meanAndStd), ignore.case = TRUE)
names(meanAndStd)<-gsub("-freq()", "Frequency", names(meanAndStd), ignore.case = TRUE)
names(meanAndStd)<-gsub("angle", "Angle", names(meanAndStd))
names(meanAndStd)<-gsub("gravity", "Gravity", names(meanAndStd))

#part 5
#from meanAndStd dataset,   create a second, independent tidy data set with the average of each 
#variable for each activity and each subject

meanAndStd$subjectID <- as.factor(meanAndStd$subjectID)
meanAndStd<- data.table(meanAndStd)

tidyData <- aggregate(. ~subjectID + activityID, meanAndStd, mean)
tidyData <- tidyData[order(tidyData$subjectID,tidyData$activityID),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
