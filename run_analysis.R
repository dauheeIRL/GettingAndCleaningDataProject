#------------------------Assignment Instructions-------------------------------------------------------------------------
#You should create one R script called run_analysis.R that does the following. 
#1-Merges the training and the test sets to create one data set.
#2-Extracts only the measurements on the mean and standard deviation for each measurement. 
#3-Uses descriptive activity names to name the activities in the data set
#4-Appropriately labels the data set with descriptive variable names. 
#5-Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#------------------------------------------------------------------------------------------------------------------------

#before running this script need to call setwd() and set to the relevant directory such as below:
#setwd("C:\\Per\\_Courseara_Data_Science\\3-Getting and Cleaning Data\\project")

#a running cleanup setting values to NULL is included but commented out as not needed
#a suggestion for speed enhancement would be to use data.table instead of data.frame

#firstly get the main datasets
testDF <- read.table("./test/X_test.txt")
trainDF <- read.table("./train/X_train.txt")

#determine that both DF have identical columns and so can be merged without error
identical(names(testDF), names(trainDF))

#merge
mergedDF<-rbind(testDF,trainDF)
#testDF<-NULL
#trainDF<-NULL

#get the proper names for the value columns
coldataDF<-read.table("./features.txt", stringsAsFactors=FALSE)
#confirmed that the values list matches the column list
identical(as.integer(length(names(mergedDF))), nrow(coldataDF))

#We are keeping the case of features.txt entries as will more than likely copying/pasting column names when looking to select 
# - so don't need to make it easier for typing by putting in lowercase. Also the existing capitilization helps distinguish the words
names(mergedDF)<-coldataDF$V2

#these are the columns to filter - this is only including the features that have mean() or std() or meanFreq(). There are other
#features such as angle(X,gravityMean) but I have decided to not include as although it is a mean, it is using the mean against
#a non-mean value such as X
mergedDF<-mergedDF[,coldataDF[grep("mean()|std()|meanFreq()", coldataDF$V2),]$V1] 
#coldataDF<-NULL

#now get the activity data
testlabelDF<-read.table("./test/y_test.txt")
trainlabelDF<-read.table("./train/y_train.txt")

#add the activity data to the dataset
mergedDF<-cbind(rbind(testlabelDF, trainlabelDF), mergedDF)
names(mergedDF)[1]<-c("activity")
#trainlabelDF<-NULL
#testlabelDF<-NULL

#translate the activities into readable format
activitylabelDF<-read.table("./activity_labels.txt", stringsAsFactors=FALSE)
decodeactivity<-function(x){activitylabelDF[x,2]}
mergedDF$activity<-sapply(mergedDF$activity,decodeactivity)
#activitylabelDF<-NULL

#obtaining the subject data
testsubject<-read.table("./test/subject_test.txt")
trainsubject<-read.table("./train/subject_train.txt")

#adding the subject data
finalDF<-cbind(c(testsubject$V1,trainsubject$V1), mergedDF)
#mergedDF<-NULL
#testsubject<-NULL
#trainsubject<-NULL
names(finalDF)[1]<-"subject"

#now getting the mean for all values by subject,activity
finalDF<-aggregate(.~subject+activity, data=finalDF, mean)

#run the following to create the data frame for upload to coursera:
write.table(finalDF, file="Step5DF.txt", row.name=FALSE)
