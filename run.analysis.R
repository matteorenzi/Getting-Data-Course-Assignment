## 1.
#set working directory
#load datasets from UCI HAR data

setwd ("/Users/Matt/Desktop/UCI HAR Dataset/.") 
features = read.table('./features.txt',header=FALSE) 
activityType = read.table('./activity_labels.txt',header=FALSE)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain = read.table('./train/x_train.txt',header=FALSE) 
yTrain = read.table('./train/y_train.txt',header=FALSE) 
subjectTest = read.table('./test/subject_test.txt',header=FALSE) 
xTest = read.table('./test/x_test.txt',header=FALSE) 
yTest = read.table('./test/y_test.txt',header=FALSE)
# set new column names
colnames(activityType) = c('activityId','activityType')
colnames(subjectTrain) = "subjectId"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityId"
#merge datasets
trainingData = cbind(yTrain,subjectTrain,xTrain)
#assign new column names
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2]
colnames(yTest) = "activityId"
#merge again the test datasets
test = cbind(yTest,subjectTest,xTest)
#merge to create final dataset
final = rbind(trainingData,test)
#create new column names
colNames = colnames(final)
#2.
#use grep command to extract a vector containing the following words
LV = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
#subset final data 
final = final[LV==TRUE]
#3.
#merge final data with activitytype
final = merge(final,activityType,by='activityId',all.x=TRUE)
# Updating the colNames vector to include the new column names after merge
colNames = colnames(final)
#4.
# Cleaning up the variable names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
# Reassigning the new descriptive column names to the final set
colnames(final) = colNames;
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# Create a new table, finalNoActivityType without the activityType column
finalNoActivityType = final[,names(final) != 'activityType']
# Summarizing the finalNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalNoActivityType[,names(finalNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalNoActivityType$activityId,subjectId = finalNoActivityType$subjectId),mean)
# Merging the tidyData with activityType to include descriptive acitvity names
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE)
# Export the tidyData set
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
