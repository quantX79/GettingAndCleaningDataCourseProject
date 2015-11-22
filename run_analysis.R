library(plyr)
library(dplyr)
library(reshape)

# Activity Labels
activityLabels<-read.table("./UCI HAR Dataset/activity_labels.txt")
names(activityLabels)<-c("ActivityClass","ActivityName")

# Features
features<-read.table("./UCI HAR Dataset/features.txt")
names(features)<-c("Indx","Name")

# Merges the training and the test sets to create one data set.
sfun <- function(x) paste0('subject',as.character(x))
fullDataSet <- data.frame()
setTypes <- c("train","test")
for(setType in setTypes)
{
  # Data
  dataSetFile <- paste0("./UCI HAR Dataset/",setType,"/","X_",setType,".txt");
  dataSet<-read.table(dataSetFile)
  
  # Labels
  labelSetFile <- paste0("./UCI HAR Dataset/",setType,"/","Y_",setType,".txt");
  labelSet<-read.table(labelSetFile)
  names(labelSet)<-c("ActivityClass")
  labelSet<-merge(labelSet,activityLabels,by.x = "ActivityClass",by.y="ActivityClass")
  
  dataSet$ActivityClass<-labelSet$ActivityClass
  dataSet$ActivityName<-labelSet$ActivityName
  
  # Subject
  subjectSetFile <- paste0("./UCI HAR Dataset/",setType,"/","subject_",setType,".txt");
  subjectSet<-read.table(subjectSetFile)
  names(subjectSet)<-c("SubjectNum")
  
  subjectSet$SubjectName <- factor(sapply(subjectSet$SubjectNum,sfun))

  dataSet$SubjectNum<-subjectSet$SubjectNum
  dataSet$SubjectName<-subjectSet$SubjectName
  
  fullDataSet <- rbind(fullDataSet,dataSet)
  remove(dataSetFile,labelSetFile,setType,dataSet,labelSet,subjectSetFile,subjectSet)
}
remove(setTypes)

## Extracts only the measurements on the mean and standard deviation for each measurement.
xFeatures <- features[sort(union(grep("\\bmean\\b",features$Name),grep("\\bstd\\b",features$Name))),]

## Uses descriptive activity names to name the activities in the data set
xDataSet <- fullDataSet[,c("SubjectName","ActivityName")]
xDataSet <- cbind(xDataSet,fullDataSet[,xFeatures$Indx])

## Appropriately label the data set with descriptive variable names. 
xFeatures$AbbrName <- gsub("[-,]","_",gsub("[()]","",xFeatures$Name))
names(xDataSet) <- c("SubjectName","ActivityName",xFeatures$AbbrName)

# From the last data set, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
mXDataSet <- melt(xDataSet, id.vars = c("SubjectName", "ActivityName"))
groupedDataSetMeans<-cast(SubjectName + ActivityName ~ variable, data = mXDataSet, fun = mean)
write.table(groupedDataSetMeans,file = "./groupedDataSetMeans.txt",row.name=FALSE)
