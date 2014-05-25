##This script downloads, imports, merges, and summarizes Smartphone activity data described in the READ_ME.md file.  

###Download data################################################################

##download zip file from project url and unzip file 

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, "Dataset.zip")
dateDownloaded<-date()
unzip("Dataset.zip")


###Import data####################################################################

##Train data files possess 7352 rows (21 patients)
##Test data files possess 2947 rows  (9 subjects)

###Import subject id numbers for train and test groups
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")

###Import 561 columns of measurements for train and test groups
X.train <- read.table("UCI HAR Dataset/train/X_train.txt")
X.test <- read.table("UCI HAR Dataset/test/X_test.txt")

##Import activity code numbers for train and test groups
y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
y.test <- read.table("UCI HAR Dataset/test/y_test.txt")

##Import column names for the 561 measurements 
features <- read.table("UCI HAR Dataset/features.txt")

##Import activity labels key (6 activity labels)
activities <- read.table("UCI HAR Dataset/activity_labels.txt")


###Combine train and test data#######################################################

##combine subject id numbers from train and test groups (10299 total rows, order: train then test)
subject.all<-factor(rbind(subject.train,subject.test)[,1])  ##make subject numbers into factor

##combine measurement datasets (561 columns) from train and test groups (10299 total rows, order: train then test)
X.all<-rbind(X.train,X.test)

##combine activity code numbers from train and test groups (10299 total rows, order: train then test)
y.all<-rbind(y.train,y.test)

###Clean activity labels#######################################################

##clean activities variables to make lower case and remove underscores 
activities[,2]<-tolower(activities[,2])
activities[,2]<-gsub("_","",activities[,2])

##use activities key for repalcing activity ids with activity name labels in y.all

for(i in 1:dim(activities)[1]){
    y.all[y.all==i]<-activities[i,2]
    
}
##make activity names into factors
activity.factor<-factor(y.all[,1],levels=c("walking","walkingupstairs","walkingdownstairs","sitting","standing","laying"))


###Create merged dataframe with Subject ids, Activity labels, Measurements########

feature.names<-as.character(features[,2])  ##make measurement names character for use as column names
group<-as.factor(c(rep("train",dim(y.train)[1]),(rep("test",dim(y.test)[1]))))  ##make group labels
combined.df<-cbind(subject.all,activity.factor,group,X.all)  ##combined dataframe (10299 rows by 564 columns (2 header columns + 561 columns of measurements)
colnames(combined.df)<-c("subject","activity","group",feature.names)  ##make column names


###Create reduced dataframe with only mean() and std() measurement colums and header columns###########

logic.columns.to.subset<-grepl("subject|activity|group|mean[(]|std[(]",colnames(combined.df))

reduced.df<-combined.df[,logic.columns.to.subset]   ##new df contains 10299 rows and 69 columns



###Clean measurement colnames names to make more legible and usable#####################################

##remove (), and replace "," or "-" with dots

fix<-colnames(reduced.df)
fix<-gsub(pattern="\\(|\\)","",fix)
fix<-gsub(pattern="[,-]",".",fix)
colnames(reduced.df)<-fix

###Reshape reduced dataframe into a table of subject by activity by taking the mean for each activity per subject

library(reshape2)  ##for melt and dcast functions

vars<-colnames(reduced.df[,4:dim(reduced.df)[2]])   ##only use numeric columns for measurement variables (remove header colums)
dfMelt<-melt(reduced.df,id=c("subject","activity","group"),measure.vars=vars,na.rm=TRUE)  ##melt measurement variables for subject and activity

tidy<-dcast(dfMelt, subject + activity + group~ variable, fun.aggregate=mean)

range.tidy<-apply(tidy,2,range)

###Export tidy dataset

write.table(tidy, "tidy_data_smartphone_measurements_by_subject_by_activity.txt", sep="\t",row.names=FALSE) 

###Export tidy dataset names and ranges for use in CodeBook.md

write.csv(t(range.tidy),"listOfVariables_ranges.csv")



