##Cleans up dataset from UCI HAR Dataset
##assumes in "UCI HAR Dataset" folder directory
run_analysis.R <- function() {
  
  ####Reads in test data###
  X_test<-read.table("test/X_test.txt")
  Y_test<-read.table("test/y_test.txt")
  subject_test<-read.table("test/subject_test.txt")
  
  #Merges test values into a single dataframe
  testdf<-X_test
  testdf<-cbind(subject_test[,1],Y_test[,1],X_test)
  
  #Add headings
  features<-read.table("features.txt")
  names(testdf)<- c("subject","activity",as.vector(features$V2))
  
  #Add descriptions to activities ie. "walking" instead of numbers
 # activity_labels<-read.table("activity_labels.txt")
#  testdf$activity<-activity_labels$V2[match(testdf$activity,activity_labels$V1)]
   
  #Subsets into only columns with "mean" and "std" ignores columns with capital "Mean" intentionally as this does not seem to be the intended info 
  cleantestdf<-testdf[,grepl("activity|subject|mean|std",names(testdf))]
 
  ###Reads in train data###
  
  X_train<-read.table("train/X_train.txt")
  Y_train<-read.table("train/y_train.txt")
  subject_train<-read.table("train/subject_train.txt")
  #Merges train values into a single dataframe
  traindf<-X_train
  traindf<-cbind(subject_train[,1],Y_train[,1],X_train)
  
  #Add headings
  features<-read.table("features.txt")
  names(traindf)<- c("subject","activity",as.vector(features$V2))
   
  #Subsets into only columns with "mean" and "std" ignores columns with capital "Mean" intentionally as this does not seem to be the intended info
  cleantraindf<-traindf[,grepl("activity|subject|mean|std",names(traindf))]
   
  #merge both df into final (could probably consolidate these steps to merge test and train dfs first)
  mergedf<-merge(cleantraindf,cleantestdf,all=TRUE)
  
  
   
   #subsets the data into subjects and activities
   tidydf <- data.frame()
   for (i in 1:30) {
     subject<- subset(mergedf,subject==i)
     for (j in 1:6){
       actv<- subset(subject, activity==j)
       temp<-as.vector(apply(actv,2,mean))
       tidydf<-rbind(tidydf,temp) 
     }
     
   }
   #Add appropriate column names
   activity_labels<-read.table("activity_labels.txt")
   colnames(tidydf)<-colnames(mergedf)
   levels(tidydf[,2])<-activity_labels$V2

   #Output resulting dataframe to a text file
   write.table(tidydf,file="Tidydataset.txt", sep = "",row.name=FALSE)
   
   }#end function
