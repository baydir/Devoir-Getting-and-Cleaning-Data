run_analysis <- function() {
  
##loading all the files  
subject_train <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/subject_train.txt")
X_train <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/X_train.txt")
y_train <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/y_train.txt")
subject_test <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/subject_test.txt")
X_test <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/X_test.txt")
y_test <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/y_test.txt")
features <- read.table("/Users/baydir/Desktop/UCI HAR Dataset/features.txt")

## merging the files for train and test
subject <- rbind(subject_test,subject_train)
X <- rbind(X_test,X_train)
y <- rbind(y_test,y_train)

## changing the names of the columns 
## especially for X
colnames(subject)[1] <- "subject" 
colnames(X) <- features[,"V2"]
colnames(y) <- "activity"

## taking only the value of mean and standard deviation
## and renaming labels for a tidy dataset
X<- X[,grepl("mean|std",names(X))]
names(X) <- gsub('-mean', 'Mean', names(X))
names(X) <- gsub('-std', 'Std', names(X))
names(X) <- gsub('[-()]', '', names(X))

## Column binding of subject, activity, and measures
data <- cbind(subject,X,y)

## Replacing each activity by its label 
## Not convinced it is the better solution to do it but it works
## End of step 4 
data <- tbl_df(data)
data <- mutate(data,activity=ifelse(activity==1,"WALKING",ifelse(activity==2,"WALKING_UPSTAIRS",ifelse(activity==3,"WALKING_DOWNSTAIRS",ifelse(activity==4,"SITTING",ifelse(activity==5,"STANDING","LAYING"))))))

## Averaging each variable for each activity and each subject
second_tidy_data <- data %>% group_by(subject,activity) %>% summarise_each(funs(mean))

write.table(second_tidy_data, "secont_tidy_data.txt") 
}