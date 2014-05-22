#This R script run_analysis.R does the following: 
#Merges the training and the test sets (X_test and X_train) to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Creates a tidy data set with the average of each variable for each activity and each subject. 
#The dataset is saved in average.txt 

require(stats)

#read in the activity and feature name files to be used in dataframe and tables
activity <- read.table("UCI HAR Dataset/activity_labels.txt") #6 activity names
features <- read.table("UCI HAR Dataset/features.txt") # 561 variable names

# clean up the feature names to be easily handled
features$V2 <- tolower(as.character(features$V2)) # change factor class to lower case character
features$V2 <- gsub("\\(\\)", "", features$V2) #get rid of () in naming
features$V2 <- gsub("-", ".", features$V2) # replace "-" with "."
features$V2 <- gsub(",", "", features$V2) # replace "," with "."

#get features with names containing "mean" or "std" only: nmean=names for mean variables; nstd=names for std variables
nmean <- features[grepl("mean", features$V2), ] # subset only the variables with "mean" in it
nstd <- features[grepl("std", features$V2), ] # subset only the variables with "std" in it

#create a volunteer(subject) ID vector corresponding to the dataset
subject_train_ID <- read.table("UCI HAR Dataset/train/subject_train.txt") #7352 rows of 1 col for volunteer ID(1-30)
subject_test_ID <- read.table("UCI HAR Dataset/test/subject_test.txt") #2947 rows of 1 col for volunteer ID(1-30)
subject_ID <- rbind(subject_train_ID, subject_test_ID) # corresponding volunteer ID for each row of the dataset

#create a label (activity) ID vector corresponding to the dataset
label_train <- read.table("UCI HAR Dataset/train/y_train.txt") #7352 rows of 1 col for label ID(1-6)
label_test <- read.table("UCI HAR Dataset/test/y_test.txt") #2947 rows of 1 col for label ID(1-6)
label_ID <- rbind(label_train, label_test) #corresponding label_ID for each row of the dataset

#combine the train and test datasets together to make one dataframe
X_train <- read.table("UCI HAR Dataset/train/X_train.txt") #7352x561 training dataset
X_test <- read.table("UCI HAR Dataset/test/X_test.txt") #2947x561 test dataset
X_all <- rbind(X_train, X_test) #all dataset combined into one data frame

#make a data.frame with the activity(label) ID, the volunteer ID, and the dataset
together <- cbind(X_all, label_ID, subject_ID)
names(together) <- c(features$V2, "activity", "volunteer") #pass the name to the columns

#this loop calculate the average of all the mean_variables by activity type
avg.meanvar.activity <- NULL
for (i in nmean$V2) {
a <- tapply(together[,i], together$activity, mean)
avg.meanvar.activity <- data.frame(cbind(avg.meanvar.activity, a))
}
rownames(avg.meanvar.activity) <- activity$V2
colnames(avg.meanvar.activity) <- nmean$V2

#this loop calculate the average of all the std_variables by activity type
avg.stdvar.activity <- NULL
for (i in nstd$V2) {
        c <- tapply(together[,i], together$activity, mean)
        avg.stdvar.activity <- data.frame(cbind(avg.stdvar.activity, c))
}
rownames(avg.stdvar.activity) <- activity$V2
colnames(avg.stdvar.activity) <- nstd$V2

#this loop calculate the average of all the mean_variables by volunteer ID
avg.meanvar.volunteer <- NULL
for (i in nmean$V2) {
        e <- tapply(together[,i], together$volunteer, mean)
        avg.meanvar.volunteer <- data.frame(cbind(avg.meanvar.volunteer, e))
}
rownames(avg.meanvar.volunteer) <- paste("person #", 1:30)
colnames(avg.meanvar.volunteer) <- nmean$V2

#this loop calculate the average of all the std_variables by volunteer ID
avg.stdvar.volunteer <- NULL
for (i in nstd$V2) {
        g <- tapply(together[,i], together$volunteer, mean)
        avg.stdvar.volunteer <- data.frame(cbind(avg.stdvar.volunteer, g))
}
rownames(avg.stdvar.volunteer) <- paste("person #", 1:30)
colnames(avg.stdvar.volunteer) <- nstd$V2

#now combine all the calculated data into one dataframe and save it to a .txt file average.txt
avg.byperson <- data.frame(cbind(avg.meanvar.volunteer, avg.stdvar.volunteer))
avg.byactivity <- data.frame(cbind(avg.meanvar.activity, avg.stdvar.activity))

avg.all <- data.frame(rbind(avg.byactivity, avg.byperson))
write.table(avg.all, file = "UCI HAR Dataset/average.txt", sep = "\t") 

##initially I was going to calculate avg from the inertia files, but since it only ask for the mean and std variables, I skipped these.
##Inertia_total_train_x <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
##Inertia_total_train_y <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
##Inertia_total_train_z <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")
##body_gyro_train_x <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
##body_gyro_train_y <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
##body_gyro_train_z <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")
##body_acc_train_x <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
##body_acc_train_y <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
##body_acc_train_z <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")

