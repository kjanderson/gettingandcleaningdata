library(dplyr)

print("run_analysis.R")

# load data from text files
root_path <- file.path("~/Downloads/UCI HAR Dataset")

path.variable.names <- file.path(root_path, "features.txt")
path.activity.names <- file.path(root_path, "activity_labels.txt")

# print("loading variable names...")
data.variable.names <- read.table(path.variable.names, sep="")
# print("loading activity names...")
data.activity.names <- read.table(path.activity.names, sep="")

path.train <- file.path(root_path, "train")
path.train.subject <- file.path(path.train, "subject_train.txt")
path.train.xdata <- file.path(path.train, "X_train.txt")
path.train.ydata <- file.path(path.train, "y_train.txt")
# print("loading training subject data...")
data.train.subject <- read.table(path.train.subject, sep="")
# print("loading training data...")
data.train.xdata <- read.table(path.train.xdata, sep="")
# print("loading training activity data...")
data.train.ydata <- read.table(path.train.ydata, sep="")

path.test <- file.path(root_path, "test")
path.test.subject <- file.path(path.test, "subject_test.txt")
path.test.xdata <- file.path(path.test, "X_test.txt")
path.test.ydata <- file.path(path.test, "y_test.txt")
# print("loading test subect data...")
data.test.subject <- read.table(path.test.subject, sep="")
# print("loading test data...")
data.test.xdata <- read.table(path.test.xdata, sep="")
# print("loading test activity data...")
data.test.ydata <- read.table(path.test.ydata, sep="")

# step 1: merge training data and test data
data.merge.xdata <- rbind(data.train.xdata, data.test.xdata)

# step 2: extract only the data for mean and sd for each measurement
index.means <- grep("^.*mean\\(", data.variable.names[,2])
index.sd <- grep("^.*std", data.variable.names[,2])
index.concat <- c(index.means, index.sd)
index <- sort(index.concat)
data.subset.xdata <- data.merge.xdata[,index]

# step 3: use descriptive activity names to name the activities
data.test.names <- as.character(data.activity.names[data.test.ydata[,1],2])
data.train.names <- as.character(data.activity.names[data.train.ydata[,1],2])
data.merge.names <- c(data.train.names, data.test.names)
data.mutated.xdata <- mutate(data.subset.xdata, names=data.merge.names)

# step 4: use descriptive names for each variable
variable.labels <- as.character(data.variable.names[index,2])
var.sub.labels <- variable.labels
var.sub.labels <- sub("t", "", var.sub.labels)
var.sub.labels <- sub("f", "fft ", var.sub.labels)
var.sub.labels <- sub("BodyAccJerk", "Body Jerk", var.sub.labels)
var.sub.labels <- sub("BodyGyro", "Body Gyro", var.sub.labels)
var.sub.labels <- sub("BodyAcc", "Body Accel", var.sub.labels)
var.sub.labels <- sub("GravityAcc", "Gravity Accel", var.sub.labels)
var.sub.labels <- sub("GyroJerk", "Gyro Jerk", var.sub.labels)
var.sub.labels <- sub("(Accel|Gyro|Jerk)Mag", "\\1 Mag", var.sub.labels)
var.sub.labels <- sub("BodyBody", "Body", var.sub.labels)
var.sub.labels <- sub("sd\\(\\)", "std\\(\\)", var.sub.labels)
var.sub.labels <- c(var.sub.labels, "activity")
names(data.mutated.xdata) <- var.sub.labels

# step 5: create an independent tidy data set with average of each
#         variable for each activity and each subject
# TODO: move the activity and subject columns to the first two columns
#       (there seems to be NA data in these columns when this is done)
data.merge.subject <- rbind(data.train.subject, data.test.subject)
data.merge.subject <- data.merge.subject[,1]
data.mutated.xdata <- mutate(data.mutated.xdata, subjects=data.merge.subject)

# tidy up the data: group by activity and subject, take means
data.tidy <- data.mutated.xdata
for(i in levels(as.factor(data.mutated.xdata$subjects)))
{
    for(j in levels(as.factor(data.mutated.xdata$activity)))
    {
        # create logical index for subject
        subject.index <- data.mutated.xdata$subjects == as.integer(i)
        # create logical index for activity
        activity.index <- data.mutated.xdata$activity == as.character(j)
        index <- subject.index & activity.index
# msg <- paste("subjects: ", as.character(sum(subject.index)), "activities: ", as.character(sum(activity.index)), "indices: ", as.character(sum(index)))
# print(msg)
        for(k in 1:(ncol(data.mutated.xdata)-2))
        {
            # compute the mean of each data block and populate in target
            mean_val <- mean(data.mutated.xdata[index, k])
            data.tidy[index, k] <- rep(mean_val, sum(index))
        }
    }
}

data.tidy <- unique(data.tidy)
# write the table out as a .txt file in the current directory
write.table(data.tidy, file="output data.txt", row.names=FALSE)

