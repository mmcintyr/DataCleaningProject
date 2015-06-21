library(dplyr)

##############
## Overview ##
##############

# For each record it is provided:
#     ======================================
#
# - Triaxial acceleration from the accelerometer (total acceleration) and
#   the estimated body acceleration.
# - Triaxial Angular velocity from the gyroscope.
# - A 561-feature vector with time and frequency domain variables.
# - Its activity label.
# - An identifier of the subject who carried out the experiment.
#
# The dataset includes the following files:
#     =========================================
#
# - 'README.txt'
# - 'features_info.txt'     : Shows information about the variables used on the
#                             feature vector.
# - 'features.txt'          : List of all features.
# - 'activity_labels.txt'   : Links the class labels with their activity name.
# - 'train/X_train.txt'     : Training set.
# - 'train/y_train.txt'     : Training labels.
# - 'test/X_test.txt'       : Test set.
# - 'test/y_test.txt'       : Test labels.
#
# The following files are available for the train and test data. Their
# descriptions are equivalent.
#
# - 'train/subject_train.txt': Each row identifies the subject who performed the
#    activity for each window sample. Its range is from 1 to 30.
#
# - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from
#    the smartphone accelerometer X axis in standard gravity units 'g'. Every
#    row shows a 128 element vector. The same description applies for the
#    'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z
#    axis.
#
# - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal
#    obtained by subtracting the gravity from the total acceleration.
#
# - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector
#    measured by the gyroscope for each window sample. The units are
#    radians/second.
#

####
# Load all of the raw data so that we can start our reshaping
####

# The following structure is assumed for the data. The dataRoot node will need to be
# modified for your particular installation.
#
# <dataRoot>
# |   activity_labels.txt
# |   features.txt
# |   features_info.txt
# |   README.txt
# |
# +---test
# |   |   subject_test.txt
# |   |   X_test.txt
# |   |   y_test.txt
# |   |
# \---train
#     |   subject_train.txt
#     |   x_train.txt
#     |       y_train.txt
#     |

# Modify for your particular installation
dataRoot = "C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset"
dataRootFiles = c("activity_labels.txt", "features.txt")

# Remaining directories are relative to the dataRoot node
testRoot = paste(dataRoot, "test", sep="/")
testRootFiles = c("subject_test.txt","X_test.txt","y_test.txt")

trainRoot = paste(dataRoot, "train", sep="/")
trainRootFiles = c("subject_train.txt","X_train.txt","y_train.txt")


#########################

# List of all features.
features <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/features.txt", quote="\"")

# Train data set
X_train <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/train/X_train.txt", quote="\"")

# Test data set
X_test <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/test/X_test.txt", quote="\"")

# Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30
subject_train <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/train/subject_train.txt", quote="\"")

# Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30
subject_test <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/test/subject_test.txt", quote="\"")

# Train labels
y_train <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/train/y_train.txt", quote="\"")

# Test labels
y_test <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/test/y_test.txt", quote="\"")

# Load activity labels
activity_labels <- read.table("C:/Users/Mike/OneDrive/Documents/Coursera/Course Project/UCI HAR Dataset/activity_labels.txt", quote="\"")

# Let's build up the Train data set first
# Next, let's rename the column names
colnames(X_train) = features[[2]]
colnames(X_test)  = features[[2]]

# Let's give the subject column a more meaningful name
colnames(subject_train) = "Subject"
colnames(subject_test)  = "Subject"

# Now, let's insert the Subjects into each of the datasets
X_train = cbind(subject_train, X_train)
X_test  = cbind(subject_test, X_test)

m_train = merge(activity_labels, y_train, by="V1")
m_test  = merge(activity_labels, y_test,  by="V1")
colnames(m_train) = c("ID", "Activity")
colnames(m_test)  = c("ID", "Activity")
m_train = subset(m_train, select = -c(ID))
m_test  = subset(m_test, select = -c(ID))

# Now, let's bring in the activities for each of the subjects
X_train = cbind(m_train, X_train)
X_test  = cbind(m_test, X_test)

# Extract the mean and stdev columns from the train dataset
train_mean = X_train[grepl(glob2rx('*mean*'), colnames(X_train))]
train_std  = X_train[grepl(glob2rx('*std*'), colnames(X_train))]
train_act  = subset(X_train, select=c(Activity,Subject))
train_temp = cbind(train_mean, train_std)
tidy_train = cbind(train_act, train_temp)
#rm(train_mean, train_std, train_act, train_temp)


# Extract the mean and stdev columns from the test dataset
test_mean = X_test[grepl(glob2rx('*mean*'), colnames(X_test))]
test_std  = X_test[grepl(glob2rx('*std*'), colnames(X_test))]
test_act  = subset(X_test, select=c(Activity,Subject))
test_temp = cbind(test_mean, test_std)
tidy_test  = cbind(test_act,  test_temp)
#rm(test_mean, test_std, test_act, test_temp)


# Let's create our all-up tidy dataset
tidy_data = rbind(tidy_train, tidy_test)

# Don't need these temporary tables anymore. Let's get rid of them
datasetToDelete = c("X_test", "X_train", "activity_labels", "features",
                    "m_test", "m_train", "subject_test", "subject_train",
                    "y_test", "y_train", "tidy_train", "tidy_test",
                    "train_mean", "train_std", "train_act", "train_temp",
                    "test_mean", "test_std", "test_act", "test_temp")
rm(list=datasetToDelete)


dataGroupedBySubject = tidy_data %>% group_by(Subject) %>% summarise_each(funs(mean))
dataGroupedBySubject = subset(dataGroupedBySubject, select = -Activity)

dataGroupedByActivity = tidy_data %>% group_by(Activity) %>% summarise_each(funs(mean))
dataGroupedByActivity = subset(dataGroupedByActivity, select = -Subject)



#
#
#
buildFilePathCollection <- function(rootFolder, listOfFiles)
{
    fileCollection = c()

    for(file in listOfFiles)
    {
        fileCollection = c(fileCollection, sprintf("%s/%s", rootFolder, file))
    }
    fileCollection
}