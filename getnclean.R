library(plyr)

# 1. Merge the training and the test sets to create one data set.

# read in test and training data
xtest <- read.table('test/X_test.txt')
xtrain <- read.table('train/X_train.txt')

# combine test and train data
xall <- rbind(xtest,xtrain)

# read in feature labels
features <- read.table('features.txt')

# Add column names for combined test and train data
colnames(xall) <- features[,2]

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
xmeanstd <- xall[,grepl("mean\\(\\)|std\\(\\)", colnames(xall))] 

# 3. Use descriptive activity names to name the activities in the data set

# read in activity number IDs
ytest <- read.table('test/Y_test.txt')
ytrain <- read.table('train/y_train.txt')

# combine activity number IDs
yall <- rbind(ytest,ytrain)

# replace activity numbers with tidy descriptions
yall <- yall[,1]
yall[yall==1]="walk"
yall[yall==2]="walkup"
yall[yall==3]="walkdown"
yall[yall==4]="sitting"
yall[yall==5]="standing"
yall[yall==6]="laying"
yalldf <- data.frame(yall)
colnames (yalldf) <- c("activity")

# combine activities with data set
dfall <- cbind(yalldf, xmeanstd)               

# 4. Appropriately labels the data set with descriptive variable names. 
colnames(dfall) <- c("activity","f_body_acceleration_mean()-X","f_body_acceleration_mean()-Y","f_body_acceleration_mean()-Z","f_body_acceleration_std()-X","f_body_acceleration_std()-Y","t_body_acceleration_jerk_magnitude_mean()","t_body_acceleration_jerk_magnitude_std()","t_body_acceleration_jerk_mean()-X","t_body_acceleration_jerk_mean()-Y","t_body_acceleration_jerk_mean()-Z","t_body_acceleration_jerk_std()-X","t_body_acceleration_jerk_std()-Y","t_body_acceleration_jerk_std()-Z","t_body_acceleration_mag_mean()","t_body_acceleration_mag_std()","t_body_acceleration_mean()-X","t_body_acceleration_mean()-Y","t_body_acceleration_mean()-Z","t_body_acceleration_std()-X","t_body_acceleration_std()-Y","t_body_acceleration_std()-Z","t_body_gyroscope_jerk_magnitude_mean()","t_body_gyroscope_jerk_magnitude_std()","t_body_gyroscope_jerk_mean()-X","t_body_gyroscope_jerk_mean()-Y","t_body_gyroscope_jerk_mean()-Z","t_body_gyroscope_jerk_std()-X","t_body_gyroscope_jerk_std()-Y","t_body_gyroscope_jerk_std()-Z","t_body_gyroscope_magniture_mean()","t_body_gyroscope_magagnitude_std()","t_body_gyroscope_mean()-X","t_body_gyroscope_mean()-Y","t_body_gyroscope_mean()-Z","t_body_gyroscope_std()-X","t_body_gyroscope_std()-Y","t_body_gyroscope_std()-Z","t-gravity_acceleration_magnitude_mean()","t-gravity_acceleration_magagnitude_std()","t-gravity_acceleration_mean()-X","t-gravity_acceleration_mean()-Y","t-gravity_acceleration_mean()-Z","t-gravity_acceleration_std()-X","t-gravity_acceleration_std()-Y","t-gravity_acceleration_std()-Z","f_body_acceleration_std()-Z","f_body_acceleration_jerk_mean()-X","f_body_acceleration_jerk_mean()-Y","f_body_acceleration_jerk_mean()-Z","f_body_acceleration_jerk_std()-X","f_body_acceleration_jerk_std()-Y","f_body_acceleration_jerk_std()-Z","f_body_gyroscope_mean()-X","f_body_gyroscope_mean()-Y","f_body_gyroscope_mean()-Z","f_body_gyroscope_std()-X","f_body_gyroscope_std()-Y","f_body_gyroscope_std()-Z","f_body_acceleration_magnitude_mean()","f_body_acceleration_magnitude_std()","f_body_acceleration_jerk_magnitude_mean()","f_body_acceleration_jerk_magnitude_std()","f_body_gyroscope_magnitude_mean()","f_body_gyroscope_magnitude_std()","f_body_gyroscope_jerk_magnitude_mean()","f_body_gyroscope_jerk_magnitude_std()")

#Perfect up to this point!

dfalltidy <- dfall
head(dfalltidy)

# 5. From step 4 data set, create a second, independent tidy data set with the average of each variable for each activity and each subject.

# read in subject data
subtestid <- read.table('test/subject_test.txt')
subtrainid <- read.table('train/subject_train.txt')
# combine test and train data
subjectsall <- rbind(subtestid,subtrainid)
colnames (subjectsall) <- c("subject_id")
dfalltidy2 <- cbind(subjectsall,dfalltidy)

library(reshape2)
reshaped <- melt(dfalltidy2, id.vars=c("subject_id", "activity"),measure.vars=c("f_body_acceleration_mean()-X","f_body_acceleration_mean()-Y","f_body_acceleration_mean()-Z","f_body_acceleration_std()-X","f_body_acceleration_std()-Y","t_body_acceleration_jerk_magnitude_mean()","t_body_acceleration_jerk_magnitude_std()","t_body_acceleration_jerk_mean()-X","t_body_acceleration_jerk_mean()-Y","t_body_acceleration_jerk_mean()-Z","t_body_acceleration_jerk_std()-X","t_body_acceleration_jerk_std()-Y","t_body_acceleration_jerk_std()-Z","t_body_acceleration_mag_mean()","t_body_acceleration_mag_std()","t_body_acceleration_mean()-X","t_body_acceleration_mean()-Y","t_body_acceleration_mean()-Z","t_body_acceleration_std()-X","t_body_acceleration_std()-Y","t_body_acceleration_std()-Z","t_body_gyroscope_jerk_magnitude_mean()","t_body_gyroscope_jerk_magnitude_std()","t_body_gyroscope_jerk_mean()-X","t_body_gyroscope_jerk_mean()-Y","t_body_gyroscope_jerk_mean()-Z","t_body_gyroscope_jerk_std()-X","t_body_gyroscope_jerk_std()-Y","t_body_gyroscope_jerk_std()-Z","t_body_gyroscope_magniture_mean()","t_body_gyroscope_magagnitude_std()","t_body_gyroscope_mean()-X","t_body_gyroscope_mean()-Y","t_body_gyroscope_mean()-Z","t_body_gyroscope_std()-X","t_body_gyroscope_std()-Y","t_body_gyroscope_std()-Z","t-gravity_acceleration_magnitude_mean()","t-gravity_acceleration_magagnitude_std()","t-gravity_acceleration_mean()-X","t-gravity_acceleration_mean()-Y","t-gravity_acceleration_mean()-Z","t-gravity_acceleration_std()-X","t-gravity_acceleration_std()-Y","t-gravity_acceleration_std()-Z","f_body_acceleration_std()-Z","f_body_acceleration_jerk_mean()-X","f_body_acceleration_jerk_mean()-Y","f_body_acceleration_jerk_mean()-Z","f_body_acceleration_jerk_std()-X","f_body_acceleration_jerk_std()-Y","f_body_acceleration_jerk_std()-Z","f_body_gyroscope_mean()-X","f_body_gyroscope_mean()-Y","f_body_gyroscope_mean()-Z","f_body_gyroscope_std()-X","f_body_gyroscope_std()-Y","f_body_gyroscope_std()-Z","f_body_acceleration_magnitude_mean()","f_body_acceleration_magnitude_std()","f_body_acceleration_jerk_magnitude_mean()","f_body_acceleration_jerk_magnitude_std()","f_body_gyroscope_magnitude_mean()","f_body_gyroscope_magnitude_std()","f_body_gyroscope_jerk_magnitude_mean()","f_body_gyroscope_jerk_magnitude_std()"))
reshapedMeans <- dcast(reshaped, subject_id + activity ~ variable, mean)
write.table(reshapedMeans, "CP2_Tidy_Data_Means2.txt", row.name=FALSE)
