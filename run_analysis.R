# Course Project - Getting and Cleaning Data
#Author: Sebastian Davila

## THE ASSESTMENT ##

#The purpose of this project is to demonstrate your ability to collect, 
#work with, and clean a data set. The goal is to prepare tidy data that can be used 
#for later analysis. You will be graded by your peers on a series of yes/no questions 
#related to the project. 

#You will be required to submit: 
# 1. a tidy data set as described below, 
# 2. a link to a Github repository with your script for performing the analysis, and 
# 3. a code book that describes the variables, the data, and any transformations 
#or work that you performed to clean up the data called CodeBook.md. 
#You should also include a README.md in the repo with your scripts. 
#This repo explains how all of the scripts work and how they are connected.  

##One of the most exciting areas in all of data science right now is wearable 
##computing - see for example this article . 
##Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced 
##algorithms to attract new users. The data linked to from the course website 
##represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
##A full description is available at the site where the data was obtained: 
  
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# Here are the data for the project: 
  
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Good luck!

##### PREPARING THE DATA #####

setwd("~/Documents/Documentos Sheby/Estudio/Specializations/Data Science/3. Getting and cleaning data/Course Project")

#GETTING THE DATA

library(httr)

  link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  file.name <- "UCI HAR Dataset.zip"
  if (!file.exists(file.name)){
      print("downloading")
      download.file(link, file.name, method = "curl")
  }
  
  #UNZIPPING AND CREATING FILES
  
  datafile <- "UCI HAR Dataset"
  folder <- "Results"
  if(!file.exists(datafile)){
      print("unzipping file")
      unzip(filename, list = F, overwrite = T)
  }
  if(!file.exists(folder)){
      print("creating Results folder")
      dir.create(folder)
  }
  
  ## CONVERTING FILES INTO DATAFRAME
  
  tables <- function (filename, cols = NULL){
    print(paste("Getting table:", filename))
    f <- paste(folder,filename, sep = "/")
    data <- data.frame()
    if(is.null(cols)){
            data <- read.table(f, sep = "", stringsAsFactors = F)
    } else {
            data <- read.table(f, sep = "", stringsAsFactors = F, col.names = cols)
    }
    data
  }
  
  #RUNNING AND CHECKING "TABLES" 
  
  features <- tables("features.txt")
  
  #READING DATA AND BUILDING THE DATABASE
  
  gettingdata <- function (type, features){
        print(paste("Getting data", type))
        subject_data <- tables(paste(type,"/","subject_",type,".txt",sep=""),"id")
        y_data <- tables(paste(type,"/","y_",type,".txt",sep=""),"activity")    
        x_data <- tables(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
        return (cbind(subject_data,y_data,x_data))
  }
  
  #RUNNING AND CHECKING "GETTINGDATA"
  
  test <- gettingdata("test", features)
  train <- gettingdata("train", features)
  
  # SAVING THE RESULTS
  
  saveresults <- function (data,name){
        print(paste("saving results", name))
        file <- paste(folder, "/", name,".csv", sep="")
        write.csv(data,file) 
  }
  
  saveresults2 <- function (data, name){
    print(paste("saving results", name))
    file <- paste(folder, "/", name, ".txt", sep = "")
    write.table(data, file, row.names = F)
  }
  
##### RESOLVING THE ASSESTMENT

# 1. Merges the training and the test sets to create one data set.
  
library(dplyr)
data <- rbind(train, test)
data <- arrange(data, id)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(mean_and_std,"mean_and_std")

# 3. Uses descriptive activity names to name the activities in the data set

activity_labels <- tables("activity_labels.txt")

# 4. Appropriately labels the data set with descriptive variable names. 

data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults2(tidy_dataset,"tidy_dataset")
