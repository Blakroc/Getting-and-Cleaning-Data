#setwd()


##########################
runtheProject <- function(){
      
      # Downloading input data once for all purposes
      getInputData()
      
      if(!file.exists("UCI HAR Dataset"))
      {
            print("Error file UCI HAR Dataset is expected but missing")
      }
      
      activities <- getAttribute("activity")
      features <- getAttribute("features")  

      # Project Step1: Merges the training and the test sets to create one data set
      mergedData <- getMergedTrainingNTestingSet()
      # Project Step2: Extracts only the measurements on the mean and standard deviation for each measurement 
      filteredData <- getMeanNStdMeasurements(mergedData)
      # Project Step3: Uses descriptive activity names to name the activities in the data set
      mergedData <- setActivitiesNames(mergedData,activities)
      # Project Step4: Appropriately labels the data set with descriptive variable names
      # Do note these titles are quite descriptive. But the logic of constructing them is in the code book 
      # However, we can replacement fairly quickly with the'features'
      updatedData <- setDescriptiveMeasurementName(mergedData)
      # Proect Step5: Creates a second, independent tidy data set with 
      # the average of each variable for each activity and each subject. 
      write.csv(updatedData, "./Activity_Phone_Measurements.csv")
}

##########################
# Zip data is downloaded and unzipped in the current folder
getInputData <- function(){
      url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      name <- "data.zip"
      download.file(url, dest = name)
      unzip(name)    
}


# Merge Training and Testing Data sets
getMergedTrainingNTestingSet <- function(){
                  
            trainData <- getCombinedData("train")
            testData <- getCombinedData("test")
            
            # 1. Merge the train and testing Data set
            mergedData <- rbind(trainData, testData)
            
        mergedData
}

# Modelling a data frame that contains 'mean' and 'standard deviation' for each measurement
getMeanNStdMeasurements <- function(dataSet){
            # Check for 'mean' measurements
            mean_fields <- grepl('mean',names(dataSet))
            # Check for 'Standard Deviation' measurements
            std_fields <- grepl('std',names(dataSet))
            
            requiredFields <- mean_fields | std_fields
            
            result <- dataSet[which(requiredFields)]
            
            result # containing only mean and standard deviation fields
      
}

# Modelling a data frame that contains 'avergage' for each  activity and each subject
getAverage <- function(dataSet){
      # Check for 'mean' measurements
      mean_fields <- grepl('mean',names(dataSet))
      mean_data <- dataSet[which(mean_fields)]      
      
      result # containing only mean and standard deviation fields
      
}


# Giving descriptive activity names to the activities
setActivitiesNames <- function(dataSet, activity){
      
      result <- join(dataSet, activity, by = "label")
      
      result
      
}

# Function to give more descriptive measurement titles
setDescriptiveMeasurementName <- function(dataSet){
      
      descriptiveNames <- sub("t", "Time_", names(dataSet))
      descriptiveNames <- sub("f", "Frequency_", descriptiveNames)
      descriptiveNames <- gsub("()", "", descriptiveNames)  
      descriptiveNames <- gsub("BodyBody", "Body", descriptiveNames)  
      descriptiveNames <- gsub("Acc", "_Acceleration", descriptiveNames)
      descriptiveNames <- gsub("Gyro", "_Gyroscopic", descriptiveNames)
      descriptiveNames <- gsub("Jerk", "_Jerk", descriptiveNames)
      descriptiveNames <- gsub("Mag", "_Mag", descriptiveNames)
      descriptiveNames <- gsub("-", "_", descriptiveNames)
      descriptiveNames <- gsub("mean", "Mean", descriptiveNames)  
      descriptiveNames <- gsub("std", "StandardDeviation", descriptiveNames)  
      descriptiveNames <- gsub("mad", "MedianAbsoluteDeviation", descriptiveNames)  
      descriptiveNames <- gsub("max", "Max", descriptiveNames)  
      descriptiveNames <- gsub("min", "Min", descriptiveNames)  
      descriptiveNames <- gsub("sma", "SignalMagnitudeArea", descriptiveNames)  
      descriptiveNames <- gsub("energy", "Energy", descriptiveNames)  
      descriptiveNames <- gsub("iqr", "InterquartileRange", descriptiveNames)  
      descriptiveNames <- gsub("entropy", "SignalEntropy", descriptiveNames)  
      descriptiveNames <- gsub("arCoeff", "Autorregresion", descriptiveNames)  
      descriptiveNames <- gsub("correlation", "Correlation", descriptiveNames)  
      descriptiveNames <- gsub("maxInds", "MaxIndex", descriptiveNames)  
      descriptiveNames <- gsub("skewness", "Skewness", descriptiveNames)  
      descriptiveNames <- gsub("kurtosis", "Kurtosis", descriptiveNames)  
      descriptiveNames <- gsub("bandsEnergy", "EnergyBands", descriptiveNames)  
      descriptiveNames <- gsub("angle", "v", descriptiveNames)  
      descriptiveNames <- gsub("-X", "X-axis", descriptiveNames)  
      descriptiveNames <- gsub("-Y", "Y-axis", descriptiveNames)  
      descriptiveNames <- gsub("-Z", "Z-axis", descriptiveNames)  
      
      names(dataSet) <- descriptiveNames
      
      dataSet
      
}

# Generic function to read a file and format to data frame with col names
getAttribute <-function(attribute){
      if( attribute == "activity"){
            file <-"./UCI HAR Dataset/activity_labels.txt"
      }else if ( attribute == "features"){
            file <- "./UCI HAR Dataset/features.txt"
      }
      attributes <- read.table(file)
      #attributes <- sapply(attributes, getSecondElementOnly )
      colnames(attributes) <- c( "label", attribute)
      
      attributes
}

# Function to merge the X file, the Y file and the 
getCombinedData<- function (fileName){
      setwd("./UCI HAR Dataset/") #Setting home directory to be the "UCI HAR Dataset file
      
      if(!file.exists(fileName)){
            print(paste("Error file named",fileName," doesn't exsist.",sep=""))
      }else{
            setwd(paste("./",fileName, "/", sep=""))
            
            x <- read.table(paste("./X_", fileName,".txt",sep=""))
            colnames(x) <- features$features
            
            y <- read.table(paste("./y_", fileName,".txt",sep=""))
            colnames(y) <- "label"
            
            subject<- read.table(paste("./subject_", fileName,".txt",sep=""))
            colnames(subject) <- "subject"
            
            combinedData <- cbind(subject,x,y)
            
            setwd("..")
      }
      setwd("..") # Resetting the current directory to the original data directory
      
      combinedData
}
