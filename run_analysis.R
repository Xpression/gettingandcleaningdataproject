## Paths and filenames
zippedData <- "Dataset.zip"
unzippedDirectory <- "."
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
outputDirectory <- "output"
outputFile <- "tidyData.txt"

######################################################
################## Helper functions ##################

## This function extracts the names of all features containing "mean" or "std"
## from the original features tables. These features will be included in output.
getMeanAndStddevFeatures <- function() {
    features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
    features <- data.table(FeatureNumber=features$V1, Description=features$V2)
    meanFeatures <- features[like(Description, "mean")]
    stdFeatures <- features[like(Description, "std")]
    
    interestingFeatures <- rbind(meanFeatures, stdFeatures)
}

## Loads the activities from the original activity label files.
getActivites <- function() {
    activities <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
    activities <- data.table(ActivityNumber=activities$V1, Description=activities$V2)
}

## This script relies on the dplyr package. This function is used to install 
## a package if it is missing.
installPackageIfMissing <- function(x) {
    
    if (!require(x, character.only = TRUE)) {
        ## Install package if missing
        message(informationString)
        informationString = paste(c("Installing dependent package ", x))
        install.packages(x, dep=TRUE, repos="http://cran.rstudio.com/")
        
        ## Verify installation of package
        if (!require(x, character.only = TRUE)) {
            errorString <- paste(c("ERROR: Dependent package ", x, "could not be found"))
            stop(errorString)
        }
    }
}

## Extracts the features contained in the 'featureTable' argument from the file 
## given by the 'fileName' argument.
extractDesiredFeatures <- function(fileName, featureTable) {
    rawFeatures <- read.table(fileName, stringsAsFactors=FALSE)
    filteredFeatures  <- rawFeatures[, featureTable$FeatureNumber]
}

## Maps the activities contained in the 'activitiesFile' to the human friendly 
## names contained in the 'activityTable'.
extractActivitiesByName <- function(activitiesFile, activityTable) {
    rawActivities <- read.table(activitiesFile)
    
    namedActivities <- vector()
    for (activity in rawActivities$V1) {
        namedActivities <- c(namedActivities, activityTable$Description[activity])
    }
    
    namedActivities
}

## Build a part of the "raw" tidy dataset. The 'isTest' argument determines 
## whether to use the test or training part of the dataset.
buildTidyDatasetPart <- function(isTest, activityTable, featureTable) {
    
    if (isTest) {
        featuresFile <- "UCI HAR Dataset/test/X_test.txt"
        subjectFile <- "UCI HAR Dataset/test/subject_test.txt"
        activitiesFile <- "UCI HAR Dataset/test/Y_test.txt"
    } else {
        featuresFile <- "UCI HAR Dataset/train/X_train.txt"
        subjectFile <- "UCI HAR Dataset/train/subject_train.txt"
        activitiesFile <- "UCI HAR Dataset/train/Y_train.txt"
    }
    
    ## Start by selecting the desired columns from the features file
    filteredFeatures <- extractDesiredFeatures(featuresFile, featureTable)
    
    ## Extract the subjects
    subjects = read.table(subjectFile)
    
    ## Extract the activities and map them to their names
    activitiesByName <- extractActivitiesByName(activitiesFile, activityTable)
    
    ## Now combine all the columns into one table
    tidyDatasetPart = data.table(Subject=subjects$V1)
    for (i in 1:length(featureTable$Description)) {
        description <- featureTable$Description[i]
        observations <- filteredFeatures[, i]
        tidyDatasetPart[[description]] <- observations
    }
    tidyDatasetPart$Activity <- activitiesByName

    tidyDatasetPart
}

######################################################
################## Tidy Data Script ##################

## Install dependent packages if not allready present
requiredPackages <- c("dplyr")

## Load libraries
library(data.table)
library(dplyr)

## The project text states that the script should be able to run if the Samsung
## data is present in the working directory. If it is not, help out the user 
## by downloading it.
if (!file.exists(zippedData)) {
    download.file(dataUrl, destfile=zippedData, method="curl")
    downloadedTime <- date()
}

## Unzip data, overwrite files/folder if allready present
unzip(zippedData, overwrite=TRUE, exdir=unzippedDirectory)

## If not allready present, create an output directory
if (!file.exists(outputDirectory)) {
    dir.create(outputDirectory)
}

## Build a table of interesting features, i.e. those that include 'mean' or 'std'
interestingFeatures <- getMeanAndStddevFeatures() 

## Build a table of the activities
activities <- getActivites()

## Build "raw" tidy versions of the training and test datasets
tidyRawTraining <- buildTidyDatasetPart(FALSE, activities, interestingFeatures) 
tidyRawTest <- buildTidyDatasetPart(TRUE, activities, interestingFeatures)

## Combine the parts by "stacking them"
tidyRawDataset <- rbind(tidyRawTraining, tidyRawTest)

## Clean up a bit by removing unecessary data (reduces memory consumption)
rm(tidyRawTraining)
rm(tidyRawTest)
rm(interestingFeatures)

## Create the tidy data as specified by the project text. The "raw tidy data" 
## is grouped by subject and activity, and each column is summarized by its mean.
tidyDataOutput <- tidyRawDataset %>%
         group_by(Subject, Activity) %>%
         summarise_each(funs(mean))

## Write the output file containing the summarized tidy dataset
write.table(tidyDataOutput, paste(outputDirectory, outputFile, sep="/"))