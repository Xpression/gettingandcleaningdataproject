## Paths and filenames
zippedData <- "Dataset.zip"
unzippedDirectory <- "."
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
outputDirectory <- "output"

## Helper functions
getMeanAndStddevFeatures <- function() {
    features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
    features <- data.table(FeatureNumber=features$V1, Description=features$V2)
    meanFeatures <- features[like(Description, "-mean()")]
    stdFeatures <- features[like(Description, "-std()")]
    
    interestingFeatures <- rbind(meanFeatures, stdFeatures)
}

getActivites <- function() {
    activities <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
    activities <- data.table(ActivityNumber=activities$V1, Description=activities$V2)
}

#installPackageIfMissing <- function(x) {
#    
#    if (!require(x, character.only = TRUE)) {
#        ## Install package if missing
#        message(informationString)
#        informationString = paste(c("Installing dependent package ", x))
#        install.packages(x, dep=TRUE, repos="http://cran.rstudio.com/")
#        
#        ## Verify installation of package
#        if (!require(x, character.only = TRUE)) {
#            errorString <- paste(c("ERROR: Dependent package ", x, "could not be found"))
#            stop(errorString)
#        }
#    }
#}

extractDesiredFeatures <- function(fileName, featureTable) {
    rawFeatures <- read.table(fileName, stringsAsFactors=FALSE)
    filteredFeatures  <- rawFeatures[, featureTable$FeatureNumber]
}

extractActivitiesByName <- function(activitiesFile, activityTable) {
    rawActivities <- read.table(activitiesFile)
    
    namedActivities <- vector()
    for (activity in rawActivities$V1) {
        namedActivities <- c(namedActivities, activityTable$Description[activity])
    }
    
    namedActivities
}

buildTidyDatasetPart <- function(isTest, activityTable, featureTable) {
    
    if (isTest) {
        featuresFile <- "UCI HAR Dataset/test/X_test.txt"
        subjectFile <- "UCI HAR Dataset/test/subject_test.txt"
        activitiesFile <- "UCI HAR Dataset/test/Y_test.txt"
        originLabel <- "Test"
    } else {
        featuresFile <- "UCI HAR Dataset/train/X_train.txt"
        subjectFile <- "UCI HAR Dataset/train/subject_train.txt"
        activitiesFile <- "UCI HAR Dataset/train/Y_train.txt"
        originLabel <- "Training"
    }
    
    ## Start by selecting the desired columns from the features file
    filteredFeatures <- extractDesiredFeatures(featuresFile, featureTable)
    
    ## Extract the subjects
    subjects = read.table(subjectFile)
    
    ## Extract the activities and map them to their names
    activitiesByName <- extractActivitiesByName(activitiesFile, activityTable)
    
    ## Create a column representing the origin of the data
    originColumn <- rep(originLabel, length(subjects$V1))
    
    ## Now combine all the columns into one table
    tidyDatasetPart = data.table(Subject=subjects$V1, Origin=originColumn)
    for (i in 1:length(featureTable$Description)) {
        description <- featureTable$Description[i]
        observations <- filteredFeatures[, i]
        tidyDatasetPart[[description]] <- observations
    }
    tidyDatasetPart$Activity <- activitiesByName
    #tidyDatasetPart[["Origin"]] <- originColumn
    
    tidyDatasetPart
}

## Install dependent packages if not allready present
#requiredPackages <- c()


## The project text states that the script should be able to run if the Samsung
## data is present in the working directory. If it is not, we help out the user 
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

## Build a table of interesting features, i.e. those that include '-mean()'
interestingFeatures <- getMeanAndStddevFeatures() 

## Build a table of the activities
activities <- getActivites()

## Build tidy versions of the training and test datasets
tidyTraining <- buildTidyDatasetPart(FALSE, activities, interestingFeatures) 
tidyTest <- buildTidyDatasetPart(TRUE, activities, interestingFeatures)

## Combine the parts by "stacking them"
tidyDataset <- rbind(tidyTraining, tidyTest)