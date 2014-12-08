## Paths and filenames
zippedData <- "Dataset.zip"
unzippedDirectory <- "."
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
outputDirectory <- "output"

## Helper functions
getMeanFeatures <- function() {
    features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
    features <- data.table(FeatureNumber=features$V1, Description=features$V2)
    meanFeatures <- features[like(Description, "-mean()")]
}

getActivites <- function() {
    activities <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
    activities <- data.table(ActivityNumber=activities$V1, Description=activities$V2)
}

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

## Install dependent packages if not allready present
requiredPackages <- c()


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
meanFeatures <- getMeanFeatures() 
print(meanFeatures)

## Build a table of the activities
activities <- getActivites()
print(activities)