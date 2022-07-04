# File Names
dataDirectory <- "../Data/"
configurations <- paste(dataDirectory, "Configurations.csv", sep="")
transactions <- paste(dataDirectory, "Transactions.csv", sep="")
accountDetails <- paste(dataDirectory, "AccountDetails.csv", sep="")

# File Types
CSV <- "csv"

# Reads csv data
# Input Params : fileName
readCSV <- function (fileName) {
    return (
        read.csv(fileName)
    )
}


# Read data from file
# Input Params :
#   fileName: specifies the name of the file
#   fileType: specifies the type of the file
readData <- function(fileName, fileType) {
    tryCatch (
        {
            return (
                switch (
                    fileType,
                    "csv" = readCSV(fileName)
                )
            )
        },
        warning = function(warn) {
            print(paste("Warning: ", warn))
        },
        error = function(err) {
            print(paste("Error: ", err))
        }
    )
}