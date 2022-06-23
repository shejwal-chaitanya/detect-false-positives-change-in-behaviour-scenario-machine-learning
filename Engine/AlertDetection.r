# Libraries
library(xlsx)

# Data Extraction
# Configurable parameters fill
# Logic run

# Data Extraction
readData <- function() {
    configData <<- read.xlsx("../Data/CIB_Data.xlsx", sheetIndex <- 3)
    transactionData <<- read.xlsx("../Data/CIB_Data.xlsx", sheetIndex <- 1)
}

# Configurations
populateConfigurationParameters <- function() {
    # Tunable
    displayAllTransaction <<- configData$DisplayAllTransaction
    excludeCreditActivity <<- configData$ExcludeCreditActivity
    minNumberSD <<- configData$MinNumberSD
    maxNumberSD <<- configData$MaxNumberSD
    minCurrentMonthAmt <<- configData$MinCurrentMonthAmt
    minPercentageIncrease <<- configData$MinPercentageIncrease
    includeMonthsWithNoActivity <<- configData$IncludeMonthsWithNoActivity
    lookBackPeriod <<- configData$LookbackPeriod
    minOpenDays <<- configData$MinOpenDays

    # Non Tunable
    # Daily = "D", Monthly = "M", Weekly = "W", Yearly = "Y"
    frequencyPeriod <<- "M"
}