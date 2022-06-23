# Libraries
library(xlsx)
library(lubridate)

# Global variables
gbAccountNumber <<- c()
gbAvgAmount <<- c()
gbInBound <<- c()
avgMonthlyCreditDebitTransaction <- data.frame(gbAccountNumber, gbAvgAmount, gbInBound)
yes <<- "Y"
no <<- "N"
inBoundCredit <<- "T"
inBoundDebit <<- "F"
# Daily = "D", Monthly = "M", Weekly = "W", Yearly = "Y"
monthlyFrequency <<- "M"

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
    gbDisplayAllTransaction <<- configData$DisplayAllTransaction
    gbExcludeCreditActivity <<- configData$ExcludeCreditActivity
    gbMinNumberSD <<- configData$MinNumberSD
    gbMaxNumberSD <<- configData$MaxNumberSD
    gbMinCurrentMonthAmt <<- configData$MinCurrentMonthAmt
    gbMinPercentageIncrease <<- configData$MinPercentageIncrease
    gbIncludeMonthsWithNoActivity <<- configData$IncludeMonthsWithNoActivity
    gbLookBackPeriod <<- configData$LookbackPeriod
    gbMinOpenDays <<- configData$MinOpenDays

    # Non Tunable
    gbFrequencyPeriod <<- monthlyFrequency
}

addAccountTransactionDetails <- function(accountNumber, avgAmount, inBound) {
    gbAccountNumber <- append(gbAccountNumber, accountNumber)
    gbAvgAmount <- append(gbAvgAmount, avgAmount)
    gbInBound <- append(gbInBound, inBound)
}

getCeilingDateBeforeLookbackPeriod <- function(latestDate) {
    #Get last date of previous month
    lastDateOfPreviousMonth <- ceiling_date(date - months(1), "months") - 1

    # Get first date of the month after lookback period is subtracted
    # 13, because ceiling_date returns the first date of the next month
    firstDateOfMonthWithLookbackPeriod <- ceiling_date(lastDateOfPreviousMonth - months(13), "month")
    return (firstDateOfMonthWithLookbackPeriod)
}

calculateAvgMonthlyTransaction <- function(data) {
    accountNumber <- append(gbAccountNumber, data$AccountNumber)
    if (gbIncludeMonthsWithNoActivity == "Y") {
        # Calculating Avg Monthly Credit Transaction Amount
        avgAmount <- (sum(subset(data$Amount, data$InBound == "T" & data$Date >= (data$Date - months(gbLookBackPeriod)))) / gbLookBackPeriod)
        inBound <- "T"
        addAccountTransactionDetails(accountNumber, avgAmount, inBound)
    } else {

    }
}

avgMonthlyCreditTransactionAmount <- function(data) {
    # Extracting distinct account numbers using factor object
    accountNumbers <- factor(data$AccountNumber)
    for(currentAccountNumber in levels(accountNumbers)) {
        recordData <- subset(data, data$AccountNumber == currentAccountNumber & format(data$Date, "%Y-%m") != format(Sys.Date(), "%Y-%m"))


    }
}