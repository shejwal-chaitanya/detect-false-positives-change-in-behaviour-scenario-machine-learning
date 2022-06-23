# Libraries
library(lubridate)
library(dplyr)

# Global variables
gbAccountNumber <<- c()
gbAvgAmount <<- c()
gbInBound <<- c()
avgMonthlyCreditDebitTransaction <<- data.frame(gbAccountNumber, gbAvgAmount, gbInBound)
yes <<- "Y"
no <<- "N"
inBoundCreditType <<- c("T", "t", "TRUE", "True", "true")
inBoundCredit <<- "T"
inBoundDebitType <<- c("F", "f", "FALSE", "False", "false")
inBoundDebit <<- "F"
# Daily = "D", Monthly = "M", Weekly = "W", Yearly = "Y"
monthlyFrequency <<- "M"

# Data Extraction
# Configurable parameters fill
# Logic run

# Data Extraction
readData <- function() {
    configData <<- read.csv("../Data/Configurations.csv")
    transactionData <<- read.csv("../Data/Transactions.csv")
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
    if (!accountNumber %in% gbAccountNumber) {
        gbAccountNumber <<- append(gbAccountNumber, accountNumber)
        gbAvgAmount <<- append(gbAvgAmount, avgAmount)
        gbInBound <<- append(gbInBound, inBound)   
    }
}

getLastDateOfPreviousMonth <- function(latestDate) {
    #Get last date of previous month
    lastDateOfPreviousMonth <- ceiling_date(as.Date(latestDate) - months(1), "months") - 1
    return(lastDateOfPreviousMonth)
}

getCeilingDateBeforeLookbackPeriod <- function(latestDate) {
    # Returns the first date of the month after lookback period is subtracted

    #Get last date of previous month
    lastDateOfPreviousMonth <- getLastDateOfPreviousMonth(latestDate)

    # Get first date of the month after lookback period is subtracted
    # 13, because ceiling_date returns the first date of the next month
    firstDateOfMonthWithLookbackPeriod <- as.Date(cut(lastDateOfPreviousMonth - years(1), "month"))
    return (firstDateOfMonthWithLookbackPeriod)
}

getTotalAmount <- function(data) {
    return (sum(subset(data$Amount, (getLastDateOfPreviousMonth(max(as.Date(data$Date))) >= getCeilingDateBeforeLookbackPeriod(max(as.Date(data$Date)))))))
}

getNumberOfMonthsWithNoActivity <- function(data) {
    summarisedAccountData <- subset(data$Date, getLastDateOfPreviousMonth(data$Date) >= getCeilingDateBeforeLookbackPeriod(data$Date)) %>% group_by(format(data$Date), "%m") %>% summarise(totalAmount = sum(data$Amount))

    return (nrow(summarisedAccountData))
}

calculateAvgMonthlyTransaction <- function(data, inBound) {
    if (gbIncludeMonthsWithNoActivity == yes) {
        # Calculating Avg Monthly Credit Transaction Amount
        avgAmount <- (getTotalAmount(data) / gbLookBackPeriod)
        addAccountTransactionDetails(unique(data$AccountNumber), avgAmount, inBound)
    } else {
        avgAmount <- (getTotalAmount(data) / getNumberOfMonthsWithNoActivity(data)) 
    }
}

avgMonthlyCreditTransactionAmount <- function(data) {
    # Extracting distinct account numbers using factor object
    accountNumbers <- factor(data$AccountNumber)
    for(currentAccountNumber in levels(accountNumbers)) {
        recordData <- subset(data, data$AccountNumber == currentAccountNumber)
        recordData <- subset(recordData, format(as.Date(recordData$Date), "%Y-%m") != format(as.Date(Sys.Date()), "%Y-%m"))
        if (nrow(recordData) >=1 ) {
            creditTransactionData <- subset(recordData, recordData$InBound %in% inBoundCreditType)
            if (nrow(creditTransactionData) >= 1) {
                calculateAvgMonthlyTransaction(creditTransactionData, inBoundCredit)
            }

            debitTransactionData <- subset(recordData, recordData$InBound %in% inBoundDebitType)
            if (nrow(debitTransactionData) >= 1) {
                calculateAvgMonthlyTransaction(debitTransactionData, inBoundDebit)
            }
        }
    }
}

showOutput <- function() {
    new_df <- data.frame(gbAccountNumber, gbAvgAmount, gbInBound)
    print(new_df)
}

readData()
populateConfigurationParameters()
avgMonthlyCreditTransactionAmount(transactionData)
showOutput()