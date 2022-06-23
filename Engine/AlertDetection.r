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
    accountDetailsData <<- read.csv("../Data/AccountDetails.csv")
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
    # Condition - To ensure no account number is repeated for same inbound type
    if (nrow(subset(avgMonthlyCreditDebitTransaction, gbAccountNumber == accountNumber & gbInBound == inBound)) == 0) {
        gbAccountNumber <<- append(gbAccountNumber, accountNumber)
        gbAvgAmount <<- append(gbAvgAmount, avgAmount)
        gbInBound <<- append(gbInBound, inBound)   
    }
    avgMonthlyCreditDebitTransaction <<- rbind(avgMonthlyCreditDebitTransaction, data.frame(gbAccountNumber, gbAvgAmount, gbInBound))
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

    summarisedAccountData <- subset(data, getLastDateOfPreviousMonth(max(as.Date(data$Date))) >= getCeilingDateBeforeLookbackPeriod(max(as.Date(data$Date)))) %>% group_by(format(as.Date(data$Date)), "%m") %>% summarise(totalAmount = sum(data$Amount))

    return (nrow(summarisedAccountData))
}

calculateAvgMonthlyTransaction <- function(data, inBound) {
    if (gbIncludeMonthsWithNoActivity == yes) {
        # Calculating Avg Monthly Credit Transaction Amount
        avgAmount <- (getTotalAmount(data) / gbLookBackPeriod)
    } else {
        avgAmount <- (getTotalAmount(data) / getNumberOfMonthsWithNoActivity(data))
    }
    addAccountTransactionDetails(unique(data$AccountNumber), avgAmount, inBound)
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
    print(avgMonthlyCreditDebitTransaction)
}

calculateAggregateAmount <- function(data, accountNumber, inBoundType) {
    return (sum(subset(data$Amount, data$AccountNumber == accountNumber & data$InBound == inBoundType)))
}

calculateStandardDeviation <- function(data, accountNumber, inBoundType) {
    return (sd(subset(data$Amount, data$AccountNumber == accountNumber & data$InBound == inBoundType)))
}

getAvgAmount <- function(data, accountNumber, inBoundType) {
    return (subset(avgMonthlyCreditDebitTransaction$gbAvgAmount, avgMonthlyCreditDebitTransaction$gbAccountNumber == accountNumber & avgMonthlyCreditDebitTransaction$gbInBound == inBoundType))
}

getAggregateCurrentMonthsTransaction <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound == inBoundType)
    currentMonthsFirstDate <- ceiling_date(Sys.Date() - months(1), "month")
    data <- subset(data, as.Date(Sys.Date()) >= currentMonthsFirstDate)
    return (sum(data$Amount))
}

getAccountOpenDate <- function(data, accountNumber) {
    data <- subset(data, data$AccountNumber == accountNumber)
    return (as.Date(data$AccountOpenDate))
}

alertGeneration <- function() {
    accounts <- factor(avgMonthlyCreditDebitTransaction$gbAccountNumber)
    for (accountNumber in levels(accounts)) {
        alertsGenerated <- c()
        # Calculation of necessary amounts
        aggCurrentMonthCreditTransactions <- getAggregateCurrentMonthsTransaction(transactionData, accountNumber, inBoundCredit)
        aggCurrentMonthDebitTransactions <- getAggregateCurrentMonthsTransaction(transactionData, accountNumber, inBoundDebit)
        accountOpenDate <- getAccountOpenDate(transactionData, accountNumber)
        avgCreditAmount <- getAvgAmount(avgMonthlyCreditDebitTransaction, accountNumber, inBoundCredit)
        avgDebitAmount <- getAvgAmount(avgMonthlyCreditDebitTransaction, accountNumber, inBoundDebit)
        sdCreditAmount <- calculateStandardDeviation(transactionData, accountNumber, inBoundCredit)
        sdDebitAmount <- calculateStandardDeviation(transactionData, accountNumber, inBoundDebit)

        if (accountOpenDate <= (Sys.Date() - gbMinOpenDays)) {
            alertsGenerated <- append(alertsGenerated, "Minimum Open Days Alert")
        }

        if ((aggCurrentMonthCreditTransactions - avgCreditAmount) >= (sdCreditAmount * gbMinNumberSD)) {
            alertsGenerated <- append(alertsGenerated, "Agg Amount - Avg Amount >= SD x Min(SD)")
        }

        if ((aggCurrentMonthCreditTransactions - avgCreditAmount) <= (sdCreditAmount * gbMaxNumberSD)) {
            alertsGenerated <- append(alertsGenerated, "Agg Amount - Avg Amount <= SD x Max(SD)")
        }

        if (aggCurrentMonthCreditTransactions >= gbMinCurrentMonthAmt) {
            alertsGenerated <- append(alertsGenerated, "Avg amount exceeds minimum current month amount")
        }

        if ((aggCurrentMonthCreditTransactions - avgCreditAmount) >= (avgCreditAmount * gbMinPercentageIncrease)) {
            alertsGenerated <- append(alertsGenerated, "Last Condition")
        }
    }

    print(alertsGenerated)
}

readData()
populateConfigurationParameters()
avgMonthlyCreditTransactionAmount(transactionData)
showOutput()
alertGeneration()