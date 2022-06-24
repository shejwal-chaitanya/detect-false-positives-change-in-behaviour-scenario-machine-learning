# Libraries
library(lubridate)
library(dplyr)

# Global variables
alertsGenerated <<- c()
gbAccountNumber <<- c()
gbAvgAmount <<- c()
gbInBound <<- c()
avgMonthlyCreditDebitTransaction <<- data.frame(gbAccountNumber, gbAvgAmount, gbInBound)
yes <<- "Y"
no <<- "N"
inBoundCreditType <<- c("T", "t", "TRUE", "True", "true", TRUE)
inBoundCredit <<- "T"
inBoundDebitType <<- c("F", "f", "FALSE", "False", "false", FALSE)
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
    if (nrow(subset(avgMonthlyCreditDebitTransaction, avgMonthlyCreditDebitTransaction$gbAccountNumber == accountNumber & avgMonthlyCreditDebitTransaction$gbInBound == inBound)) == 0) {
        gbAccountNumber <<- accountNumber
        gbAvgAmount <<- avgAmount
        gbInBound <<- inBound
        avgMonthlyCreditDebitTransaction <<- rbind(avgMonthlyCreditDebitTransaction, data.frame(gbAccountNumber, gbAvgAmount, gbInBound))
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
    print(alertsGenerated)
    alertsGenerated <<- c()
}

calculateStandardDeviation <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound %in% inBoundType)
    if (nrow(data >= 1)) {
        return (sd(data$Amount))
    } else {
        return (0)
    }
}

getAvgAmount <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$gbAccountNumber == accountNumber & data$gbInBound == inBoundType)
    
    if (nrow(data) >= 1) {
        return (data$gbAvgAmount)
    } else {
        return(0)
    }
}

getAggregateCurrentMonthsTransaction <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound %in% inBoundType)
    currentMonthsFirstDate <- ceiling_date(Sys.Date() - months(1), "month")
    if (nrow(data >= 1)) {
        data <- subset(data, as.Date(Sys.Date()) >= currentMonthsFirstDate)
        return (sum(data$Amount))
    } else {
        return (0)
    }       
}

getAccountOpenDate <- function(data, accountNumber) {
    data <- subset(data, data$AccountNumber == accountNumber)
    return (as.Date(data$AccountOpenDate))
}

alertGenerator <- function(accountNumber, inBoundType, inBound) {
    # Calculation of necessary amounts
    # Get Aggregate For Current Month Transactions For Given InBound Type
    aggCurrentMonthTransactions <- getAggregateCurrentMonthsTransaction(transactionData, accountNumber, inBoundType)

    # Get Account Open Date
    accountOpenDate <- getAccountOpenDate(accountDetailsData, accountNumber)

    # Get Average Credit Amount
    avgAmount <- getAvgAmount(avgMonthlyCreditDebitTransaction, accountNumber, inBound)

    # Get SD of Credit Amount
    sdAmount <- calculateStandardDeviation(transactionData, accountNumber, inBoundType)

    # Get difference between aggregate and average amount
    diffAggAvg <- aggCurrentMonthTransactions - avgAmount
    
    # Multiplication of SD and Min Number SD
    mulStdDevMinSd <- sdAmount * gbMinNumberSD
    
    # Multiplication of SD and Max Number SD
    mulStdDevMaxSd <- sdAmount * gbMaxNumberSD

    # Mulitiplied Average Amount with Minimum Percentage Increase
    mulAvgAmountMinPercentageIncrease <- avgAmount * (gbMinPercentageIncrease/100)


    if (accountOpenDate <= (Sys.Date() - gbMinOpenDays)) {
        alertsGenerated <<- append(alertsGenerated, "Minimum Open Days Alert")
    }
        
    if (diffAggAvg >= mulStdDevMinSd) {
        alertsGenerated <<- append(alertsGenerated, "Agg Amount - Avg Amount >= SD x Min(SD)")
    }

    if (diffAggAvg <= mulStdDevMaxSd) {
        alertsGenerated <<- append(alertsGenerated, "Agg Amount - Avg Amount <= SD x Max(SD)")
    }

    if (aggCurrentMonthTransactions >= gbMinCurrentMonthAmt) {
        alertsGenerated <<- append(alertsGenerated, "Avg amount exceeds minimum current month amount")
    }

    if (diffAggAvg >= mulAvgAmountMinPercentageIncrease) {
        alertsGenerated <<- append(alertsGenerated, "Last Condition")
    }
    cat("Alerts Generated For AccountNumber - ", accountNumber, "\n")
    showOutput()
}

alertGeneration <- function() {
    accounts <- factor(avgMonthlyCreditDebitTransaction$gbAccountNumber)
    for (accountNumber in levels(accounts)) {
        accountNumber <- as.numeric(accountNumber)
        
        # Generate Alerts for Credit Transactions
        if(nrow(subset(transactionData, transactionData$AccountNumber == accountNumber & transactionData$InBound %in% inBoundCreditType) >= 1)) {
            alertGenerator(accountNumber, inBoundCreditType, inBoundCredit)
        }

        # Generate Alerts for Debit Transaction
        if(nrow(subset(transactionData, transactionData$AccountNumber == accountNumber & transactionData$InBound %in% inBoundDebitType) >= 1)) {
        alertGenerator(accountNumber, inBoundDebitType, inBoundDebit)
        }
    }    
}

readData()
populateConfigurationParameters()
avgMonthlyCreditTransactionAmount(transactionData)
alertGeneration()