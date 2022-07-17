# Adding necessary libraries
library(lubridate)

# Calculates necessary amounts and values and runs checks for alert generation

generateAlert <- function(accountNumber, inBound, amount, date) {
    alertDataFrame <- data.frame(
        Date <- date,
        AccountNumber <- accountNumber,
        InBound <- inBound,
        Amount <- amount
    )
    return(alertDataFrame)
}

alertGenerator <- function(accountNumber, inBoundType, inBound, configData) {
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
    mulStdDevMinSd <- sdAmount * configData$MinNumberSD
    
    # Multiplication of SD and Max Number SD
    mulStdDevMaxSd <- sdAmount * configData$MaxNumberSD

    # Mulitiplied Average Amount with Minimum Percentage Increase
    mulAvgAmountMinPercentageIncrease <- avgAmount * (configData$MinPercentageIncrease/100)

    # Min Open Days Match
    accountOpenDateMatch <- (accountOpenDate) <= (Sys.Date() - configData$MinOpenDays)
    
    # Agg Amount - Avg Amount >= SD x Min(SD)
    diffAggAvgGreaterProductSdMinMatch <- diffAggAvg >= mulStdDevMinSd

    # Agg Amount - Avg Amount <= SD x Max(SD)
    diffAggAvgLesserProductSdMaxMatch <- diffAggAvg <= mulStdDevMaxSd
    
    # Avg amount exceeds minimum current month amount
    averageAmountExceedsMinCurrentAmountMatch <- aggCurrentMonthTransactions >= configData$MinCurrentMonthAmt

    # (aggCurrentMonthTransactions - avgAmount) >= Product of Average Amount with Minimum Percentage Increase
    diffAggAvgGreaterProductAvgAmountMicPercentageIncrease <- diffAggAvg >= mulAvgAmountMinPercentageIncrease

    if (accountOpenDateMatch & diffAggAvgGreaterProductSdMinMatch & diffAggAvgLesserProductSdMaxMatch & averageAmountExceedsMinCurrentAmountMatch & diffAggAvgGreaterProductAvgAmountMicPercentageIncrease) {
        alertGenerate <- generateAlert(accountNumber, inBound, aggCurrentMonthTransactions, today())
        return(alertGenerate)
    } else {
        return(data.frame())
    }
}

getAggregateCurrentMonthsTransaction <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound %in% inBoundType)
    currentMonthsFirstDate <- ceiling_date(Sys.Date() - months(1), "month")
    if (nrow(data >= 1)) {
        data <- subset(data, data$Date >= currentMonthsFirstDate)
        return (sum(data$Amount))
    } else {
        return (0)
    }       
}

getAccountOpenDate <- function(data, accountNumber) {
    data <- subset(data, data$AccountNumber == accountNumber)
    return (as.Date(data$AccountOpenDate))
}

getAvgAmount <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$gbAccountNumber == accountNumber & data$gbInBound == inBoundType)
    
    if (nrow(data) >= 1) {
        return (data$gbAvgAmount)
    } else {
        return(0)
    }
}

calculateStandardDeviation <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound %in% inBoundType)
    if (nrow(data >= 1)) {
        return (sd(data$Amount))
    } else {
        return (0)
    }
}