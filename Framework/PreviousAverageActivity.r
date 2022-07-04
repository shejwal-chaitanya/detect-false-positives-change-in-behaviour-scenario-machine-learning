# Calculates necessary amounts and values and runs checks for alert generation
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


    if (accountOpenDate <= (Sys.Date() - configData$MinOpenDays)) {
        alertsGenerated <<- append(alertsGenerated, "Minimum Open Days Alert")
    }
        
    if (diffAggAvg >= mulStdDevMinSd) {
        alertsGenerated <<- append(alertsGenerated, "Agg Amount - Avg Amount >= SD x Min(SD)")
    }

    if (diffAggAvg <= mulStdDevMaxSd) {
        alertsGenerated <<- append(alertsGenerated, "Agg Amount - Avg Amount <= SD x Max(SD)")
    }

    if (aggCurrentMonthTransactions >= configData$MinCurrentMonthAmt) {
        alertsGenerated <<- append(alertsGenerated, "Avg amount exceeds minimum current month amount")
    }

    if (diffAggAvg >= mulAvgAmountMinPercentageIncrease) {
        alertsGenerated <<- append(alertsGenerated, "Last Condition")
    }
    return(alertsGenerated)
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