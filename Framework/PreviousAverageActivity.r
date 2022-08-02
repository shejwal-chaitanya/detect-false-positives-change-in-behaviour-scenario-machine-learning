# Adding necessary libraries
library(lubridate)

# File imports
source("../Framework/Constants.r", local = const <- new.env())

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

generateTestDataFrameRandomForest <- function(accountNumber, inBound, diffAggAvg, mulStdDevMinSd, mulStdDevMaxSd, aggCurrentMonthTransactions, mulAvgAmountMinPercentageIncrease) {
    output <- data.frame(
        AccountNumber <- accountNumber,
        InBound <- inBound,
        DiffAggAvg <- diffAggAvg,
        MinSd <- mulStdDevMinSd,
        MaxSd <- mulStdDevMaxSd,
        AggAmount <- aggCurrentMonthTransactions,
        AvgAmountPercentageIncrease <- mulAvgAmountMinPercentageIncrease
    )
    names(output) <- c("AccountNumber", "InBound","DiffAggAvg", "MinSd", "MaxSd", "AggAmount", "AvgAmountPercentageIncrease")
    return(output)
}

storeAlertGeneratorResults <- function(output) {
    write.table(output, file = "../Data/FalsePositiveRecords.csv", append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",", quote = FALSE)
}

alertGenerator <- function(accountNumber, inBoundType, inBound, configData, fpObject) {
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

    alertData <- generateTestDataFrameRandomForest(accountNumber, inBound, diffAggAvg, mulStdDevMinSd, mulStdDevMaxSd, aggCurrentMonthTransactions, mulAvgAmountMinPercentageIncrease)

    if (accountOpenDateMatch & diffAggAvgGreaterProductSdMinMatch & diffAggAvgLesserProductSdMaxMatch & averageAmountExceedsMinCurrentAmountMatch & diffAggAvgGreaterProductAvgAmountMicPercentageIncrease) {

        # Re-run using random forest
        # If output of random forest signifies that its FP, then send to analyst, else to supervisor
        randomForestOutput <- fp$randomForest(accountNumber, inBound, alertData)
        if (randomForestOutput == const$falsePositive) {
            cat("Random Forest evaluation -\n", "Is False Positive - TRUE\n", "Sent to Supervisor\n")
            alertData$FP = "Yes"
            storeAlertGeneratorResults(alertData)
            cat(const$lineBreaker)
        } else if (randomForestOutput == const$negFalsePositive) {
            cat("Random Forest evaluation -\n", "Is False Positive - FALSE\n", "Sent to Analyst\n")
            alertData$FP = "No"
            storeAlertGeneratorResults(alertData)
            cat(const$lineBreaker)
        } else {
            cat(paste("Data not found for account number", accountNumber, "with InBound Type:", inBound, "\n"))
            cat("Sent to analyst for further analysis. Analyst will store the results in history for further use.\n")
            cat(const$lineBreaker)
        }
        alertGenerate <- generateAlert(accountNumber, inBound, aggCurrentMonthTransactions, today())

        return(alertGenerate)
    } else {
        cat("Alert not generated. Skipped(Random Forest Evaluation)\n")
        alertData$FP = "Yes"
        cat(const$lineBreaker)
        return(data.frame())
    }
}

getAggregateCurrentMonthsTransaction <- function(data, accountNumber, inBoundType) {
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound %in% inBoundType)
    currentMonthsFirstDate <- ceiling_date(Sys.Date() - months(1), "month")
    if (nrow(data >= 1)) {
        data <- subset(data, data$Date >= currentMonthsFirstDate)
        if (nrow(data) >= 1) {
            return (sum(data$Amount))
        } else {
            return (0)
        }
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