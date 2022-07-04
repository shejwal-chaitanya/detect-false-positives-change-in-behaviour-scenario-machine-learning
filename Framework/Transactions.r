# Returns the last date of the previous month
getLastDateOfPreviousMonth <- function(latestDate) {
    #Get last date of previous month
    lastDateOfPreviousMonth <- ceiling_date(as.Date(latestDate) - months(1), "months") - 1
    return(lastDateOfPreviousMonth)
}

# Returns the first date of the month after lookback period is subracted from the current datemonthlyFrequency
getCeilingDateBeforeLookbackPeriod <- function(latestDate) {
    # Returns the first date of the month after lookback period is subtracted

    #Get last date of previous month
    lastDateOfPreviousMonth <- getLastDateOfPreviousMonth(latestDate)

    # Get first date of the month after lookback period is subtracted
    firstDateOfMonthWithLookbackPeriod <- as.Date(cut(lastDateOfPreviousMonth - years(1), "month"))
    return (firstDateOfMonthWithLookbackPeriod)
}

# Calculates the number of months in which the transaction occured
getNumberOfMonthsWithNoActivity <- function(data) {

    summarisedAccountData <- subset(data, getLastDateOfPreviousMonth(max(as.Date(data$Date))) >= getCeilingDateBeforeLookbackPeriod(max(as.Date(data$Date)))) %>% group_by(format(as.Date(data$Date)), "%m") %>% summarise(totalAmount = sum(data$Amount))

    return (nrow(summarisedAccountData))
}

# Calculates total amount for the given account number
# From - before 12 months (as specified in lookback period) To - previous month's last date
getTotalAmount <- function(data) {
    return (sum(subset(data$Amount, (getLastDateOfPreviousMonth(max(as.Date(data$Date))) >= getCeilingDateBeforeLookbackPeriod(max(as.Date(data$Date)))))))
}

# Calculate the Average Amount
calculateAvgMonthlyTransaction <- function(data, inBound, configData) {
    if (configData$IncludeMonthsWithNoActivity == const$yes) {
        # Calculating Avg Monthly Credit Transaction Amount
        avgAmount <- (getTotalAmount(data) / configData$LookbackPeriod)
    } else {
        avgAmount <- (getTotalAmount(data) / getNumberOfMonthsWithNoActivity(data))
    }
    return(avgAmount)
}