# Libraries
library(lubridate)

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

# Returns the last date of the month
getCeilingDate <- function(date) {
    return(ceiling_date(date, "months") - 1)
}

# Generates a dataframe based on account details
generateAccountDetails <- function(AverageAmount = 0, StandardDeviation = 0, dataFound = F, AmountTransacted = 0, Status = "Normal") {
    result <- list(dataFound, data.frame())
    if(dataFound) {
        accountDetails <- data.frame(
            AmountTransacted <- AmountTransacted,
            AverageAmount <- AverageAmount,
            StandardDeviation <- StandardDeviation,
            Status <- Status
        )
        names(accountDetails) <- c("AmountTransacted", "AverageAmount", "StandardDeviation", "Status")
        result <- list(dataFound, accountDetails)
    }
    names(result) <- c("DataFound", "AccountDetails")
    return(result)
}

# Get standard deviation
getStdDev <- function(data, mean, days, from, to) {
    difference <- c()
    while(from <= to) {
        amountData <- subset(data, data$Date == from)
        if (nrow(amountData) > 0) {
            difference <- append(difference, (sum(amountData$Amount) - mean) ^ 2)
        } else {
            difference <- append(difference, (-mean) ^ 2)
        }
        from <- from + days(1)
    }
    return(sqrt(sum(difference) / days))
}

# Get average amount for every month in the last 1 year
# Return type is dataframe
getAccountDetails <- function(accountNumber, inBoundType, data) {
    accountData <- subset(data, (data$AccountNumber == accountNumber) & (data$InBound == inBoundType))
    if(nrow(accountData) > 0) {

        # Initialize
        AmountTransacted <- c()
        AverageAmount <- c()
        StandardDeviation <- c()
        Status <- c()

        # Get last date of the previous month
        end_date <- floor_date(Sys.Date(), "months") - 1

        # ceiling_date returns the first date of the next month
        from_date <- ceiling_date(end_date - years(1), "months")

        while (getCeilingDate(from_date) <= end_date) {

            to_date <- getCeilingDate(from_date)
            monthlyAmountData <- subset(accountData, accountData$Date >= from_date & accountData$Date <= to_date)
            lastDateOfMonth <- day(to_date)
            # Calculate the average amount and append to the vector
            if (nrow(monthlyAmountData) >= 1) {
                avg <- sum(monthlyAmountData$Amount) / lastDateOfMonth
                AmountTransacted <- append(AmountTransacted, sum(monthlyAmountData$Amount))
                AverageAmount <- append(AverageAmount, avg)
                StandardDeviation <- append(StandardDeviation, getStdDev(monthlyAmountData, avg, lastDateOfMonth, from_date, to_date))
                Status <- append(Status, "Normal")
            } else {
                AmountTransacted <- append(AmountTransacted, 0)
                AverageAmount <- append(AverageAmount, 0)
                StandardDeviation <- append(StandardDeviation, 0)
                Status <- append(Status, "Normal")
            }

            # Resetting from date
            from_date <- from_date + months(1)
        }
        # Generate result
        return(generateAccountDetails(AverageAmount, StandardDeviation, T, AmountTransacted, Status))
    } else {
        return(generateAccountDetails())
    }
}