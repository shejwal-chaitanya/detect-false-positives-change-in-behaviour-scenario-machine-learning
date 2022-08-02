# File imports
source("../Framework/Constants.r", local = const <- new.env())
source("../Framework/FileOperations.r", local = fileOperations <- new.env())
source("../Framework/Transactions.r", local = transactions <- new.env())
source("../Framework/PreviousAverageActivity.r", local = snroPAA <- new.env())
source("../Framework/Highlight.r", local = hlt <- new.env())
source("../ML/FalsePositive.r", local = fp <- new.env())

# Libraries
library(lubridate)
library(dplyr)

# Global variables
alertsGenerated <<- data.frame(
    Date <- lubridate::Date(),
    AccountNumber <- integer(),
    InBound <- logical(),
    Amount <- double()
)

gbAccountNumber <<- c()
gbAvgAmount <<- c()
gbInBound <<- c()
avgMonthlyCreditDebitTransaction <<- data.frame(
    gbAccountNumber <- integer(), 
    gbAvgAmount <- double(), 
    gbInBound <- character()
)

# Data Extraction
readData <- function() {
    configData <<- fileOperations$readData(fileOperations$configurations, fileOperations$CSV)
    transactionData <<- fileOperations$readData(fileOperations$transactions, fileOperations$CSV)
    accountDetailsData <<- fileOperations$readData(fileOperations$accountDetails, fileOperations$CSV)
}

addAccountTransactionDetails <- function(accountNumber, avgAmount, inBound) {
    # Create a new transaction record for storing the average amount for the account number and inbound type
    # Condition - To ensure no account number is repeated for same inbound type
    if (nrow(subset(avgMonthlyCreditDebitTransaction, avgMonthlyCreditDebitTransaction$gbAccountNumber == accountNumber & avgMonthlyCreditDebitTransaction$gbInBound == inBound)) == 0) {
        gbAccountNumber <<- accountNumber
        gbAvgAmount <<- avgAmount
        gbInBound <<- inBound
        avgMonthlyCreditDebitTransaction <<- rbind(avgMonthlyCreditDebitTransaction, data.frame(gbAccountNumber, gbAvgAmount, gbInBound))
    }
}

calculateAvgMonthlyTransaction <- function(data, inBound) {
    avgAmount <- transactions$calculateAvgMonthlyTransaction(data, inBound, configData)
    addAccountTransactionDetails(unique(data$AccountNumber), avgAmount, inBound)
}

avgMonthlyCreditTransactionAmount <- function(data) {
    # Extracting distinct account numbers using factor object
    accountNumbers <- factor(data$AccountNumber)
    for(currentAccountNumber in levels(accountNumbers)) {
        recordData <- subset(data, data$AccountNumber == currentAccountNumber)
        recordData <- subset(recordData, format(as.Date(recordData$Date), "%Y-%m") != format(as.Date(Sys.Date()), "%Y-%m"))
        if (nrow(recordData) >=1 ) {
            creditTransactionData <- subset(recordData, recordData$InBound %in% const$inBoundCreditType)
            if (nrow(creditTransactionData) >= 1) {
                calculateAvgMonthlyTransaction(creditTransactionData, const$inBoundCredit)
            }

            debitTransactionData <- subset(recordData, recordData$InBound %in% const$inBoundDebitType)
            if (nrow(debitTransactionData) >= 1) {
                calculateAvgMonthlyTransaction(debitTransactionData, const$inBoundDebit)
            }
        }
    }
}

showOutput <- function(accountNumber) {
    print(alertsGenerated)
}

alertGeneration <- function() {
    accounts <- factor(avgMonthlyCreditDebitTransaction$gbAccountNumber)
    for (accountNumber in levels(accounts)) {
        accountNumber <- as.numeric(accountNumber)
        
        # Generate Alerts for Credit Transactions
        if(nrow(subset(transactionData, transactionData$AccountNumber == accountNumber & transactionData$InBound %in% const$inBoundCreditType) >= 1)) {
            result <- transactions$getAccountDetails(accountNumber, const$inBoundCredit, transactionData)
            # Generate abnormal data behaviour according to configuration
            result <- fp$generateAbormalBehaviourData(result, configData)
            cat(const$lineBreaker)
            cat("Credit Transaction Results for AccountNumber -", accountNumber, "\n")
            # Run the data against hierarchical clustering
            hcResult <- fp$hierarchicalClustering(accountNumber, result$EntireAccountHistory["Amount"], configData, const$inBoundCreditType)
            cat("Hierarchical Clustering Output For Account Number -", accountNumber,hcResult, "\n")
            if(hcResult == const$normalBehaviour) {
                cat(const$lineBreaker)
            } else {
                # Run the data against support vector machine
                svmOutput <- fp$supportVectorMachine(result$AccountDetails, configData, accountNumber, const$inBoundCredit)
                cat("SVM Output for Account Number - ", accountNumber, svmOutput, "\n")
                if (svmOutput == const$normalBehaviour) {
                    cat(const$lineBreaker)
                } else {
                    checkForAlert <- snroPAA$alertGenerator(accountNumber, const$inBoundCreditType, const$inBoundCredit, configData, fp)
                    if (nrow(checkForAlert) > 0) {
                        # Alert generated added to the dataframe
                        alertsGenerated <<- rbind(alertsGenerated, checkForAlert)
                    }
                }
            }
            cat(const$lineBreaker)
        }

        # Generate Alerts for Debit Transaction
        if(nrow(subset(transactionData, transactionData$AccountNumber == accountNumber & transactionData$InBound %in% const$inBoundDebitType) >= 1)) {
            result <- transactions$getAccountDetails(accountNumber, const$inBoundDebit, transactionData)
            # Generate abnormal data behaviour according to configuration
            result <- fp$generateAbormalBehaviourData(result, configData)
            cat(const$lineBreaker)
            cat("Debit Transaction Results for AccountNumber -", accountNumber, "\n")
            # Run the data against hierarchical clustering
            hcResult <- fp$hierarchicalClustering(accountNumber, result$EntireAccountHistory["Amount"], configData, const$inBoundDebitType)
            cat("Hierarchical Clustering Output For Account Number -", accountNumber, " ",  hcResult, "\n")
            if(hcResult == const$normalBehaviour) {
                cat(const$lineBreaker)
            } else {
                # Run the data against support vector machine
                svmOutput <- fp$supportVectorMachine(result$AccountDetails, configData, accountNumber, const$inBoundDebit)
                cat("SVM Output for Account Number - ", accountNumber, svmOutput, "\n")
                if (svmOutput == const$normalBehaviour) {
                    cat(const$lineBreaker)
                } else {
                    checkForAlert <- snroPAA$alertGenerator(accountNumber, const$inBoundDebitType, const$inBoundDebit, configData)
                    if (nrow(checkForAlert) > 0) {
                        # Alert generated added to the dataframe
                        alertsGenerated <<- rbind(alertsGenerated, checkForAlert)
                    }
                }
            }
            cat(const$lineBreaker)
        }
    }    
}

displayAlertsHighlights <- function() {
    if(nrow(alertsGenerated) > 0) {
        cat("---------- Alerts Generated ----------\n")
        print(alertsGenerated)
        cat("\n\n\n")
        cat("---------- Highlights Generated ----------\n")
        print(hlt$generateHighlight(alertsGenerated))
        cat("\n\n\n")
    }
}

alertsGenerateNameChange <- function() {
    names(alertsGenerated) <<- c("Date", "AccountNumber", "InBound", "Amount")
}

readData()
avgMonthlyCreditTransactionAmount(transactionData)
alertGeneration()
alertsGenerateNameChange()
displayAlertsHighlights()