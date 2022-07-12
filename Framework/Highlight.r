# File imports
source("../Framework/Constants.r", local = const <- new.env())

gbHighlightDataFrame <<- data.frame(
    NumberOfTransactions <- integer(),
    Amount <- double(),
    InBound <- logical()
)

createHighlight <- function(numberOfTransactions, amount, inBound) {
    highlightDataFrame <- data.frame(
        NumberOfTransactions <- as.integer(numberOfTransactions),
        Amount <- as.double(amount),
        InBound <- as.logical(inBound)
    )
    gbHighlightDataFrame <<- rbind(gbHighlightDataFrame, highlightDataFrame)
}

getHighlights <- function(data, inBound) {
    createHighlight(nrow(data), sum(data$Amount), inBound)
}

generateHighlight <- function(data) {
    # Generate highlights for credit transactions
    creditData <- subset(data, data$InBound == const$inBoundCredit)
    if(nrow(creditData) > 0) {
        getHighlights(creditData, const$inBoundCredit)
    }
    # Generate highlights for debit transactions
    debitData <- subset(data, data$InBound == const$inBoundDebit)
    if(nrow(debitData) > 0) {
        getHighlights(debitData, const$inBoundDebit)
    }

    # Renaming columns to avoid concatenation of dots to column names
    # Need to research why this happens
    names(gbHighlightDataFrame) <- c("NumberOfTransactions", "Amount", "InBound")
    return(gbHighlightDataFrame)
}