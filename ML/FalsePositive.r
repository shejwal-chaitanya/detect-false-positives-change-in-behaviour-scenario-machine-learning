library(factoextra)
library(class)
library(scorecard)
library(caTools)

source("../Framework/PreviousAverageActivity.r", local = snroPAA <- new.env())
source("../Framework/Transactions.r", local = transactions <- new.env())

generateTestDataFrame <- function(accountNumber, inBoundType) {
    # Get current months aggregate amount
    amountData <- snroPAA$getAggregateCurrentMonthsTransaction(transactionData, accountNumber, inBoundType)
    averageAmountData <- amountData / day(ceiling_date(Sys.Date(), "months") - 1)

    monthlyAmountData <- subset(transactionData, transactionData$AccountNumber == accountNumber & transactionData$InBound %in% inBoundType)

    from_date <- floor_date(Sys.Date(), "months")
    to_date <- ceiling_date(Sys.Date(), "months") - 1
    lastDateOfMonth <- day(ceiling_date(Sys.Date(), "months") - 1)
    stdDev <- transactions$getStdDev(monthlyAmountData, amountData, lastDateOfMonth, from_date, to_date)

    return(transactions$generateAccountDetails(averageAmountData, stdDev, T, amountData))
}

generateAbormalBehaviourData <- function(data, configData) {
    abnormalData <- subset(data, data$AverageAmount > 0 & data$StandardDeviation > 0)
    abnormalData$AverageAmount = abnormalData$AverageAmount * (configData$MinPercentageIncrease / 100)
    abnormalData$StandardDeviation = abnormalData$StandardDeviation * configData$MaxNumberSD

    data <- rbind(data, abnormalData)
    return(data)
}

hierarchicalClustering <- function(accountNumber, data, configData, inBoundType) {

    # Current Data
    currentData <- generateTestDataFrame(accountNumber, inBoundType)

    # Add additional data above min percentage increase for abnormial behaviour
    data <- generateAbormalBehaviourData(data, configData)

    # Generate cluster based on amount only
    data <- data[1]

    # Scale the data
    scaledData <- scale(data)

    # Calculating the distance
    distData <- dist(scaledData, method = "euclidean")

    # Row names
    rownames(scaledData) <- paste("month", 1:dim(data)[1], sep = "_")

    # Creating clustering model
    hcModel <- hclust(distData, method = "complete")

    # Labeling data
    hcModel$labels <- c("Normal", "Abnormal")

    # Cluster Groups
    # k = 2, i.e., 2 clusters, Normal and Abnormal
    groups <- cutree(hcModel, k = 2)

    # Plotting the cluster
    #fviz_cluster(list(data = scaledData, cluster = groups))

    # Current data prediction
    predictedBehaviour <- knn(train = data, test = currentData$AccountDetails[1], k = 1, cl = groups)

    # Behaviour output
    hcModel$labels[predictedBehaviour]

    return(hcModel$labels[predictedBehaviour])
}