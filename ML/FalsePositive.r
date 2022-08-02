library(factoextra)
library(class)
library(scorecard)
library(caTools)
library(e1071)
library(randomForest)
library(dplyr)

# File imports
source("../Framework/Constants.r", local = const <- new.env())
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

    accHistory <- transactions$getEntireAccountHistory(transactionData, to_date, from_date)

    stdDev <- transactions$getStdDev(monthlyAmountData, amountData, lastDateOfMonth, from_date, to_date)

    return(transactions$generateAccountDetails(averageAmountData, stdDev, T, amountData, EntireAccountHistory = accHistory))
}

generateAbormalBehaviourData <- function(data, configData) {
    # Get only valid data, else the accuracy might be disturbed
    abnormalData <- subset(data$AccountDetails, data$AccountDetails["AverageAmount"] > 0 & data$AccountDetails["StandardDeviation"] > 0)

    abnormalData["AmountTransacted"] = abnormalData["AmountTransacted"] * (configData$MinPercentageIncrease / 100)

    abnormalData["AverageAmount"] = abnormalData["AverageAmount"] * (configData$MinPercentageIncrease / 100)

    abnormalData["StandardDeviation"] = abnormalData["StandardDeviation"] * configData$MaxNumberSD

    abnormalData["Status"] = const$abnormalBehaviour
    
    data$AccountDetails <- rbind(data$AccountDetails, abnormalData)

    abnormalData <- subset(data$EntireAccountHistory, data$EntireAccountHistory["Amount"] > 0)

    abnormalData["Amount"] = abnormalData["Amount"] * (configData$MinPercentageIncrease / 100)
    abnormalData["Status"] = const$abnormalBehaviour

    data$EntireAccountHistory <- rbind(data$EntireAccountHistory, abnormalData)

    return(data)
}

hierarchicalClustering <- function(accountNumber, data, configData, inBoundType) {

    # Current Data
    currentData <- generateTestDataFrame(accountNumber, inBoundType)

    # Scale the data
    scaledData <- scale(data)

    # Calculating the distance
    distData <- dist(scaledData, method = "euclidean")

    # Row names
    rownames(scaledData) <- paste("month", 1:dim(data)[1], sep = "_")

    # Creating clustering model
    hcModel <- hclust(distData, method = "complete")

    # Labeling data
    hcModel$labels <- c(const$normalBehaviour, const$abnormalBehaviour)

    # Cluster Groups
    # k = 2, i.e., 2 clusters, Normal and Abnormal
    groups <- cutree(hcModel, k = 2)

    # Plotting the cluster, since dim of data < 2, plotting not possible
    # fviz_cluster(list(data = scaledData, cluster = groups))

    # Current data prediction
    predictedBehaviour <- knn(train = data, test = currentData$AccountDetails[1], k = 1, cl = groups)

    return(hcModel$labels[predictedBehaviour])
}

supportVectorMachine <- function(data, configData, accountNumber, inBound) {

    # Generate abnormal behaviour data
    # data <- generateAbormalBehaviourData(data, configData)

    # Data only for columns "Average Amount", "Standard Deviation"
    data <- select(data, c("AverageAmount", "StandardDeviation", "Status"))

    data$Status <- as.factor(data$Status)

    data <- subset(data, data$AverageAmount > 0 & data$StandardDeviation > 0)
    
    # Splitting the data into training and testing dataset
    # 80% training dataset, 20% testing dataset
    sample <- caTools::sample.split(data, SplitRatio = 0.8)
    train <- subset(data, sample == TRUE)
    test <- subset(data, sample == FALSE)

    # Tune model
    # tunedModel <- tune(svm, Status ~., data = train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))

    #summary(tunedModel)
    # Model generation
    svmModel <- svm(Status ~., data = train, kernel = "linear", cost = 100, scale = FALSE)

    # Prediction
    predictedValues <- predict(svmModel, test, type = "class")

    # Accuracy
    cat("Accuracy of the model -- ", (mean(predictedValues == test[, 3])) * 100, "%\n")
    # Current data prediction with the model
    currentData <- generateTestDataFrame(accountNumber, inBound)
    return(predict(svmModel, currentData$AccountDetails, type = "response"))
}

randomForest <- function(accountNumber, inBound, alertData) {
    # Set Environment variables
    Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-18.0.1.1")

    # Import data for the given account number and inbound type
    data <- read.csv("../Data/FalsePositiveRecords.csv")

    # Retrieving data for the given acc number and inbound type
    data <- subset(data, data$AccountNumber == accountNumber & data$InBound == inBound)

    if (nrow(data) > 0) {
        # Converting FP to factors as it is our output and categorical variable
        data$FP <- as.factor(data$FP)

        # Creating training and testing dataset
        split <- caTools:sample.split(data, SplitRatio = 0.7)
        train <- subset(data, split == TRUE)
        test <- subset(data, split == FALSE)

        # Generating random forest model
        rdModel <- randomForest(FP ~., data = train, mtry = 4, ntree = 2000, importance = TRUE)

        # plot model
        plot(rdModel)

        # Prediction
        result <- as.vector(predcit(rdModel, select(alertData, -c("AccountNumber", "InBound"))))

        if (result == "1") {
            return(const$falsePositive)
        } else {
            return(const$negFalsePositive)
        }
    } else {
        return(const$dataNotFound)
    }
}