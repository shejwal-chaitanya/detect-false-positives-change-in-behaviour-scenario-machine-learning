# Data needed:
# Current months aggregate amount, then we will find the avg for it
# Aggregate amount of each month for the last 12 months

library(lubridate)

dummyDates <- seq(as.Date("2022-03-01"), as.Date("2022-04-30"), by = 1)
dummyAmount <- rnorm(61, mean = 20000, sd = 3000)
dummyData <- data.frame(
    Date <- seq(as.Date("2022-03-01"), as.Date("2022-04-30"), by = 1),
    AccountNumber <- 5678,
    Amount <- rnorm(61, mean = 500, sd = 50),
    InBound <- F
)
names(dummyData) <- c("Date","AccountNumber","Amount","InBound")
print(dummyData)
write.csv(dummyData, "dummyData.csv", row.names = F)