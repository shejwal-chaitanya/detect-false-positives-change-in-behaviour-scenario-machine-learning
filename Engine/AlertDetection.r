# Libraries
library(xlsx)

# Data Extraction
# Configurable parameters fill
# Logic run

# Data Extraction
data <- read.xlsx("../Data/CIB_Data.xlsx")

# Configurations
# Tunable
displayAllTransaction <<- ""
excludeCreditActivity <<- ""
minNumberSD <<- ""
maxNumberSD <<- ""
minCurrentMonthAmt <<- ""
minPercentageIncrease <<- ""
includeMonthsWithNoActivity <<- ""
lookBackPeriod <<- ""
minOpenDays <<- ""

# Non Tunable
# Daily = "D", Monthly = "M", Weekly = "W", Yearly = "Y"
frequencyPeriod <<- "M"