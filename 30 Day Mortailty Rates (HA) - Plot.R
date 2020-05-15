#PART 1 - PLOT A HISTOGRAM
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])

## You may get a warning about NAs being introduced; that is okay

hist(outcome[, 11], main="Maximum Mortality Rates from Heart Attack", 
                  xlab="30-Day Mortality Rates from Heart Attack",col="red")
