## 2014-08-22
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/ProgrammingAssignment3
## data from http://hospitalcompare.hhs.gov
## Assignment 3

## exercise 4
## function that takes two arguments: an outcome name (outcome) and a hospital 
## ranking (num). The function reads the outcome-of-care-measures.csv file and 
## returns a 2-column data frame containing the hospital in each state that 
## has the ranking specified in num. For example the function call 
##
##     rankall("heart attack", "best") 
##
## would return a data frame containing the names of the hospitals that are 
## the best in their respective states for 30-day heart attack death rates.

rankall <- function(outcome, num = "best") {
    rank <- num
    
    ## read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## require that outcome be a valid outcome
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    outcomeIndex <- match(outcome, outcomes)
    if (is.na(outcomeIndex)) {
        stop("invalid outcome")
    }
    ## require that rank be a valid rank
    ranks <- c("best", "worst")
    rankIndex <- match(rank, ranks)
    if (!is.numeric(rank) && is.na(rankIndex)) {
        stop("invalid rank")
    }
    
    ## map outcome to column number
    colMap <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcomeCol <- colMap[[outcome]]
    
    ## convert the outcome column to numeric
    data[, outcomeCol] <- suppressWarnings(as.numeric(data[, outcomeCol]))
    
    ## get a skinnier data set of hospital name, state, and outcome columns
    data2 <- data[, c(2, 7, outcomeCol)]
    
    ## rename columns for ease of future reference
    colnames(data2) <- c("hospital", "state", "Outcome")
    
    ## order the data by outcome and hospital name
    data3 <- data2[order(data2$Outcome, data2$hospital), ]
    
    ## remove non-numeric data in outcome column
    data4 <- subset(data3, !(Outcome == 'Not Available'))
    
    ## helper function to select the "rank"ed hospital name and state
    getHospitalForState <- function(my_state) {
        returnData <- {
            data5 <- subset(data4, data4$state == my_state)
            if (rank == "best") {
               data5[1, c(1, 2)]
            }
            else if (rank == "worst") {
                data5[nrow(data5), c(1, 2)]
            }
            else if (rank > nrow(data5)) {
                data.frame(hospital = NA, state = my_state)
            }
            else {
                data5[rank, c(1, 2)]
            }
        }
        returnData
    }
    
    ## loop over states and rbind them together using helper function
    states <- sort(unique(data[, 7]))
    returnValue <- data.frame(NULL)
    for (each_state in states) {
        returnValue <- rbind(returnValue, getHospitalForState(each_state))
    }
    returnValue
}