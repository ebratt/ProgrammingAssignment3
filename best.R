## 2014-08-22
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/ProgrammingAssignment3
## data from http://hospitalcompare.hhs.gov
## Assignment 3

## exercise 2
## function that takes a state and an outcome type
best <- function(state, outcome) {
    
    ## read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## require that state be a valid state
    states <- unique(data[, 7])
    stateIndex <- match(state, states)
    if (is.na(stateIndex)) {
        stop("invalid state")
    }
    ## require that outcome be a valid outcome
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    outcomeIndex <- match(outcome, outcomes)
    if (is.na(outcomeIndex)) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    ## get data for state
    data2 <- subset(data, State == state)
    
    ## map outcome to column number
    colMap <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcomeCol <- colMap[[outcome]]
    
    ## convert the outcome column to numeric
    data2[, outcomeCol] <- suppressWarnings(as.numeric(data2[, outcomeCol]))
            
    ## get a skinnier data set of hospital name and outcome column
    data3 <- data2[, c(2, outcomeCol)]
    
    ## rename columns for ease of future reference
    colnames(data3) <- c("Hospital", "Outcome")
    
    ## order the data by outcome and hospital name
    data4 <- data3[order(data3$Outcome, data3$Hospital), ]
    
    ## select the first hospital name
    data4[[1, 1]]
}