## 2014-08-22
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/ProgrammingAssignment3
## data from http://hospitalcompare.hhs.gov
## Assignment 3

## exercise 3
## function that takes a state, an outcome type, and a ranking number
## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the ranking 
## specified by the num argument. For example, the call
## 
##    rankhospital("MD", "heart failure", 5)
## 
## would return a character vector containing the name of the hospital with 
## the 5th lowest 30-day death rate for heart failure.

rankhospital <- function(state, outcome, num = "best") {
    rank <- num
    
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
    ## require that rank be a valid rank
    ranks <- c("best", "worst")
    rankIndex <- match(rank, ranks)
    if (!is.numeric(rank) && is.na(rankIndex)) {
        stop("invalid rank")
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
    
    ## remove non-numeric data in outcome column
    data5 <- subset(data4, !(Outcome == 'Not Available'))
    
    ## select the "rank"ed hospital name
    if (rank == "best") data5[[1, 1]]
    else if (rank == "worst") data5[[nrow(data5), 1]]
    else if (rank > nrow(data5)) NA
    else data5[[rank, 1]]
}