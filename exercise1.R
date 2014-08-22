## 2014-08-22
## Eric Bratt
## Coursera rprog-006
## https://github.com/ebratt/ProgrammingAssignment3
## data from http://hospitalcompare.hhs.gov
## Assignment 3

## exercise 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
