####################################################################################################
### Library
library(nnet)
library(neuralnet)
library(devtools)

####################################################################################################

calcAcc <- function(compTable) {
    len <- nrow(compTable)
    total <- 0
    for(l in c(1:len)) { total <- total + compTable[l,l] }
    accuracy <- round((total / sum(compTable)) * 100, 2)
    return(accuracy)
}