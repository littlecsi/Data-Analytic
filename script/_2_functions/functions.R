#########################################################################################
# source() 함수를 사용해서 이 파일을 참조하시기 바랍니다.
#########################################################################################

#########################################################################################
### Library
library(dplyr)
library(ggdark)
library(reshape2)
library(stringr)
library(ggplot2)
library(progress)

#########################################################################################
### Functions

# Normalize Function
## Min-Max Algorithm
## param : vector
## return : Normalized Vector
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Cutting necessary columns
## param : vector
## return : data frame
cutColumns <- function(col) {
  res <- base_data %>% select(str_c(col))
  return(res)
}

# Add Year and Month Columns
## param : data frame
## return : data frame
cutMonthYear <- function(df) {
  tmp <- df[, -1]
  df <- as.data.frame(df[, 1])
  df$year <- str_sub(df[, 1], 7, 10)
  df$month <- str_sub(df[, 1], 1, 2)
  df <- cbind(df, tmp)
  return(df)
}

dateToYM <- function(dates, yearName, monthName) {
  if(class(dates) == "data.frame") {
    yVec <- substr(unlist(dates), 7, 10)
    mVec <- substr(unlist(dates), 1, 2)

    df <- data.frame(yVec=yVec, mVec=mVec)
    colnames(df) <- c(as.character(yearName), as.character(monthName))

    return(df)
  }
  cat("dateToYM function Error \n")
}