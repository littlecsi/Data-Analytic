####################################################################################################
### 여기서 절대로 코딩 하지 마세요 !!!
### 이곳은 코딩머신이 작업하는 공간입니다.
### source() 함수를 사용해서 이 파일을 참조하시기 바랍니다.
####################################################################################################

####################################################################################################
### Library
library(dplyr)
library(ggdark)
library(reshape2)
library(stringr)
library(ggplot2)

####################################################################################################
### Functions

# Normalize Function
## Min-Max Algorithm
## param : vector
## return : Normalized Vector
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
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

getColumnsDF <- function(data, sYr, fYr, columns) {
  year <- as.character(c(sYr:fYr)) # Year vector (for loop)
  len <- nrow(data) # Total number of rows in the data frame
  colLen <- length(columns)
  month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  start <- 0; end <- len;
  
  # finds the starting point and the end of the data
  startFound <- F
  for(i in c(1:len)) {
    if(substr(data[i,2], 7, 10) == as.character(sYr) & startFound == F) {
      start <- i
      startFound <- T
    }
    if(as.numeric(substr(data[i,2], 7, 10)) > fYr) {
      end <- i - 1
      break
    }
  }

  data <- data[c(start:end),]

  df <- data["OCC_DATE"]
  ym <- dateToYM(df, "occYear", "occMonth")

  df <- cbind(df, ym)
  
  if(colLen > 0) {
    for(i in c(1:colLen)) {
      df <- cbind(df, data[columns[i]])
    }
  }
  return(df)
}

getYrData <- function(data, sYr, fYr) {
  year <- as.character(c(sYr:fYr)) # Year vector (for loop)
  len <- nrow(data) # Total number of rows in the data frame
  month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  start <- 0

  # finds the starting point of the data
  for(i in c(1:len)) {
    if(substr(data[i,2], 7, 10) == as.character(sYr)) {
      start <- i
      break
    } 
  }

  # Extract only the data required.
  dates <- data %>% select("OCC_DATE")
  dates <- as.character(levels(unlist(dates)))[unlist(dates)]
  dates <- gsub("/", "", dates[c(start:len)])
  
  df_combined <- data.frame(a=c(1:12))
  for(y in year) {
    t_df <- as.data.frame(
      table(
        substr(
          subset(dates, substr(dates, 5, 8) == y), 1, 2)))
    df_combined <- cbind(df_combined, t_df$Freq)
  }
  df_combined <- df_combined[,-1]
  colnames(df_combined) <- year
  df_combined$month <- month

  return(df_combined)
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