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

# function get Seasonal Average
## param : season data (data frame)
## return : vector
getSeasonalAvg <- function(season) {
  season_avg <- c()
  for(i in year_vector) {
    season_avg <- c(season_avg, round(mean(as.numeric(season[season$year == i,3])), 0))
  }
  return(season_avg)
}

# function get Seasonal Proportion
## param : season data (data frame), full data included season (data frame)
## return : vector
getSeasonalProp <- function(season, originData) {
  season_prop <- c()
  for(i in year_vector) {
    season_prop <- c(season_prop, round(sum(as.numeric(season[season$year == i, 3])) / sum(as.numeric(originData[originData$year == i, 3])), 4))
  }
  return(season_prop * 100)
}

# function get Seasonal Proportion
## param : season data (data frame), full data included season (data frame)
## return : vector
getSeasonalTot <- function(season, yearData) {
  yearProp <- round(sum(season[,3]) / sum(yearData[,3]), 4)
  return(yearProp * 100)
}

# function Transpose Data Frame
## param : data frame
## return : data frame
transDf <- function(season) {
  transposedData <- as.data.frame(t(as.matrix(season)))
  return(transposedData)
}