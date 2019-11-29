####################################################################################################
### 여기서 절대로 코딩 하지 마세요 !!!
### 이곳은 코딩머신이 작업하는 공간입니다.
### source() 함수를 사용해서 이 파일을 참조하시기 바랍니다.
####################################################################################################

####################################################################################################
### Functions
getColumns <- function(data, sYr, fYr, columns) {
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

  df <- data[c(start:len),] %>% select('Occurred.Date')
  
  df$year <- substr(df[,1], 7, 10)
  df$month <- substr(df[,1], 1, 2)
  
  if(colLen > 0) {
    for(i in c(1:colLen)) {
      col <- columns[i]
      df <- cbind(df, data[col])
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
  for(i in c(1:len)) { if(substr(data[i,2], 7, 10) == as.character(sYr)) { start <- i; break; } }

  # Extract only the data required.
  dates <- data %>% select("Occurred.Date")
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

####################################################################################################
# ### Test
library(dplyr)

setwd("./topic/Crime/")

df <- read.csv('Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)

df_combined <- getColumns(df, 2008, 2018, c("Sector"))

str(df)