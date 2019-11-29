####################################################################################################
### Functions
getColumns <- function(data, sYr, fYr, columns) {
  year <- as.character(c(sYr:fYr)) # Year vector (for loop)
  len <- nrow(data) # Total number of rows in the data frame
  colLen <- length(columns)
  month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  start <- 0
  
  # finds the starting point of the data
  for(i in c(1:len)) {
    if(substr(data[i,2], 7, 10) == as.character(sYr)) {
      cat(substr(data[i,2], 7, 10), '\n')
      start <- i
      break
    }
  }
  
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

getYrData <- function(sYr, fYr, data) {
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