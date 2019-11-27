####################################################################################################
# Library Import
library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)
library(stringr)

####################################################################################################
### set work space
# setwd('topic/Crime/')
# Lee's directory
setwd("D:/GitHub/Data-Analytic/topic/Crime/")

####################################################################################################
### dataset
df <- read.csv('Seattle_Crime_Data.csv', header = T)
head(df)

####################################################################################################
### Functions
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

####################################################################################################
### Main

df_combined <- getYrData(2008, 2018, df)

### making a line graph
df_channged <- melt(df_combined, id = 'month')
df_channged

graph01 <- ggplot(df_channged, aes(x = month, y = value)) +
  geom_line(aes(colour = variable, group = variable))  +
  labs(x = "Month", y = "Frequency", title = "The Number of Monthly Crime Occurrences per Each Year", color='Year') +
  dark_theme_grey()

### calculating average values & adding a new column
average <- c()
for(i in c(1:12)) {
  average <- c(average, round(mean(as.numeric(df_combined[i,-12])), 0))
}
average
df_combined$avg <- average
df_combined

### adding a new line on graph01
graph02 <- graph01 + 
  geom_line(aes(x=month, avg, group=1), data=df_combined[,12:13], linetype='longdash', size = 1)
graph02