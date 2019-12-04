library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)
library(DBI)
library(RMySQL)
library(stringr)
library(openxlsx)
library(progress)

### Variable and Option

# Get Data set and Preprogress
df <- read.csv('dataset/Crime/Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)
df <- subset(df, as.numeric(substr(df$Occurred.Date, 7, 10)) > 2007)

# MySQL DB Connect 
conn <- dbConnect(MySQL(), user="crime", password="Crime1q2w3e4r!", dbname="crimedb",host="localhost")

### Function

# Database Insert Function
## param : data frame
dbSend <- function(df) {
  len <- nrow(df)
  pb <- progress_bar$new(total = len)
  
  for(l in c(1:len)) {
    query01 <- paste("INSERT INTO SEATTLE_CRIME VALUES(REP_NUM, \'", df$Occurred.Date[l], "\', ", df$Occurred.Time[l], ", \'", df$Reported.Date[l], "\', ", df$Reported.Time[l], ", \'", df$Crime.Subcategory[l], "\', \'", df$Primary.Offense.Description[l], "\', \'", df$Precinct[l], "\', \'", df$Sector[l], "\', \'", df$Beat[l], "\', \'", df$Neighborhood[l], "\')",  sep = '')
    # cat(query01, '\n')
    dbSendQuery(conn, query01)
    pb$tick()
  }
  cat('FIN')
}

# Database Disconnect All Function
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

### Main

# Database Send
dbSend(df)

### FIN

# DB Disconnect
dbDisconnectAll()
