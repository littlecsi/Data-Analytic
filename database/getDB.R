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

# MySQL DB Connect 
conn <- dbConnect(MySQL(), user="crime", password="Crime1q2w3e4r!", dbname="crimedb",host="localhost")

### Function

# Database Disconnect All Function
## param : none
## return : none
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

# Get Columns from Database
## param : vector
## return : result set
getColumns <- function(col) {
  query01 <- paste("SELECT ", str_c(col, sep='', collapse = ', ') ," FROM SEATTLE_CRIME")
  res <- dbGetQuery(conn, query01)
  return(res)
}

# Simple Query Sending Function
## param : query(text)
## return : result set
sendQuery <- function(query) {
  return(dbGetQuery(conn, query01))
}