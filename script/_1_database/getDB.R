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
conn <- dbConnect(MySQL(), user="crime", password="Crime1q2w3e4r!", dbname="crimedb",host="ec2-13-209-74-172.ap-northeast-2.compute.amazonaws.com")

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
getSeattleCrimeData <- function(col) {
  query01 <- paste("SELECT ", str_c(col, sep='', collapse = ', ')," FROM SEATTLE_CRIME WHERE NOT OCC_DATE LIKE \'%2019\'")
  cat('\n', query01, '\n')
  res <- dbGetQuery(conn, query01)
  return(res)
}

# Get FBI Data by year
## param : vector
## return : result set
getFBIData <- function(year) {
  query01 <- paste("SELECT * FROM FBI_DATA WHERE YEAR LIKE", str_c(year, sep = '', collapse = ' or year like '))
  cat('\n', query01, '\n')
  res <- dbGetQuery(conn, query01)
  return(res)
}

# Get Edu Data
## param : vector
## return : result set
## param_val : all columns
getEduData <- function() {
  query01 <- paste("SELECT * FROM usa_univ_completions")
  cat('\n', query01, '\n')
  res <- dbGetQuery(conn, query01)
  return(res)
}

# Simple Query Sending Function
## param : query(text)
## return : result set
sendQuery <- function(query) {
  cat('\n', query, '\n')
  return(dbGetQuery(conn, query))
}
