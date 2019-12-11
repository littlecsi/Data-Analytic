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
## Get Seattle Crime Data
df <- read.csv('dataset/Crime/Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)
df <- subset(df, as.numeric(substr(df$Occurred.Date, 7, 10)) > 2007)

## Get FBI Data
fbi_csv <- data.frame()
year_vec <- c('08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18')
for(i in c(1:length(year_vec))) {
  path <- paste('dataset/Crime/FBI_Offenses/FBI_Offenses_20', year_vec[i], '.csv', sep = '')
  tmp <- read.csv(path, stringsAsFactors = F)
  fbi_csv <- rbind(fbi_csv, tmp)
}
for(i in c(1:ncol(fbi_csv))) {
  if(i == 2) {
    next()
  }
  fbi_csv[i] <- str_replace_all(unlist(fbi_csv[i]), ',', '')
  fbi_csv[i] <- as.numeric(unlist(fbi_csv[i]))
}
fbi_csv[is.na(fbi_csv)] <- 0

## Get Edu Data
edu_csv <- read.csv('dataset/Edu/Edu_Ver.csv')
edu_csv[is.na(edu_csv)] <- 0
str(edu_csv)

# MySQL DB Connect 
conn <- dbConnect(MySQL(), user="crime", password="Crime1q2w3e4r!", dbname="crimedb",host="localhost")

### Function

# Database Create Seattle Table Function
## param : none
dbCreateSeattleTable <- function() {
  query01 <- 'CREATE TABLE SEATTLE_CRIME(REP_NUM INT AUTO_INCREMENT PRIMARY KEY,OCC_DATE VARCHAR(32),OCC_TIME INT,REP_DATE VARCHAR(32),REP_TIME INT,SUB_CATE VARCHAR(128),PRI_DESC VARCHAR(128),PRECINCT VARCHAR(64),SECTOR VARCHAR(8),BEAT VARCHAR(8),NEIGHBOR VARCHAR(128))'
  dbSendQuery(conn, query01)
}

dbCreateFBITable <- function() {
  query02 <- 'CREATE TABLE FBI_DATA(YEAR INT,CITY VARCHAR(64),POPULATION INT,VIOLENT_CRIME INT,MURDER INT,RAPE INT,ROBBERY INT,AGGRABATED_ASSAULT INT,PROPERTY_CRIME INT,BURGLARY INT,LARCENY_THEFT INT,MOTOR_VEHICLE_THEFT INT,ARSON INT)'
  dbSendQuery(conn, query02)
}

dbCreateUnivTable <- function() {
  query03 <- 'CREATE TABLE USA_UNIV_COMPLETIONS(REP_NUM INT AUTO_INCREMENT PRIMARY KEY, YEAR_N INT,UNIV_ID INT,UNIV_NAME VARCHAR(256),SECTOR_ID INT,SECTOR_NAME VARCHAR(256),GEO_ID VARCHAR(256),GEO_NAME VARCHAR(64),COMPLETIONS INT)'
  dbSendQuery(conn, query03)
}

# Database Insert Function
# Seattle Crime data
## param : data frame
dbSendSeattleCrime <- function(df) {
  len <- nrow(df)
  pb <- progress_bar$new(format = "Inserting [:bar] :current/:total (:percent) elapsed::elapsedfull", total = len, clear = F)
  
  for(l in c(1:len)) {
    query01 <- paste("INSERT INTO SEATTLE_CRIME VALUES(REP_NUM, \'", df$Occurred.Date[l], "\', ", df$Occurred.Time[l], ", \'", df$Reported.Date[l], "\', ", df$Reported.Time[l], ", \'", df$Crime.Subcategory[l], "\', \'", df$Primary.Offense.Description[l], "\', \'", df$Precinct[l], "\', \'", df$Sector[l], "\', \'", df$Beat[l], "\', \'", df$Neighborhood[l], "\')",  sep = '')
    # cat(query01, '\n')
    dbSendQuery(conn, query01)
    pb$tick()
  }
  cat('FIN\n')
}

# FBI Data
## param : data frame
dbSendFBIData <- function(df) {
  len <- nrow(df)
  pb <- progress_bar$new(format = "Inserting [:bar] :current/:total (:percent) elapsed::elapsedfull", total = len, clear = F)
  for(l in c(1:len)) {
    query01 <- paste("INSERT INTO FBI_DATA VALUES(", 
                     df$Year[l], ", \'", 
                     df$City[l], "\', ", 
                     df$Population[l], ", ", 
                     df$Violent.crime[l], ", ", 
                     df$Murder[l], ", ", 
                     df$Rape[l], ", ", 
                     df$Robbery[l], ", ", 
                     df$Aggravated.assault[l], ", ", 
                     df$Property.crime[l], ", ", 
                     df$Burglary[l], ", ", 
                     df$Larceny.theft[l], ", ", 
                     df$Motor.vehicle.theft[l], ", ", 
                     df$Arson[l], ")", 
                     sep = '')
    dbSendQuery(conn, query01)
    pb$tick()
  }
  cat('FIN\n')
}

# Univ Completion data
dbSendEdu <- function(df) {
  len <- nrow(df)
  pb <- progress_bar$new(format = "Inserting [:bar] :current/:total (:percent) elapsed::elapsedfull", total = len, clear = F)
  for(l in c(1:len)) {
    query01 <- paste("INSERT INTO USA_UNIV_COMPLETIONS VALUES(REP_NUM, ", 
                     df$Year[l], ", ",
                     df$ID.University[l], ", \'", 
                     df$University[l], "\', ", 
                     df$ID.Sector[l], ", \'", 
                     df$Sector[l], "\', \'", 
                     df$ID.Geography[l], "\', \'", 
                     df$Geography[l], "\', ", 
                     df$Completions[l],")", 
                     sep = '')
    dbSendQuery(conn, query01)
    pb$tick()
  }
  cat('FIN\n')
}

# Database Disconnect All Function
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}


### Main

# Database Send
## Seattle Crime Data
# dbCreateSeattleTable()
# dbSendSeattleCrime(df)

## FBI Data
# dbCreateFBITable()
# dbSendFBIData(fbi_csv)

## Univ Completions data
dbCreateUnivTable()
dbSendEdu(edu_csv)

### FIN

# DB Disconnect
dbDisconnectAll()
