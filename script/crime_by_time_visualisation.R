####################################################################################################
### Library
library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)
library(jsonlite)

####################################################################################################
### Import
source("script/functions/functions.R")
source("database/getDB.R")

####################################################################################################
### Variables
sector_list <- c("N","L","J","U","B","Q","D","M","C","E","G","K","W","F","M","")

####################################################################################################
### Main

# OCC_DATE, OCC_TIME, REP_DATE, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c("BEAT")

crimeData <- getColumns(col)

###### Get Json File - Beat Area
map <- fromJSON("database/seattle_beat.json")
BeatDF <- map$features$attributes[,c(2,5)]

beatJSON <- unique(map$features$attributes$beat)
beatCrime <- unique(crimeData$BEAT)
print(beatJSON && beatCrime)
