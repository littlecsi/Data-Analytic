####################################################################################################
### Library
library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)
library(jsonlite)
library(plot3D)

####################################################################################################
### Import
source("script/functions/functions.R")
source("database/getDB.R")

####################################################################################################
### Variables


####################################################################################################
##### Main

# OCC_DATE, OCC_TIME, REP_DATE, REP_TIME, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c("OCC_TIME", "REP_TIME", "SUB_CATE", "BEAT")
crimeData <- getColumns(col)
dbDisconnectAll()

### Get Json File - Beat Area
map <- fromJSON("database/seattle_beat.json")
BeatDF <- map$features$attributes[,c(2,5)]

beatJSON <- unique(map$features$attributes$beat)
beatCrime <- unique(crimeData$BEAT)

beats <- intersect(beatJSON, beatCrime)

### Extract Rows from Data Frame
crime_beat <- crimeData %>% subset(BEAT %in% beats)

##################################################
##### 3D Plot - category, time, beat

### Categorize REP_TIME
plot3dDF <- crime_beat[c("OCC_TIME", "SUB_CATE", "BEAT")]
plot3dDF$TIME <- ifelse(nchar(plot3dDF$OCC_TIME) <= 2, 0, # 12AM ~ 1AM
                  ifelse(nchar(plot3dDF$OCC_TIME) == 3, substr(plot3dDF$OCC_TIME, 1, 1), # 1AM ~ 12PM
                    substr(plot3dDF$OCC_TIME, 1, 2))) # 12PM ~ 12AM

### Group Beats to Sectors
plot3dDF$SECTOR <- substr(plot3dDF$BEAT, 1, 1)

### Rearrange data frame and remove OCC_TIME column
plot3dData <- plot3dDF[c("TIME", "SUB_CATE", "SECTOR")]

TIME <- plot3dData$TIME
SUB_CATE <- plot3dData$SUB_CATE
SECTOR <- plot3dData$SECTOR

scatter3D(x=TIME, y=SUB_CATE, z=SECTOR)

##################################################
##### Crime by Beat visualisation
crime_beat$SECTOR <- substr(crime_beat$BEAT, 1, 1)
