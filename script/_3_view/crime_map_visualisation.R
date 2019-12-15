####################################################################################################
##### Library
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(jsonlite)

library(progress)

####################################################################################################
##### Import
source("script/_1_database/getDB.R")
source("script/_2_functions/functions.R")

####################################################################################################
##### Variables
map <- fromJSON("dataset/seattle_beat.json")

####################################################################################################
##### ggmap setup
ggmap::register_google(key="AIzaSyCJ__Rslc-MfKssqQb0EURUOWV7ZE_o8nE")

####################################################################################################
##### Main

##################################################
##### Relationship between Beat Area and Crime Frequency
AreaDf <- data.frame(beat=map$features$attributes$beat, area=map$features$attributes$shape_Area)
AreaDf$beat <- as.character(levels(unlist(AreaDf$beat)))[unlist(AreaDf$beat)]

# OCC_DATE, OCC_TIME, REP_DATE, REP_TIME, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c("BEAT")
crimeData <- getColumns(col)

beatCrime <- unique(crimeData$BEAT)

beats <- intersect(AreaDf$beat, beatCrime)

### Extract Rows from Data Frame
AreaDf <- AreaDf %>% subset(beat %in% beats)
crimeData <- crimeData %>% subset(BEAT %in% beats)

crimeTable <- as.data.frame(table(crimeData$BEAT))
colnames(crimeTable) <- c('beat','frequency')

### Sort
AreaDf <- AreaDf[order(AreaDf$beat),]

### Merge
AreaDf <- cbind(AreaDf, crimeTable$frequency)
colnames(AreaDf) <- c('beat','area','freq')

### Visualise
plot1 <- ggplot(data=AreaDf, mapping=aes(x=area, y=freq)) +
  geom_point(stat="identity") + 
  geom_smooth() +
  labs(title="Crime Frequency against Sector Area", x="Area of Sector", y="Crime Frequency")
plot1 + geom_density_2d()

# Scatter plot is very spread out with very weak regression relationship

##################################################
##### Map Visualisation

### Crime Frequency by Neighborhood
neighFreq <- getColumns("NEIGHBOR")
neighFreq <- as.data.frame(table(neighFreq$NEIGHBOR))

### Coordinates of borders
llpoint <- map$features$geometry$rings

plot3 <- ggmap(get_googlemap(center=c(lon=-122.335467, lat=47.608013),
                             zoom=11, scale=2,
                             maptype='terrain',
                             color='bw'))

pb <- progress_bar$new(format = "Plotting [:bar] :current/:total (:percent) elapsed::elapsedfull", total = 55, clear = F)
for(i in c(1:length(llpoint))) {
  for(j in c(1:length(llpoint[[i]]))) {
    if(nrow(llpoint[[i]][[j]]) != 1) {
      for(k in c(1:nrow(llpoint[[i]][[j]]))) {
        plot3 <- plot3 + geom_point(x=llpoint[[i]][[j]][k,1], y=llpoint[[i]][[j]][k,2], size = 0.5)
      }
    } else {
      for(k in c(1:length(llpoint[[j]])/2)) {
        plot3 <- plot3 + geom_point(x=llpoint[[i]][1,k,1], y=llpoint[[i]][1,k,2], size=0.5)
      }
    }
  }
  pb$tick()
}

plot3

dbDisconnectAll()
