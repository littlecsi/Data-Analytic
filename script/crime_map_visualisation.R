####################################################################################################
##### Library
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)

library(progress)

####################################################################################################
##### Import
source("database/getDB.R")

####################################################################################################
##### ggmap setup
ggmap::register_google(key="AIzaSyCJ__Rslc-MfKssqQb0EURUOWV7ZE_o8nE")

####################################################################################################
##### Main

##################################################
##### Map Visualisation

### Crime Frequency by Neighborhood
neighFreq <- getColumns("NEIGHBOR")
neighFreq <- as.data.frame(table(neighFreq$NEIGHBOR))

### Coordinates of borders
map <- fromJSON("database/seattle_beat.json")
lonlatpoint <- map$features$geometry$rings

plot3 <- ggmap(get_googlemap(center=c(lon=-122.335467, lat=47.608013),
                             zoom=11, scale=2,
                             maptype='terrain',
                             color='bw'))
plot3

pb <- progress_bar$new(format = "Plotting [:bar] :current/:total (:percent) elapsed::elapsedfull", total = 55, clear = F)
pb$tick(0)
for(i in c(1:length(lonlatpoint))) {
  cat(i, '\n')
  for(j in c(1:length(lonlatpoint[[i]]))) {
    cat(j, '\n')
    for(k in c(1:length(lonlatpoint[[i]][[j]])/2)) {
      cat(k, '\n')
      plot3 + geom_point(x=lonlatpoint[[i]][[j]][k,1], y=lonlatpoint[[i]][[j]][k,2], size = 0.5)
    }
  }
  pb$tick(1)
}

dbDisconnectAll()
