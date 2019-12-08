####################################################################################################
### Library
library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)
library(jsonlite)

library(plotly)
library(car)

####################################################################################################
### Import
source("script/functions/functions.R")
source("database/getDB.R")

####################################################################################################
### Variables


####################################################################################################
##### Main
cdf <- getColumns("SUB_CATE")

# Finding the Frequency of each crime category
crimeF <- table(cdf$SUB_CATE)
#                     Var1   Freq
# 9              CAR PROWL 148123
# 26       THEFT-ALL OTHER  54239
# 29        THEFT-SHOPLIFT  48623
# 7   BURGLARY-RESIDENTIAL  46776
# 17   MOTOR VEHICLE THEFT  43483
crimeF <- as.data.frame(crimeF)
crimeF <- crimeF[order(crimeF$Freq, decreasing=T),]
crimeF <- crimeF[c(1:5),]

# Plotting TOP 5 Crime Frequency
crimePlot1 <- ggplot() + 
  geom_col(aes(Var1, Freq, fill=Var1), crimeF, position="stack", show.legend=F) +
  labs(x="Crime Subcategory", y="Frequency", title="TOP 5 Frequecy of Crime by Frequency between 2008 and 2018") +
  theme(axis.line=element_line(color="Black", size=1, linetype="solid")) 
crimePlot1

# OCC_DATE, OCC_TIME, REP_DATE, REP_TIME, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c("OCC_TIME", "SUB_CATE", "BEAT")
crimeData <- getColumns(col)

### Get Json File - Beat Area
map <- fromJSON("database/seattle_beat.json")
BeatDF <- map$features$attributes[,c(2,5)]

beatJSON <- unique(map$features$attributes$beat)
beatCrime <- unique(crimeData$BEAT)

beats <- intersect(beatJSON, beatCrime)

### Extract Rows from Data Frame
crime_beat <- crimeData %>% subset(BEAT %in% beats)

##################################################
##### 3D Plot "Car Prowl" - category, time, beat

### Categorize OCC_TIME and RPE_TIME
plot3dDF <- crime_beat[c("OCC_TIME", "SUB_CATE", "BEAT")]

### Group Beats to Sectors
plot3dDF$SECTOR <- substr(plot3dDF$BEAT, 1, 1)

### Subset "Car Prowl" category as it has the largest amount of crime frequency
plot3dDF <- subset(plot3dDF, SUB_CATE != "")

### Rearrange data frame and count occurances
plot3dData <- plot3dDF[c("OCC_TIME", "SUB_CATE", "SECTOR")]
catData <- as.data.frame(rename(count(plot3dData, OCC_TIME, SUB_CATE, SECTOR), FREQ=n))

### Only treat data if frequency is higher than 10
catData <- catData %>% subset(FREQ > 10)

### Changing columns data into integers for plotting
catData$SUB_CATE <- as.factor(catData$SUB_CATE)
catData$SECTOR <- as.factor(catData$SECTOR)

### 3D Scatter Plot with Color Scaling
plot1 <- plot_ly(catData, x=~OCC_TIME, y=~SUB_CATE, z=~SECTOR, size=0.1,
                 marker=list(color=~FREQ, colorscale=c("#FFE1A1", "#683531"), showscale=T)) %>% 
  add_markers() %>% 
  layout(scene=list(xaxis=list(title="Occured Time"),
                    yaxis=list(title="SUB_CATE"),
                    zaxis=list(title="SECTOR")),
         annotations=list(
           x=1.13, y=1.05,
           text="Frequency of Crime",
           xref="paper", yref="paper",
           showarrow=F
         ))
plot1

##################################################
##### Crime by Beat visualisation
# crime_beat$SECTOR <- substr(crime_beat$BEAT, 1, 1)

dbDisconnectAll()
