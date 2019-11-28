library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)

### set work space
setwd('topic/Crime/')

### dataset
df <- read.csv('Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)
head(df)
colnames(df)
nrow(df)

# get columns data with date
## param : necessary columns
## return : data frame(date, param data)
getColumns <- function(data, sYr, fYr, columns) {
  year <- as.character(c(sYr:fYr)) # Year vector (for loop)
  len <- nrow(data) # Total number of rows in the data frame
  colLen <- length(columns)
  month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  start <- 0
  
  # finds the starting point of the data
  for(i in c(1:len)) {
    if(substr(data[i,2], 7, 10) == as.character(sYr)) {
      start <- i
      break
    } 
  }
  
  df <- data %>% select('Occurred.Date')
  if(colLen > 0) {
    for(i in c(1:colLen)) {
      col <- columns[i]
      df <- cbind(df, data[col])
    }
  }
  return(df)
}

crimeCat <- getColumns(df, 2008, 2018, c('Crime.Subcategory'))
crimeCat <- subset(crimeCat, crimeCat$Crime.Subcategory != '')
head(crimeCat)

group <- unique(crimeCat$Crime.Subcategory)
df_group <- data.frame(Category = group, Index = c(1:length(group)))
df_group

mapCrime <- read.csv('group_crime.csv', header = T, stringsAsFactors = F)

for(i in c(1:ncol(mapCrime))) {
  mapCrime[,i] <- ifelse(is.na(mapCrime[,i]), 0, mapCrime[,i])
}

df_group$Category <- as.character(levels(unlist(df_group$Category)))[unlist(df_group$Category)]
crimeCat$Crime.SubcategoryIndex <- factor(crimeCat$Crime.Subcategory,
        levels=df_group$Category, labels=df_group$Index)

crimeCat$Category <- 
  ifelse(crimeCat[,3] %in% mapCrime[,2], 'Murder', 
  ifelse(crimeCat[,3] %in% mapCrime[,3], 'Rape', 
  ifelse(crimeCat[,3] %in% mapCrime[,4], 'Robbery', 
  ifelse(crimeCat[,3] %in% mapCrime[,5], 'Aggravated',
  ifelse(crimeCat[,3] %in% mapCrime[,6], 'Burglary', 
  ifelse(crimeCat[,3] %in% mapCrime[,7], 'LarcenyTheft', 
  ifelse(crimeCat[,3] %in% mapCrime[,8], 'VehicleTheft', 'Arson')))))))

crimeCat <- crimeCat[,-3]
violent <- c('Murder', 'Rape', 'Robbery', 'Aggravated')
crimeCat$Violent <- ifelse(crimeCat[,3] %in% violent, T, F)

vioTable <- table(crimeCat$Violent)
## True : Violent, False : Property
# FALSE   TRUE 
# 426579  96750

v_table <- data.frame(c(vioTable))

df_table <- cbind(v_table, Violent=c('Property', 'Violent'))
colnames(df_table) <- c('Freq', 'Type')

ggplot(data = df_table, aes(x = "", y = Freq, fill = Type)) + geom_bar(stat = 'identity') + coord_polar(theta = 'y', direction = 1) + labs(x = "", y = "", title = "Proportion of Crime Type during the Last 10 years") + theme_minimal()
