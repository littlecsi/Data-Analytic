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
getcolumns <- function(data, columns) {
  len <- length(columns)
  df <- data %>% select('Occurred.Date')
  if(len > 0) {
    for(i in c(1:len)) {
      col <- columns[i]
      df <- cbind(df, data[col])
    }
  }
  return(df)
}

crimeCat <- getcolumns(df, c('Crime.Subcategory'))
crimeCat <- subset(crimeCat, crimeCat$Crime.Subcategory != '')
crimeCat

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

v_table <- as.data.frame(c(vioTable))

ggplot(data = v_table, aes(x = "", y = V1, fill = season)) + geom_bar(stat = 'identity') + coord_polar(theta = 'y', direction = -1) + labs(x = "Year", y = "Proportion", title = "Proportion of Seasonal Average Crime Occurance during the Last 10 years") + theme_minimal()
