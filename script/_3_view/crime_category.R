library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)

source("script/_2_functions/functions.R")
source("script/_1_database/getDB.R")
### Main

# OCC_DATE, OCC_TIME, REP_DATE, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c('SUB_CATE')

### dataset
crimeCat <- getColumns(col)

crimeCat <- subset(crimeCat, crimeCat$SUB_CATE != '')
head(crimeCat)

group <- unique(crimeCat$SUB_CATE)
df_group <- data.frame(Category = group, Index = c(1:length(group)))
df_group

mapCrime <- read.csv('dataset/Crime/group_crime.csv', header = T, stringsAsFactors = F)

for(i in c(1:ncol(mapCrime))) {
  mapCrime[,i] <- ifelse(is.na(mapCrime[,i]), 0, mapCrime[,i])
}

df_group$Category <- as.character(levels(unlist(df_group$Category)))[unlist(df_group$Category)]
crimeCat$Crime.SubcategoryIndex <- factor(crimeCat$SUB_CATE,
        levels=df_group$Category, labels=df_group$Index)

crimeCat$Category <- 
  ifelse(crimeCat[,2] %in% mapCrime[,2], 'Murder', 
  ifelse(crimeCat[,2] %in% mapCrime[,3], 'Rape', 
  ifelse(crimeCat[,2] %in% mapCrime[,4], 'Robbery', 
  ifelse(crimeCat[,2] %in% mapCrime[,5], 'Aggravated',
  ifelse(crimeCat[,2] %in% mapCrime[,6], 'Burglary', 
  ifelse(crimeCat[,2] %in% mapCrime[,7], 'LarcenyTheft', 
  ifelse(crimeCat[,2] %in% mapCrime[,8], 'VehicleTheft', 'Arson')))))))

crimeCat <- crimeCat[,-2]
violent <- c('Murder', 'Rape', 'Robbery', 'Aggravated')
crimeCat$Violent <- ifelse(crimeCat[,2] %in% violent, T, F)

vioTable <- table(crimeCat$Violent)
## True : Violent, False : Property
# FALSE   TRUE 
# 426579  96750

v_table <- data.frame(c(vioTable))
tot <- sum(v_table[,1])
per_table <- rbind(round(v_table[2,1]/tot, 4) * 100, round(v_table[1,1]/tot, 4) * 100)
per_table <- cbind(Percent = per_table, Violent = c('Property', 'Violent'))
colnames(per_table) <- c('Percent', 'Type')

per_table <- as.data.frame(per_table)
per_table$Percent <- as.numeric(as.character.factor(per_table$Percent))

ggplot(data = per_table[order(per_table$Percent, decreasing = T),], aes(x = "", y = Percent, fill = Type)) + geom_bar(stat = 'identity') + coord_polar(theta = 'y', start = 0) + geom_text(aes(label = paste(Percent,'%')), position = position_stack(vjust = 0.5)) + labs(x = "", y = "", title = "Proportion of Crime Type during the Last 10 years") + theme_minimal()

dbDisconnectAll()