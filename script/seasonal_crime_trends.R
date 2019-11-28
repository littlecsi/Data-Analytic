library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)

### set work space
setwd('topic/Crime/')

# function get Seasonal Average
## param : season data (data frame)
## return : vector
getSeasonalAvg <- function(season) {
  season_avg <- c()
  for(i in c(1:11)) {
    season_avg <- c(season_avg, round(mean(as.numeric(season[,i])), 0))
  }
  return(season_avg)
}

# function get Seasonal Proportion
## param : season data (data frame), full data included season (data frame)
## return : vector
getSeasonalProp <- function(season, originData) {
  season_prop <- c()
  for(i in c(1:11)) {
    season_prop <- c(season_prop, round(sum(as.numeric(season[,i]))/sum(as.numeric(originData[,i])), 4))
  }
  return(season_prop * 100)
}

# function get Seasonal Proportion
## param : season data (data frame), full data included season (data frame)
## return : vector
getSeasonalTot <- function(season, yearData) {
  yearProp <- round(sum(season[,-12])/sum(yearData[,-12]), 4)
  return(yearProp * 100)
}

# function Transpose Data Frame
## param : data frame
## return : data frame
transDf <- function(season) {
  transposedData <- as.data.frame(t(as.matrix(season)))
  return(transposedData)
}

### dataset
df <- read.csv('Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)
head(df)

### sorting dataset to extract 'Occurred.Date' column
df01 <- df %>% 
  select('Occurred.Date')
head(df01)

### 2008 ~ 2019 data
# hard-coding is the best!!!!! 
# View(substr(df01$Occurred.Date, 7, 10) == '2008')
# df$Occurred.Date[1037] is TRUE
df01 <- substr(df01$Occurred.Date[1037:nrow(df01)], 1, 10)
head(df01)
df01 <- gsub('/','', df01)
head(df01)

# year vector variable
year_vector <- as.character(c(2008:2018))

### data for each 
# in 2008
test_08 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2008'), 1, 2)))
data_2008 <- test_08$Freq
data_2008

# in 2009
test_09 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2009'), 1, 2)))
data_2009 <- test_09$Freq
data_2009

# in 2010
test_10 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2010'), 1, 2)))
data_2010 <- test_10$Freq
data_2010

# in 2011
test_11 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2011'), 1, 2)))
data_2011 <- test_11$Freq
data_2011

# in 2012
test_12 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2012'), 1, 2)))
data_2012 <- test_12$Freq
data_2012

# in 2013
test_13 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2013'), 1, 2)))
data_2013 <- test_13$Freq
data_2013

# in 2014
test_14 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2014'), 1, 2)))
data_2014 <- test_14$Freq
data_2014

# in 2015
test_15 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2015'), 1, 2)))
data_2015 <- test_15$Freq
data_2015

# in 2016
test_16 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2016'), 1, 2)))
data_2016 <- test_16$Freq
data_2016

# in 2017
test_17 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2017'), 1, 2)))
data_2017 <- test_08$Freq
data_2017

# in 2018
test_18 <- as.data.frame(
  table(
    substr(
      subset(df01, substr(df01, 5, 8) == '2018'), 1, 2)))
data_2018 <- test_18$Freq
data_2018

### data combined
df_combined <- data.frame(cbind(data_2008, data_2009, data_2010, data_2011, data_2012, data_2013, data_2014, data_2015, data_2016, data_2017, data_2018))
df_combined
colnames(df_combined) <- year_vector
df_combined$month <- c('01','02','03','04','05','06','07','08','09','10','11','12')

# make season <- subset by month
spring <- subset(df_combined, df_combined$month %in% c('03', '04', '05'))
summer <- subset(df_combined, df_combined$month %in% c('06', '07', '08'))
fall <- subset(df_combined, df_combined$month %in% c('09', '10', '11'))
winter <- subset(df_combined, df_combined$month %in% c('12', '01', '02'))

#####
# Frequency of Seasonal Crime
#####

# make data frame by season avg column
df_season_avg <- data.frame(
  spring = getSeasonalAvg(spring), 
  summer = getSeasonalAvg(summer), 
  fall = getSeasonalAvg(fall), 
  winter = getSeasonalAvg(winter))
rownames(df_season_avg) <- year_vector

# transpose data frame for melting
t_season_avg <- transDf(df_season_avg)
season <- rownames(t_season_avg)
t_season_avg$season <- season
t_season_avg

# melt data
m_season_avg <- melt(t_season_avg, id = 'season')
colnames(m_season_avg) <- c('season', 'year', 'freq')
m_season_avg

# set legend order
m_season_avg <- transform(m_season_avg, season = factor(season, levels = c('spring', 'summer', 'fall', 'winter')))

# draw season average plot
season_plot1 <- ggplot(m_season_avg, aes(x = year, y = freq, fill = season)) + geom_bar(stat='identity', position = 'dodge', aes(col = season, group = season)) + coord_cartesian(ylim = c(3000, 4500)) + labs(x = "Year", y = "Frequency", title = "Frequency of Seasonal Crime Occurance during the Last 10 years") + theme_minimal()
season_plot1

#####
# Proportion of Seasonal Crime
#####

# make data frame by season prop column
df_season_prop <- data.frame(
  spring = getSeasonalProp(spring, df_combined), 
  summer = getSeasonalProp(summer, df_combined), 
  fall = getSeasonalProp(fall, df_combined), 
  winter = getSeasonalProp(winter, df_combined))
rownames(df_season_prop) <- year_vector
df_season_prop

# tanspose season prop data
t_season_prop <- transDf(df_season_prop)
season <- rownames(t_season_prop)
t_season_prop$season <- season
t_season_prop

# melt data
m_season_prop <- melt(t_season_prop, id = 'season')
colnames(m_season_prop) <- c('season', 'year', 'prop')

# set legend order
m_season_prop <- transform(m_season_prop, season = factor(season, levels = c('spring', 'summer', 'fall', 'winter')))
m_season_prop

# draw season prop plot
season_plot2 <- ggplot(m_season_prop, aes(x = year, y = prop, fill = season)) + geom_bar(stat='identity', position = 'dodge', aes(col = season, group = season)) + coord_cartesian(ylim = c(20, 30)) + labs(x = "Year", y = "Percentage", title = "Percentage of Seasonal Crime Occurance during the Last 10 years") + theme_minimal()
season_plot2

#####
# Proportion of Seasonal Crime
#####

df_season_tot <- data.frame(
  spring = getSeasonalTot(spring, df_combined),
  summer = getSeasonalTot(summer, df_combined),
  fall = getSeasonalTot(fall, df_combined),
  winter = getSeasonalTot(winter, df_combined)
  )
df_season_tot
# spring summer  fall winter
# 1  24.64  25.52 25.78  24.06

t_season_tot <- transDf(df_season_tot)
t_season_tot$season <- season
t_season_tot

t_season_tot <- transform(t_season_tot, season = factor(season, levels = c('spring', 'summer', 'fall', 'winter')))

ggplot(data = t_season_tot, aes(x = "", y = V1, fill = season)) + geom_bar(stat = 'identity') + coord_polar(theta = 'y', direction = -1) + labs(x = "Year", y = "Proportion", title = "Proportion of Seasonal Average Crime Occurance during the Last 10 years") + theme_minimal()

#####
# Null hypothesis
## There is no difference in seasonal crime rates.
#####

resultChisq <- chisq.test(df_season_tot[1,])

# Chi-squared test for given probabilities
# 
# data:  unlist(df_season_tot[1, ])
# X-squared = 0.07568, df = 3, p-value = 0.9946

resultChisq$p.value > 0.05
# [1] TRUE

## Reject Null Hypothesis
### Therefore, There is a difference in seasonal crime rates.