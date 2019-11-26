library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)

### set work space
setwd('topic/Crime/')

### dataset
df <- read.csv('Seattle_Crime_Data.csv', header = T)
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
colnames(df_combined) <- c('2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')
df_combined$month <- c('01','02','03','04','05','06','07','08','09','10','11','12')

### making a line graph
df_channged <- melt(df_combined, id = 'month')
df_channged

graph01 <- ggplot(df_channged, aes(x = month, y = value)) +
  geom_line(aes(colour = variable, group = variable))  +
  labs(x = "Month", y = "Frequency", title = "The Number of Monthly Crime Occurrences per Each Year", color='Year') +
  dark_theme_grey()

### calculating average values & adding a new column
average <- c()
for(i in c(1:12)) {
  average <- c(average, round(mean(as.numeric(df_combined[i,-12])), 0))
}
average
df_combined$avg <- average
df_combined

### adding a new line on graph01 graph
graph02 <- graph01 + 
  geom_line(aes(x=month, avg, group=1), data=df_combined[,12:13], linetype='longdash', size = 1)
graph02
