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

df01 <- df %>% 
  select('Crime.Subcategory', 'Occurred.Date')
df01

df02 <- subset(df01, df01[,1] != "")
df02

length(df01$Crime.Subcategory)

table(df01$Crime.Subcategory)
