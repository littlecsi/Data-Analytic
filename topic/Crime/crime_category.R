library(dplyr)
library(ggplot2)
library(ggdark)
library(reshape2)

### set work space
setwd('topic/Crime/')

### dataset
df <- read.csv('Seattle_Crime_Data.csv', header = T)
head(df)
colnames(df)

df01 <- df %>% 
  select('Crime.Subcategory')
df01

nrow(unique(df01))
table(df01)
