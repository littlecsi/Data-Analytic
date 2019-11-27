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
  select('Crime.Subcategory')
df01
aa <- as.character(levels(unlist(df01)))[unlist(df01)]
table(is.na(aa))
table(is.null(aa))
table(aa %in% "")

nrow(unique(df01))
table(df01)
