library(rvest)
library(dplyr)
library(stringr)

# 2016 Washington state cities by population
url <- 'https://www.washington-demographics.com/cities_by_population'
data01 <- read_html(url) %>% html_node('.ranklist ') %>% html_table(fill=T)
df <- as.data.frame(data01)
str(df$Population)
df <- df[-583,]
pop <- str_replace_all(df$Population, ',', '')
df$Population <- as.numeric(pop)
str(df)

# get original data
data02 <- read.csv('dataset/Crime/FBI_Offenses/FBI_Offenses_2016.csv')
data02
tmpCrime <- data02[,-c(1:2)]
data02 <- data02[,c(1:2)]
data02
