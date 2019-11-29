####################################################################################################
### !!! Warning !!!
### This File is NOT to be used by project
### ONLY to be used to test the functions.R file
####################################################################################################
source('script/functions.R')

setwd('topic/Crime/')

df <- read.csv('Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)
# str(df)
# colnames(df)
# nrow(df)

crimeCat <- getColumns(df, 2008, 2018, c('Crime.Subcategory'))
colnames(crimeCat)