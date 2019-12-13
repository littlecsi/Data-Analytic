# ref lib
library(dplyr)
library(reshape2)
library(stringr)

source("script/_2_functions/functions.R")
source("script/_1_database/getDB.R")

# Print Edu Plot
df_Edu <- getEduData()

table(df_Edu$YEAR_N)
# 데이터 갯수
# 2012 2013 2014 2015 2016 
# 6759 6923 7084 7237 6950 

# base ggplot :: boxplot / bar / line
