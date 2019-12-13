# ref lib
library(dplyr)
library(reshape2)
library(stringr)

source("script/_2_functions/functions.R")
source("script/_1_database/getDB.R")

# Print Edu Plot
df_Edu <- getEduData()

# Create DF
df_Edu_YearComp <- cbind(df_Edu$YEAR_N, df_Edu$COMPLETIONS) %>% as.data.frame(header = T)
df_Edu_YearComp <- df_Edu_YearComp %>% as.factor(df_Edu_YearComp$V1)

# table(df_Edu)
colnames(df_Edu_YearComp) <- c("Year", "Completions")
# 데이터 갯수
# 2012 2013 2014 2015 2016 
# 6759 6923 7084 7237 6950 

# base ggplot :: boxplot / bar / line
ggplot(data = df_Edu_YearComp, mapping = aes(x = Year, y = Compl, col = , fill = )) + geom_bar(stat = "identity")
