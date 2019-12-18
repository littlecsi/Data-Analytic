source("script/_3_view/_001_preparation_script.R")

### dataset
# OCC_DATE, OCC_TIME, REP_DATE, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c('OCC_DATE')
df01 <- cutColumns(col)

df_combined <- cutMonthYear(df01)

### making a line graph
df_changed <- as.data.frame(table(df_combined[,-1]))

graph01 <- ggplot(df_changed, aes(x = month, y = Freq)) +
  geom_line(aes(color = year, group = year))  +
  labs(x = "Month", y = "Frequency", title = "The Number of Monthly Crime Occurrences per Each Year", color='Year') + dark_theme_grey()

### calculating average values & adding a new column
mx_table <- as.matrix(table(df_combined[,-1]))
ncol(mx_table)
average <- c()
for(i in c(1:12)) {
  average <- c(average, round(mean(as.numeric(mx_table[,i])), 0))
}

avg <- as.data.frame(average)
avg <- cbind(avg, month = unique(df_combined$month))

### adding a new line on graph01
graph02 <- graph01 + 
  geom_line(aes(x = month, y = average, group=1), data=avg, linetype='longdash', size = 1, inherit.aes = FALSE)
graph02

dbDisconnectAll()