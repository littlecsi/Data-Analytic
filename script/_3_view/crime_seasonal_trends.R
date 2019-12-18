source("script/_3_view/_001_preparation_script.R")

### dataset
# OCC_DATE, OCC_TIME, REP_DATE, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c('OCC_DATE')
df01 <- cutColumns(col)

cut_ym <- cutMonthYear(df01)
cut_ym <- cut_ym[-1]

table_data <- as.data.frame(t(table(cut_ym)))
#     month year Freq
# 1      01 2008 3322
# 2      02 2008 3124
# 3      03 2008 3367
# 4      04 2008 3327
# 5      05 2008 3637
# 6      06 2008 3615

# year vector variable
year_vector <- as.character(c(2008:2018))

#####
# Frequency of Seasonal Crime
#####

# make season <- subset by month
spring <- subset(table_data, table_data$month %in% c('03', '04', '05'))
summer <- subset(table_data, table_data$month %in% c('06', '07', '08'))
fall <- subset(table_data, table_data$month %in% c('09', '10', '11'))
winter <- subset(table_data, table_data$month %in% c('12', '01', '02'))

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
## coord_cartesian : set x, y limit. but not omit, just enlarge
season_plot1 <- ggplot(m_season_avg, aes(x = year, y = freq, fill = season)) + 
  geom_bar(stat='identity', position = 'dodge', aes(col = season, group = season)) + 
  coord_cartesian(ylim = c(3000, 4500)) + 
  geom_text(aes(label = freq), vjust=-0.5, position = position_dodge2(width = 1)) + 
  labs(x = "Year", y = "Frequency", title = "Frequency of Seasonal Crime Occurance during the Last 10 years") + 
  theme_minimal(base_size = 12)
season_plot1

#####
# Proportion of Seasonal Crime
#####

# make data frame by season prop column
df_season_prop <- data.frame(
  spring = getSeasonalProp(spring, table_data), 
  summer = getSeasonalProp(summer, table_data), 
  fall = getSeasonalProp(fall, table_data), 
  winter = getSeasonalProp(winter, table_data))
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
season_plot2 <- ggplot(m_season_prop, aes(x = year, y = prop, fill = season)) + 
  geom_bar(stat='identity', position = 'dodge', aes(col = season, group = season)) + 
  coord_cartesian(ylim = c(20, 30)) + 
  geom_text(aes(label = prop), vjust=-0.5, position = position_dodge2(width = 1)) + 
  labs(x = "Year", y = "Percentage", title = "Percentage of Seasonal Crime Occurance during the Last 10 years") + 
  theme_minimal(base_size = 12)
season_plot2

#####
# Proportion of Seasonal Crime
#####

df_season_tot <- data.frame(
  spring = getSeasonalTot(spring, table_data),
  summer = getSeasonalTot(summer, table_data),
  fall = getSeasonalTot(fall, table_data),
  winter = getSeasonalTot(winter, table_data)
  )
df_season_tot
# spring summer  fall winter
# 1  24.64  25.52 25.78  24.06

t_season_tot <- transDf(df_season_tot)
t_season_tot$season <- season
t_season_tot

# set legend order
t_season_tot <- transform(t_season_tot, season = factor(season, levels = c('spring', 'summer', 'fall', 'winter')))

# Draw pie plot
## coord_polar : draw pie graph
### start : start degree
### direction : default is clockwise, -1 is counterclockwise
ggplot(data = t_season_tot, aes(x = "", y = V1, fill = season)) + 
  geom_bar(stat = 'identity') + 
  coord_polar(theta = 'y', start = 0, direction = -1) + 
  geom_text(aes(label = paste(V1,'%')), position = position_stack(vjust = 0.5)) + 
  labs(x = "Year", y = "Proportion", title = "Proportion of Seasonal Average Crime Occurance during the Last 10 years") + 
  theme_minimal(base_size = 12)

#####
# Null hypothesis
## There is no difference in seasonal crime rates.
#####

resultChisq <- chisq.test(df_season_tot[1,])
resultChisq

# Chi-squared test for given probabilities
# 
# data:  df_season_tot[1, ]
# X-squared = 0.059192, df = 3, p-value = 0.9962

resultChisq$p.value > 0.05
# [1] TRUE

## Reject Null Hypothesis
### Therefore, There is a difference in seasonal crime rates.

dbDisconnectAll()