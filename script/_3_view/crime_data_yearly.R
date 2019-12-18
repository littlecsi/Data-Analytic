source("script/_3_view/_001_preparation_script.R")

### dataset
# OCC_DATE, OCC_TIME, REP_DATE, SUB_CATE, PRI_DESC, PRECINCT, SECTOR, BEAT, NEIGHBOR
col <- c('OCC_DATE')
df01 <- cutColumns(col)

### substring years
year <- substr(df01[,1], 7, 10)
head(year)
# [1] "1908" "1964" "1973" "1974" "1975" "1975"

### changing data form
tb_year <- table(year)
head(tb_year)
# year
# 1908 1964 1973 1974 1975 
# 2    1    1    1    1    2

### data framing
df_year <- as.data.frame(tb_year)
head(df_year)
# year Freq
# 1         2
# 2 1908    1
# 3 1964    1
# 4 1973    1
# 5 1974    1
# 6 1975    2

# extracting specific years(2008~2018)
df <- df_year[-12,]
mean_occr <- mean(df$Freq)
mean_occr
# [1] 46084.45

regFreq <- as.numeric(df$Freq)
regYear <- as.numeric(df$year)

reg <- lm(formula = regFreq ~ regYear)
summary(reg)
# Call:
#   lm(formula = regFreq ~ regYear)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4146.9  -393.0   343.8   963.0  2693.8 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   8861.5     8190.8   1.082  0.30744   
# regYear        930.6      204.1   4.559  0.00137 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2141 on 9 degrees of freedom
# Multiple R-squared:  0.6978,	Adjusted R-squared:  0.6642 
# F-statistic: 20.78 on 1 and 9 DF,  p-value: 0.001369

# making a graphic chart
ggplot(data = df, aes(x = year, y = Freq, group = 1)) + 
  geom_line(linetype = "dotted", color = "red", size = 2) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Year", y = "Frequency", title = "The Number of Crime Occurrences per Each Year during the Last 10 years") +
  dark_theme_gray() +
  # geom_hline(yintercept = mean_occr, color = "orange", size = 1) +
  geom_text(aes(label = Freq), vjust = -0.3) +
  stat_smooth(method = 'lm', col = 'orange')

dbDisconnectAll()
