library(dplyr)
library(ggplot2)
library(ggdark)

### set work space
setwd('topic/Crime/')

viewClassNMode <- function(data) {
  cat('class:', class(data))
  cat('\n','mode:', mode(data))
}

### dataset
df <- read.csv('Seattle_Crime_Data.csv', header = T)
head(df)

### sorting dataset to extract 'Occurred.Date' column
df01 <- df %>% select('Occurred.Date')
head(df01)

### substring years
year <- substr(df01$Occurred.Date, 7, 10)
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
df <- df_year[35:45,]
mean_occr <- mean(df$Freq)
mean_occr
# [1] 46084.45

regFreq <- as.numeric(df$Freq)

viewClassNMode(regFreq)

reg <- lm(df$Freq ~ df$year)
summary(reg)

# making a graphic chart
ggplot(data = df, aes(x = year, y = Freq, group = 1)) + 
  geom_line(linetype = "dotted", color = "red", size = 2) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Year", y = "Frequency", title = "The Number of Crime Occurrences per Each Year during the Last 10 years") +
  dark_theme_gray() +
  geom_hline(yintercept = mean_occr, color = "orange", size = 1) +
  geom_text(aes(label = Freq), vjust = -0.3) +
  geom_abline(reg)

