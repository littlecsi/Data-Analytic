library(ggplot2)

source("script/_3_view/_001_preparation_script.R")

df01 <- income_data

df02 <- df01[c('YEAR', 'HOUSEHOLD_RACE', 'SECTOR')]
df02

sector <- sort(unique(df02$SECTOR))

sectorIncome <- data.frame()

for(s in sector) {
  df03 <- df02 %>% subset(SECTOR == s)
  total <- mean(df03$HOUSEHOLD_RACE)
  
  tmp <- data.frame(s, total)
  
  sectorIncome <- rbind(sectorIncome, tmp)
}
sectorIncome

options(scipen="999")
plot1 <- ggplot(data=sectorIncome, mapping=aes(x=s, y=total, col=s, fill=s)) +
  geom_col(show.legend=F) +
  geom_line(mapping=aes(col="Black", group=1), show.legend=F) +
  labs(title="Average Household Income by Sector", x="Sector", y="Average")
plot1
