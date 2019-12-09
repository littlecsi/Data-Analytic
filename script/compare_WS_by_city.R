source('database/getDB.R')

# Probability Function
getTop10City <- function(year_vec) {
  for(i in year_vec) {
    data01 <- getFBIData(i)
    data01 <- data01[, c(1, 2, 3, 4, 9)]
    data01$Total <- data01$VIOLENT_CRIME + data01$PROPERTY_CRIME
    data01$Prob <- round(data01$Total / data01$POPULATION, 4) * 100
    
    top10 <- data01 %>% arrange(desc(data01$Prob)) %>% head(20)
    print(top10)
    plot_main <- paste(i, "년 전체 인구수 대비 범죄율 Top 10", sep = '')
    plot1 <- ggplot(data = top10, aes(x = CITY, y = Prob, fill = factor(ifelse(CITY == 'Seattle', 'Normal', 'Highlighted')))) + 
      geom_bar(position = 'dodge', stat = 'identity') + 
      labs(x = "City", y = "Probability", title = plot_main, color='YEAR') +
      theme_minimal(base_family = 'D2Coding') + scale_x_discrete(limit = top10$CITY) +
      scale_fill_manual(name = 'CITY', values = c('grey50', 'red')) + theme(legend.position = 'none')
    print(plot1)
  }
}

### Main

setYear <- c(2014:2018)
getTop10City(setYear)
