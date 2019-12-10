source('database/getDB.R')

# Probability Function
## param : year
## param : Crime Type(T : Total, Pb : Prob, V : Violent, Pp : Property)
getTop10City <- function(year_vec, type) {
  pb <- progress_bar$new(format = "Inserting [:bar] :current/:total (:percent) elapsed::elapsedfull", total = length(year_vec), clear = F)
  
  for(i in year_vec) {
    data01 <- getFBIData(i)
    data01$Total <- data01$VIOLENT_CRIME + data01$PROPERTY_CRIME
    data01$Prob <- round(data01$Total / data01$POPULATION, 4) * 100
    
    if(type == 'T') {
      data01$Target <- data01$Total
      y_label <- 'Total Frequency'
      plot_main <- paste("Top 20 ", i, " Total Crimes Frequency in WA", sep = '')
    }
    else if(type == 'Pb') {
      data01$Target <- data01$Prob
      y_label <- 'Ratio'
      plot_main <- paste("Top 20 ", i, " Crimes compared to total population in WA", sep = '')
    }
    else if(type == 'V') {
      data01$Target <- round(data01$VIOLENT_CRIME / data01$Total, 4) * 100
      y_label <- 'Violent Crimes Ratio'
      plot_main <- paste("Top 20 ", i, " Violent Crimes Ratio in WA", sep = '')
    }
    else if(type == 'Pp') {
      data01$Target <- round(data01$PROPERTY_CRIME / data01$Total, 4) * 100
      y_label <- 'Property Crimes Ratio'
      plot_main <- paste("Top 20 ", i, " Property Crimes Ratio in WA", sep = '')
    }
    top10 <- data01 %>% arrange(desc(data01$Target)) %>% head(20)
    plot1 <- 
      ggplot(data = top10, 
             aes(x = CITY, y = Target, fill = factor(ifelse(
               CITY == 'Seattle', 'Normal', 'Highlighted')))) + 
      geom_bar(position = 'dodge', stat = 'identity') + 
      labs(x = "City", y = y_label, title = plot_main, color='YEAR') +
      theme_minimal(base_family = 'D2Coding') + 
      scale_x_discrete(limit = top10$CITY) +
      scale_fill_manual(name = 'CITY', values = c('grey50', 'red')) + 
      theme(legend.position = 'none')
    print(plot1)
    pb$tick()
  }
}

### Main

setYear <- c(2008:2018)

# Crime Type(T : Total, Pb : Prob, V : Violent, Pp : Property)
getTop10City(setYear, 'T')
# getTop10City(setYear, 'Pb')
# getTop10City(setYear, 'V')
# getTop10City(setYear, 'Pp')

