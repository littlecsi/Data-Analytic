source('database/getDB.R')
### Main

X2008 <- getFBIData(c(2008))

str(X2008)

data01 <- X2008[, c(2, 3, 4, 9)]
data01$Total <- data01$VIOLENT_CRIME + data01$PROPERTY_CRIME
data01$Prob <- round(data01$Total / data01$POPULATION, 4) * 100

top20 <- data01 %>% arrange(desc(data01$Prob)) %>% head(20)

ggplot(data = data01, aes(x = CITY, y = Prob)) + geom_bar(stat = 'identity', position = 'dodge') + scale_x_discrete(limit = top20)
