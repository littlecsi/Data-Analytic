####################################################################################################
### Library
library(ggplot2)

####################################################################################################
### Import
source("script/functions/functions.R")
source("script/functions/mlfunctions.R")

####################################################################################################
### Importing Data
df <- read.csv('dataset/Crime/Seattle_Crime_Data.csv', header = T, stringsAsFactors = F)
colnames(df)

# Get required Columns
reqCol <- c("Reported.Date","Reported.Time","Crime.Subcategory","Primary.Offense.Description", "Beat", "Neighborhood")
cdf <- getColumns(df, 2008, 2018, reqCol)

# Finding the Frequency of each crime category
crimeF <- table(cdf$Crime.Subcategory)
crimeF <- as.data.frame(crimeF)
crimeF <- crimeF[order(crimeF$Freq, decreasing=T),]

# Plotting Crime Frequency
crimePlot1 <- ggplot() + 
    geom_col(aes(Var1, Freq, fill=Var1), crimeF, position="stack", show.legend=F) +
    labs(x="Crime Subcategory", y="Frequency", title="Frequecy of Crime by Frequency between 2008 and 2018") +
    theme(axis.line=element_line(color="Black", size=1, linetype="solid")) +
    scale_x_discrete(labels=c(1:nrow(crimeF)))
crimePlot1

##################################################
### Car Prowl Prediction
# Data Extraction
cpData <- subset(cdf, Crime.Subcategory == "CAR PROWL")
nrow(cpData) # 144,327

# Find the Frequency of Crime Neighborhood
neighF <- table(cpData$Neighborhood)
neighF <- as.data.frame(neighF)
neighF <- neighF[order(neighF$Freq, decreasing=T),]

# Plotting Frequency of Crime Neighborhood of Car Prowl subcategory
neighPlot1 <- ggplot() +
    geom_col(aes(Var1, Freq, fill=Var1), neighF, position="stack", show.legend=F) +
    labs(x="Crime Neighborhood", y="Frequency", title="Frequency of Crime by Neighborhood between 2008 and 2018") +
    theme(axis.line=element_line(color="Black", size=1, linetype="solid")) +
    scale_x_discrete(labels=c(1:nrow(neighF)))
neighPlot1

# Adding a y = 4000 line to the previous graph
neighPlot2 <- neighPlot1 + geom_hline(aes(yintercept=4000), color="Black", size=2) +
    annotate(geom="text", x=30, y=4300, label="Frequency = 4000", color="Black", size=5)
neighPlot2

# Subsetting Neighborhoods(15, 46, 52, 42, 8, 49, 5, 57, 4) from cpData
neighF <- subset(neighF, Freq >= 4000)
neighborhoods <- c("DOWNTOWN COMMERCIAL", "QUEEN ANNE", "SLU/CASCADE", "NORTHGATE", "CAPITOL HILL", "ROOSEVELT/RAVENNA", "BELLTOWN", "UNIVERSITY", "BALLARD SOUTH")
neData <- subset(cpData, Neighborhood %in% neighborhoods)

# Remove "Occurred.Date" column
neData <- neData[,-1]
colnames(neData) <- c("oYear","oMonth","rDate","rTime","Subcategory","Description","Beat","Neighborhood")

rYM <- dateTOYM(neData["rDate"], "rYear", "rMonth")
neData <- cbind(neData, rYM)