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
# crimePlot1

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
# neighPlot1

# Adding a y = 4000 line to the previous graph
neighPlot2 <- neighPlot1 + geom_hline(aes(yintercept=4000), color="Black", size=2) +
    annotate(geom="text", x=30, y=4300, label="Frequency = 4000", color="Black", size=5)
# neighPlot2

# Subsetting Neighborhoods(15, 46, 52, 42, 8, 49, 5, 57, 4) from cpData
neighF <- subset(neighF, Freq >= 4000)

neighborhoods <- c("DOWNTOWN COMMERCIAL", "QUEEN ANNE", "SLU/CASCADE", "NORTHGATE", "CAPITOL HILL", "ROOSEVELT/RAVENNA", "BELLTOWN", "UNIVERSITY", "BALLARD SOUTH")
neData <- subset(cpData, Neighborhood %in% neighborhoods)

# Remove "Occurred.Date" column
neData <- neData[,-1]
colnames(neData) <- c("oYear","oMonth","rDate","rTime","Subcategory","Description","Beat","Neighborhood")

# Divide "Reported.Date" column into Year and Month
rYM <- dateToYM(neData["rDate"], "rYear", "rMonth")
neData <- cbind(neData, rYM)

# Divide "Reported.Time" column into Hour and Minute
neData <- omit(neData)

rTimeInt <- neData$rTime
rTimeChar <- as.character(rTimeInt)

for(i in c(1:length(rTimeChar))) { # Making all elements length of 4
    if(nchar(rTimeChar[i]) == 3) {
        rTimeChar[i] <- paste("0", rTimeChar[i], sep="")
    } else if(nchar(rTimeChar[i]) == 2) {
        rTimeChar[i] <- paste("00", rTimeChar[i], sep="")
    } else if(rTimeChar[i] == 0) {
        rTimeChar[i] <- "0000"
    } else if(nchar(rTimeChar[i]) == 1) {
        rTimeChar[i] <- paste("000", rTimeChar[i], sep="")
    }
}

rHour <- substr(rTimeChar, 1, 2)
rMin <- substr(rTimeChar, 3, 4)

neData <- cbind(neData, rHour, rMin)

# Remove Factors
factors <- c("oMonth","rMonth","rHour")
for(f in factors) {
    neData[f] <- as.numeric(as.character(unlist(neData[f])))
}

# Reorder Data Frame and remove unnecessary columns
neData <- neData[c("oMonth","rMonth","rHour","Beat")]

##############################
### nnet package
# One-Hot Encoding
beat.ind <- class.ind(neData$Beat)
neData <- cbind(neData, beat.ind)

# Training and Testing Data
idx <- sample(c(1:nrow(neData)), 0.8*nrow(neData))
trainData <- neData[idx,]
testData <- neData[-idx,]

trainX <- trainData[,c(1:3)]; trainY <- trainData[,c(5:30)];
testX <- testData[,c(1:3)];

# Data Normalisation
trainX <- as.data.frame(lapply(trainX, normalize))
testX <- as.data.frame(lapply(testX, normalize))

# Create Model
nnModel <- nnet(x=trainX, y=trainY, decay=5e-04, maxit=200, MaxNWts=2000, trace=T, size=c(30,30, 30), softmax=T)

plot.nnet(nnModel)

nnPred <- predict(nnModel, testX, type="class")
nnPredTable <- table(nnPred, testData$Beat)
predAccuracy <- calcAcc(nnPredTable)
cat(predAccuracy, "% \n", sep="")