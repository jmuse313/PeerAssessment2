setwd("~/Documents/Coursera/5_ReproducibleResearch/PeerAssessment2")

#Download the zipped file to a temporary location, load the data
temp <- tempfile()
link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(link, temp, method = "curl")
data <- read.csv(bzfile(temp))
unlink(temp)

#Create a local copy of data
write.csv(data, file = "StormData.csv", row.names = FALSE) 
data <- read.csv("StormData.csv")

#Libraries used:
library(lattice)
library(stringdist)
library(maps)
library(mapdata)
library(scales)

#Data Processing

#Create a vector of Event Types as they are classified in the National Weather Service Documentation
EventTypes <- read.csv("EventTypesNWS.csv", header = FALSE)
EventTypes <- toupper(EventTypes$V1)

#Subset DF to columns of intrest
ColsInt <- c("STATE", "EVTYPE", "MAG", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", 
             "CROPDMGEXP", "LATITUDE", "LONGITUDE")
SubData <- data[,ColsInt]

#Review unique EVTYPE
UniqueEvts <- unique(SubData$EVTYPE)

##This section processes the data based on the initial visual inspection of the unique EVTYPE values
#everything to uppercase
SubData$EVTYPE <- toupper(SubData$EVTYPE)

#Replace "\" with "/"
SubData$EVTYPE <- gsub("\\\\", "/", SubData$EVTYPE)

#Remove Parens and periods
SubData$EVTYPE <- gsub("\\(", "", SubData$EVTYPE)
SubData$EVTYPE <- gsub("\\)", "", SubData$EVTYPE)
SubData$EVTYPE <- gsub("\\.", "", SubData$EVTYPE)

#Remove MPH
SubData$EVTYPE <- gsub("MPH", "", SubData$EVTYPE)

#Remove all numbers
SubData$EVTYPE <- gsub("[0-9]", "", SubData$EVTYPE)

#TSTM to Thunderstorm 
SubData$EVTYPE <- gsub("TSTM", "THUNDERSTORM", SubData$EVTYPE)

#Rain and Snow... Rain to "Heavy Rain".  Snow to "Heavy Snow"
SubData$EVTYPE[SubData$EVTYPE=="RAIN"]<-"HEAVY RAIN"
SubData$EVTYPE[SubData$EVTYPE=="SNOW"]<-"HEAVY SNOW"

#Anycase of Microburst to Thunderstorm Winds
microburst <- function(evtypes) {
    tgtList <- unique((grep("MICROBURST", evtypes, value = TRUE)))
    for (i in 1:length(tgtList)) {
        evtypes[(evtypes == tgtList[i])] <- "THUNDERSTORM WINDS"
    }
    evtypes
}
SubData$EVTYPE <- microburst(SubData$EVTYPE)

#For anywhere "TORNADO" is part of Event Type, change to just "TORNADO" 
tornado <- function(evtypes) {
    tgtList <- unique(grep("TORNADO", evtypes, value = TRUE))
    for (i in 1:length(tgtList)) {
        evtypes[(evtypes == tgtList[i])] <- "TORNADO"
    }
    evtypes
}
SubData$EVTYPE <- tornado(SubData$EVTYPE)

#Hurricane or Typhoon to "Hurricane/Typhoon" do nothing
hurricane <- function(evtypes) {
    tgtList <- unique(grep("HURRICANE|TYPHOON", evtypes, value = TRUE))
    for (i in 1:length(tgtList)) {
        evtypes[(evtypes == tgtList[i])] <- "HURRICANE/TYPHOON"
    }
    evtypes
}
SubData$EVTYPE <- hurricane(SubData$EVTYPE)

#This section loops through each unique event type in the data and uses stringdist to compare them
#to the event types listed in the NOAA's NWS Documentation (found at http://www.ncdc.noaa.gov/
#stormevents/pd01016005curr.pdf).  The Optimal String Alignment distance (osa) counts the number of 
#deletions, insertions and substitutions necessary to turn b into a, and also allows transposition 
#of adjacent characters. Each substring may be edited only once. (For example, a character cannot 
#be transposed twice to move it forward in the string).

UniqueEvt <- unique(SubData$EVTYPE)
UnqEVtype <- c()
TxtDist <- c()
NewEvt <- c() 

for (i in 1:length(UniqueEvt)) {
    Distances <- stringdist(UniqueEvt[i], EventTypes)
    minDist <- min(Distances)
    minEventType <- EventTypes[which.min(Distances)]
    UnqEVtype <- c(UnqEVtype, UniqueEvt[i])
    TxtDist <- c(TxtDist, minDist)
    NewEvt <- c(NewEvt, minEventType)
}

UnqEvts <- data.frame(EVTYPE = UnqEVtype, TXTDIST = TxtDist, NEWEVT = NewEvt, 
                      stringsAsFactors = FALSE)
DiffEvts3up <- UnqEvts[(UnqEvts$TXTDIST > 2),]

#Count the number of Event types in the SubData that have a stringdist of 3 or more.
sum(SubData$EVTYPE %in% DiffEvts3up$EVTYPE) #16155


#Checking the frequencies of the remaining unique values that have a stringdist greater than 2
UnqFreq <- as.data.frame(table(SubData$EVTYPE))
UnqFreq <- UnqFreq[(UnqFreq$Var1 %in% DiffEvts3up$EVTYPE),]
UnqFreq <- UnqFreq[order(UnqFreq$Freq, decreasing = TRUE),]
head(UnqFreq, n=10)

#Top Ten based on the freqencies: URBAN/SML STREAM FLD (3392), WILD/FOREST FIRE (1457), WINTER 
#WEATHER/MIX (1104), THUNDERSTORM WIND/HAIL (1029), FLASH FLOODING (682), EXTREME COLD (657), FLOOD/
#FLASH FLOOD (625), LANDSLIDE (600), FOG (538), WIND (346) -- Manually edit these where appropriate

#"Heavy rain situations, resulting in urban and/or small stream flooding, should be classified as a 
#Heavy Rain event.." according to the NWS documentation
SubData[(SubData$EVTYPE == "URBAN/SML STREAM FLD"),]$EVTYPE <- "HEAVY RAIN"

#"Any significant forest fire, grassland fire, rangeland fire, or wildland-urban interface fire..." 
#according to the NWS documentation this should be "WILDFIRE"
SubData[(SubData$EVTYPE == "WILD/FOREST FIRE"),]$EVTYPE <- "WILDFIRE"

#"A Winter Weather event could result from one or more winter precipitation types (snow, or blowing/
#drifting snow, or freezing rain/drizzle), on a widespread or localized basis ..." 
#according to the NWS documentation this should be "WINTER WEATHER"
SubData[(SubData$EVTYPE == "WINTER WEATHER/MIX"),]$EVTYPE <- "WINTER WEATHER"

#THUNDERSTORM WIND/HAIL -- It is not clear whether this should be "THUNDERSTORM WIND" or "HAIL", so
#These will be dropped.
SubData <- SubData[!(SubData$EVTYPE == "THUNDERSTORM WIND/HAIL"),] #1029 rows

#FLASH FLOODING to "FLASH FLOOD"
SubData[(SubData$EVTYPE == "FLASH FLOODING"),]$EVTYPE <- "FLASH FLOOD"

#EXTREME COLD to "EXTREME COLD/WIND CHILL"
SubData[(SubData$EVTYPE == "EXTREME COLD"),]$EVTYPE <- "EXTREME COLD/WIND CHILL"

#FLOOD/FLASH FLOODING to "FLASH FLOOD"
SubData[(SubData$EVTYPE == "FLOOD/FLASH FLOODING"),]$EVTYPE <- "FLASH FLOOD"

#LANDSLIDE should be renamed to "DEBRIS FLOW"
SubData[(SubData$EVTYPE == "LANDSLIDE"),]$EVTYPE <- "DEBRIS FLOW"
SubData$EVTYPE <- gsub("LANDSLIDE", "DEBRIS FLOW", SubData$EVTYPE)

#FOG to DENSE FOG
SubData[(SubData$EVTYPE == "FOG"),]$EVTYPE <- "DENSE FOG"

#WIND to STRONG WIND or HIGH WIND based on MAG value from dataset - those with MAG values
#greater than 50 assigned to "HIGH WIND", those with values less than 50 assigned to STRONG WIND
SubData[(SubData$EVTYPE == "WIND" & SubData$MAG > 50),]$EVTYPE <- "HIGH WIND"
SubData[(SubData$EVTYPE == "WIND" & SubData$MAG <= 50),]$EVTYPE <- "STRONG WIND"



#Count the number of observations in SubData at this point that have a stringdist of three or more

#Re-calculate the string distances

UniqueEvt <- unique(SubData$EVTYPE)
UnqEVtype <- c()
TxtDist <- c()
NewEvt <- c() 

for (i in 1:length(UniqueEvt)) {
    Distances <- stringdist(UniqueEvt[i], EventTypes)
    minDist <- min(Distances)
    minEventType <- EventTypes[which.min(Distances)]
    UnqEVtype <- c(UnqEVtype, UniqueEvt[i])
    TxtDist <- c(TxtDist, minDist)
    NewEvt <- c(NewEvt, minEventType)
}

UnqEvts <- data.frame(EVTYPE = UnqEVtype, TXTDIST = TxtDist, NEWEVT = NewEvt, 
                      stringsAsFactors = FALSE)
DiffEvts3up <- UnqEvts[(UnqEvts$TXTDIST > 2),]

#off by less than three -- Visual inspection confirms that all of these are matched to the correct
#event from the NWS Event Type vector.  Update EVTYPES in Subdata
OffByUnder3 <- UnqEvts[(UnqEvts$TXTDIST < 3),]

for (i in 1:nrow(OffByUnder3)) {
    SubData[(SubData$EVTYPE == OffByUnder3$EVTYPE[i]),]$EVTYPE <- OffByUnder3$NEWEVT[i]
}

#Count the number of Event types in the SubData that have a stringdist of 3 or more.
sum(SubData$EVTYPE %in% DiffEvts3up$EVTYPE) #6340

#6340 out of the 901268 total observations still in the dataset is 0.7%.  This small percentage of 
#observations will be dropped from the dataset as cleaning them will be excessively time consuming
SubData <- SubData[(!(SubData$EVTYPE %in% DiffEvts3up$EVTYPE)),]

#The total number of observations dropped is 7369.  This was less than 1% of the original dataset.

UnqFreq <- as.data.frame(table(SubData$EVTYPE))

#Questions: 

    # 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are
        #most harmful with respect to population health? Goal: sum the number of fatalities and 
        #the number of injuries for each event type across all states

Fatalities <- aggregate(FATALITIES~EVTYPE, data = SubData, sum)
Fatalities <- Fatalities[order(Fatalities$FATALITIES, decreasing = TRUE),]

FatalState <- aggregate(FATALITIES~STATE + EVTYPE, data = SubData, sum)
FatalStateDropZeros <- FatalState[!(FatalState$FATALITIES == 0),]
FatalStateDropZeros <- FatalStateDropZeros[order(FatalStateDropZeros$STATE),]

UnqFreq <- UnqFreq[order(UnqFreq$Freq, decreasing = TRUE),]
                             
Injuries <- aggregate(INJURIES~EVTYPE, data = SubData, sum)
InjState <- aggregate(INJURIES~STATE + EVTYPE, data = SubData, sum)

Fatalities2 <- Fatalities[!(Fatalities$FATALITIES == 0),]
Injuries2 <- Injuries[!(Injuries$INJURIES == 0),]


    # 2. Across the United States, which types of events have the greatest economic consequences?
