library(dplyr)

accidentData <- readRDS("data/accidentData.rds")

vehicleData <- readRDS("data/vehicleData.rds")

wseverityList<-unique(accidentData$Accident_Severity)
lightList<-unique(accidentData$Light_Conditions)
vehicleNumberList<-unique(accidentData$Number_of_Vehicles)
roadList<-unique(accidentData$Road_Surface_Conditions)
speedList<-unique(accidentData$Speed_limit)
weatherList<-unique(accidentData$Weather_Conditions)
districtList<-unique(as.character(accidentData$Local_Authority_.District.))
makeList<-unique(vehicleData$make)
yearList<-unique(accidentData$Year)
