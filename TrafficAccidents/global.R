library(dplyr)

accidentData <- readRDS("data/accidentData.rds")


severityList<-unique(accidentData$Accident_Severity)
lightList<-unique(accidentData$Light_Conditions)
vehicleNumberList<-unique(accidentData$Number_of_Vehicles)
roadList<-unique(accidentData$Road_Surface_Conditions)
speedList<-unique(accidentData$Speed_limit)
weatherList<-unique(accidentData$Weather_Conditions)
districtList<-unique(accidentData$Local_Authority_.District.)
makeList<-unique(accidentData$makes)
yearList<-unique(accidentData$Year)


allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )
