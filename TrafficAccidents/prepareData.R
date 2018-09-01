library(dplyr)
library(lubridate)

accidentData<-read.csv("data/Accident_Information.csv")%>%filter(InScotland=="Yes")

vehicleData<-read.csv("data/Vehicle_Information.csv")



accidentData<-accidentData%>%
  select(Accident_Index, Accident_Severity, Date, Light_Conditions, Number_of_Vehicles,
         Longitude, Latitude, Road_Surface_Conditions, Speed_limit, Weather_Conditions,
         Local_Authority_.District., Year, Day_of_Week, Time)%>%
  mutate(TimeSegment=as.POSIXct(as.character(Time), format = "%H:%M"))%>%
  mutate(TimeSegment=as.POSIXct(round(as.double(TimeSegment)/(60*60))*(60*60)+3600,origin=(as.POSIXct('1970-01-01'))))%>%
  mutate(TimeSegment=strftime(TimeSegment, format = "%H:%M"))


vehicleData<-vehicleData%>%filter(Accident_Index %in% accidentData$Accident_Index)%>%
  left_join(accidentData)
  
saveRDS(accidentData, file = "data/accidentData.rds")

saveRDS(vehicleData, file = "data/vehicleData.rds")

