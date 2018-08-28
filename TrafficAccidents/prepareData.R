library(dplyr)

accidentData<-read.csv("data/Accident_Information.csv")%>%filter(InScotland=="Yes")

vehicleData<-read.csv("data/Vehicle_Information.csv")



saveRDS(unique(as.character(makeList$make)), file = "data/makes.rds")

mergedData<-accidentData%>%left_join(vehicleData, by="Accident_Index")%>%
  group_by(Accident_Index)%>%
  mutate(makes=paste0(make, collapse = ""))%>%
  select(Accident_Index, Accident_Severity, Date, Light_Conditions, Number_of_Vehicles,
         Longitude, Latitude, Road_Surface_Conditions, Speed_limit, Weather_Conditions, makes,
         Local_Authority_.District., Year.x)%>%
  rename(Year=Year.x)%>%
  distinct(Accident_Index, .keep_all = TRUE)%>%
  ungroup()

vehicleData<-vehicleData%>%filter(Accident_Index %in% mergedData$Accident_Index)
  
saveRDS(mergedData, file = "data/accidentData.rds")

saveRDS(vehicleData, file = "data/vehicleData.rds")

