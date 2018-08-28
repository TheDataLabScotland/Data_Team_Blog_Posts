library(dplyr)

accidentData<-read.csv("data/Accident_Information.csv")%>%filter(InScotland=="Yes")

vehicleData<-read.csv("data/Vehicle_Information.csv")

mergedData<-accidentData%>%left_join(vehicleData, by="Accident_Index")%>%
  group_by(Accident_Index)%>%
  mutate(makes=paste0(make, collapse = ""))%>%
  select(Accident_Index, Accident_Severity, Date, Light_Conditions, Number_of_Vehicles,
         Longitude, Latitude, Road_Surface_Conditions, Speed_limit, Weather_Conditions, makes,
         Local_Authority_.District., Year.x)%>%
  rename(Year=Year.x)%>%
  distinct(Accident_Index, .keep_all = TRUE)%>%
  ungroup()
  
saveRDS(mergedData, file = "data/accidentData.rds")
