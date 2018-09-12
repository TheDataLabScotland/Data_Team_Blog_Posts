library(dplyr)
library(lubridate)
library(data.table)

accidentData<-read.csv("data/Accident_Information.csv")%>%filter(InScotland=="Yes")

vehicleData<-read.csv("data/Vehicle_Information.csv")



accidentData<-accidentData%>%
  select(Accident_Index, Accident_Severity, Date, Longitude, Latitude, Road_Surface_Conditions,
         Local_Authority_.District., Year, Day_of_Week, Time)%>%
  mutate(TimeSegment=as.POSIXct(as.character(Time), format = "%H:%M"))%>%
  mutate(TimeSegment=as.POSIXct(round(as.double(TimeSegment)/(60*60))*(60*60)+3600,origin=(as.POSIXct('1970-01-01'))))%>%
  mutate(TimeSegment=strftime(TimeSegment, format = "%H:%M"),
         Month=factor(month.abb[as.numeric(format(as.Date(Date), "%m"))], levels=month.abb))

levels(accidentData$Local_Authority_.District.) <- gsub("Edinburgh, City of", "Edinburgh", levels(accidentData$Local_Authority_.District.))

vehicleData<-vehicleData%>%filter(Accident_Index %in% accidentData$Accident_Index)%>%
  select(Accident_Index, Vehicle_Manoeuvre)%>%
  mutate(Vehicle_Manoeuvre=as.character(Vehicle_Manoeuvre))

#cleaning the journey purpose field
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "Changing lane"]<-"Changing lane"
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "other"]<-"Other"
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "bend"]<-"Road bend"
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "Overtaking"]<-"Overtaking"
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "Turning"]<-"Turning l/r"
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "Waiting"]<-"Waiting"
vehicleData$Vehicle_Manoeuvre[vehicleData$Vehicle_Manoeuvre %like% "Slowing"]<-"Slowing/Stopping"
  
saveRDS(accidentData, file = "data/accidentData.rds")

saveRDS(vehicleData, file = "data/vehicleData.rds")

