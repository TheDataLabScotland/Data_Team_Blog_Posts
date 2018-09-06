library(dplyr)
library(data.table)
library(caret)
library(xlsx)
library(ranger)
library(MLmetrics)

bankHolidayList<-read.xlsx("data/bankHolidaysScotland.xlsx", sheetIndex = 1)

accidentData <- readRDS("data/accidentData.rds")%>%
  select(Accident_Severity, Date, Weather_Conditions, Local_Authority_.District., Day_of_Week,
         TimeSegment, Month)%>%
  filter(!is.na(TimeSegment))%>%
  mutate(Area=as.character(Local_Authority_.District.),
         Month=as.character(Month),
         Day_of_Week=as.character(Day_of_Week),
         Date=as.Date(Date),
         Weather=ifelse(Weather_Conditions %like% "Fine", "Fine", 
                        ifelse(Weather_Conditions %like% "Raining", "Raining",
                               ifelse(Weather_Conditions %like% "Snowing", "Snowing",
                                      ifelse(Weather_Conditions %like% "Fog", "Fog", "Other")))),
         Wind=ifelse(Weather_Conditions %like% "no high winds", "No High Winds", 
                     ifelse(Weather_Conditions %like% "high winds", "High Winds", "Other")),
         TimeSegment=as.numeric(gsub(":.*", "", TimeSegment)),
         TimeSegment=TimeSegment%/%6+1,
         Bank_Holiday=ifelse(Date %in% bankHolidayList$Date, "Yes", "No"))%>%
  group_by(Date, TimeSegment, Area)%>%
  summarise(Day_of_Week=names(which.max(table(Day_of_Week))),
            Month=names(which.max(table(Month))),
            Bank_Holiday=names(which.max(table(Bank_Holiday))),
            Weather=names(which.max(table(Weather))),
            Wind=names(which.max(table(Wind))),
            AccidentCount=n())

allPermutaions<-expand.grid(Date=unique(accidentData$Date), TimeSegment=unique(accidentData$TimeSegment),
                            Area=unique(accidentData$Area))%>%
  mutate(Area=as.character(Area))

finalDataset<-allPermutaions%>%left_join(accidentData, by=c("Date", "TimeSegment", "Area"))%>%
  mutate(Day_of_Week=weekdays(Date),
         Month=month.abb[as.numeric(format(as.Date(Date), "%m"))],
         Bank_Holiday=ifelse(Date %in% bankHolidayList$Date, "Yes", "No"),
         AccidentCount=ifelse(is.na(AccidentCount), 0, AccidentCount))%>%
  select(-Weather, -Wind, -Date)%>%
  mutate(AccidentCountNumeric=ifelse(AccidentCount>0, 1, 0),
         AccidentCount=ifelse(AccidentCount>0, "Yes", "No"))

#build the model

set.seed(123)

trainIndex<-createDataPartition(finalDataset$AccidentCount, p=0.8, list=FALSE)

train<-finalDataset[trainIndex, ]
test<-finalDataset[-trainIndex, ]

train_control <- trainControl(method="none", sampling = "down", classProbs = TRUE)

grid <- expand.grid(mtry=c(2), splitrule=c("gini"), min.node.size=c(3))

model <- caret::train(x=train[,1:5], y=train[,6],
               trControl=train_control, method="ranger",
               tuneGrid=grid
               )

preds <- predict(model, test[,1:5], type="prob")

#MAE(preds$predictions, test$AccidentCount)
LogLoss(preds[,2], test$AccidentCountNumeric)
LogLoss(preds$predictions[,2], test$AccidentCount)
#test$preds<-preds$predictions[,2]
test$preds<-preds[,2]

length(which(test$preds>=0.5 & test$AccidentCountNumeric==1))/length(which(test$AccidentCountNumeric==1))
length(which(test$preds<0.5 & test$AccidentCountNumeric==0))/length(which(test$AccidentCountNumeric==0))

