library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(scales)
library(lattice)
library(dplyr)
library(scales)



function(input, output, session) {
  
  # preparing reactive datasets
  
  accidents1<-reactive({
    accidentData%>%filter(
                            Local_Authority_.District. %in% input$district &
                            Year %in% input$year &
                            Day_of_Week %in% input$day)
  })
  
  accidents2<-reactive({
    accidentData%>%filter(
                            Local_Authority_.District. %in% input$district2 &
                            Year %in% input$year2 &
                            Day_of_Week %in% input$day2)
  })
  
  vehicles1<-reactive({
    vehicleData%>%filter(Accident_Index %in% accidents1()$Accident_Index)
  })
  
  vehicles2<-reactive({
    vehicleData%>%filter(Accident_Index %in% accidents2()$Accident_Index)
  })
  
  riskPredictionData<-reactive({ 
    
    stepsAhead<-60*60*6*c(0:7)
    timeSteps<-Sys.time()+stepsAhead
    
    dataset<-expand.grid(datetime=timeSteps, Area=c(input$district, input$district2))%>%
      mutate(Area=as.character(Area),
             Day_of_Week=weekdays(datetime),
             Month=month.abb[as.numeric(format(datetime, "%m"))],
             Bank_Holiday=ifelse(datetime %in% bankHolidayList$Date, "Yes", "No"),
             TimeSegment=as.numeric(gsub(":.*", "", as.character(strftime(datetime, format="%H:%M"))))%/%6+1,
             datetime=NULL)
    # this produces a dataset that looks like this:
    #   Area Day_of_Week Month Bank_Holiday TimeSegment
    # 1  Edinburgh, City of      Friday   Sep           No           3
    # 2  Edinburgh, City of      Friday   Sep           No           4
    # 3  Edinburgh, City of    Saturday   Sep           No           1
    # 4  Edinburgh, City of    Saturday   Sep           No           2
    # 5  Edinburgh, City of    Saturday   Sep           No           3
    # 6  Edinburgh, City of    Saturday   Sep           No           4
    # 7  Edinburgh, City of      Sunday   Sep           No           1
    # 8  Edinburgh, City of      Sunday   Sep           No           2
    # 9        Glasgow City      Friday   Sep           No           3
    # 10       Glasgow City      Friday   Sep           No           4
    # 11       Glasgow City    Saturday   Sep           No           1
    # 12       Glasgow City    Saturday   Sep           No           2
    # 13       Glasgow City    Saturday   Sep           No           3
    # 14       Glasgow City    Saturday   Sep           No           4
    # 15       Glasgow City      Sunday   Sep           No           1
    # 16       Glasgow City      Sunday   Sep           No           2
  })
  
  # produce predictions reactively
  
  riskPredictions<-reactive({
    
    preds <- predict(riskModel, riskPredictionData(), type="prob")
    
    
    riskPredictions<-data.frame(Area=riskPredictionData()["Area"],
                                TimeSegment=riskPredictionData()["TimeSegment"],
                                Day_of_Week=riskPredictionData()["Day_of_Week"],
                                predictions=preds[,2])%>%
      mutate(xLabels=paste(Day_of_Week, hourList[TimeSegment]))
    
    # this outputs something like this (predictions being the model's risk predction):
    #   Area TimeSegment Day_of_Week predictions           xLabels
    # 1  Edinburgh, City of           3      Friday   0.6520889   Friday 12pm-6pm
    # 2  Edinburgh, City of           4      Friday   0.5743273   Friday 6pm-12am
    # 3  Edinburgh, City of           1    Saturday   0.2041248 Saturday 12am-6am
    # 4  Edinburgh, City of           2    Saturday   0.5464998 Saturday 6am-12pm
    # 5  Edinburgh, City of           3    Saturday   0.6156699 Saturday 12pm-6pm
    # 6  Edinburgh, City of           4    Saturday   0.5196448 Saturday 6pm-12am
    # 7  Edinburgh, City of           1      Sunday   0.4065399   Sunday 12am-6am
    # 8  Edinburgh, City of           2      Sunday   0.4882825   Sunday 6am-12pm
    # 9        Glasgow City           3      Friday   0.6763118   Friday 12pm-6pm
    # 10       Glasgow City           4      Friday   0.6234673   Friday 6pm-12am
    # 11       Glasgow City           1    Saturday   0.2390006 Saturday 12am-6am
    # 12       Glasgow City           2    Saturday   0.6203835 Saturday 6am-12pm
    # 13       Glasgow City           3    Saturday   0.7017921 Saturday 12pm-6pm
    # 14       Glasgow City           4    Saturday   0.5589943 Saturday 6pm-12am
    # 15       Glasgow City           1      Sunday   0.4143968   Sunday 12am-6am
    # 16       Glasgow City           2      Sunday   0.5637810   Sunday 6am-12pm
  })
  
  # reactive graph labeling
  
  labels1<-reactive({
    if(length(input$year)==1)
    {
      years<-as.character(input$year)
    } else
    {
      yearListLength<-length(input$year)
      years<-paste0(input$year[1], " - ", input$year[yearListLength])
    }
    
    if(length(input$day)==1)
    {
      days<-as.character(input$day)
    } else
    {
      dayListLength<-length(input$day)
      days<-paste0(input$day[1], " - ", input$day[dayListLength])
    }
    
    c(input$district, years, days)
    
  })
  
  labels2<-reactive({
    if(length(input$year2)==1)
    {
      years<-as.character(input$year2)
    } else
    {
      yearListLength<-length(input$year2)
      years<-paste0(input$year2[1], " - ", input$year2[yearListLength])
    }
    
    if(length(input$day2)==1)
    {
      days<-as.character(input$day2)
    } else
    {
      dayListLength<-length(input$day2)
      days<-paste0(input$day2[1], " - ", input$day2[dayListLength])
    }
    
    c(input$district2, years, days)
    
  })
  
  graphLabel1 <- reactive({
    
    thisLabel<-c()
    if(labels1()[1]!=labels2()[1]) return(paste(labels1()[1], collapse = "_"))
    if(labels1()[2]!=labels2()[2]) return(paste(labels1()[2], collapse = "_"))
    if(labels1()[3]!=labels2()[3]) return(paste(labels1()[3], collapse = "_"))
    
    return(labels1()[1])
  })
  
  graphLabel2 <- reactive({
    
    if(labels2()[1]!=labels1()[1]) return(paste(labels2()[1], collapse = "_"))
    if(labels2()[2]!=labels1()[2]) return(paste(labels2()[2], collapse = "_"))
    if(labels2()[3]!=labels1()[3]) return(paste(labels2()[3], collapse = "_"))
    
    return(labels2()[1])
  })
  
  # outputs (valueboxes + plots)
  
  output$info1 <- renderValueBox({
    valueBox(
      paste(labels1()[1])
      ,paste0(labels1()[2], ", ", labels1()[3])
      ,icon = icon("map-marker",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$info2 <- renderValueBox({
    valueBox(
      paste(labels2()[1])
      ,paste0(labels2()[2], ", ", labels2()[3])
      ,icon = icon("map-marker",lib='glyphicon')
      ,color = "light-blue")   
  })
  
  
  output$fatal1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Fatal"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=1), "%")
      ,paste('Fatal Accidents')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$fatal2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Fatal"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=1), "%")
      ,paste('Fatal Accidents')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$severe1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Serious"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Severe Accidents')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$severe2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Serious"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Severe Accidents')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$minor1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Slight"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Minor Accidents')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$minor2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Slight"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Minor Accidents')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  
  output$overTime <- renderPlot({
    allAccidents<-rbind(accidents1(), accidents2())
    allAccidents$group<-c(rep(graphLabel1(), nrow(accidents1())), rep(graphLabel2(), nrow(accidents2())))

    allAccidents<-allAccidents%>%group_by(group)%>%mutate(totalNumOfAccidents=n())%>%
      group_by(group, TimeSegment)%>%summarise(percentage=n()/max(totalNumOfAccidents))%>%filter(!is.na(TimeSegment))

    ggplot(allAccidents, aes(x=TimeSegment, y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
      scale_y_continuous(labels=percent)+
      labs(x="", y="", fill="")+
      theme_traffic()
  })
  
  output$overMonth <- renderPlot({
    allAccidents<-rbind(accidents1(), accidents2())
    allAccidents$group<-c(rep(graphLabel1(), nrow(accidents1())), rep(graphLabel2(), nrow(accidents2())))
    
    allAccidents<-allAccidents%>%group_by(group)%>%mutate(totalNumOfAccidents=n())%>%
      group_by(group, Month)%>%summarise(percentage=n()/max(totalNumOfAccidents))%>%filter(!is.na(Month))
    
    ggplot(allAccidents, aes(x=reorder(as.character(Month), as.numeric(Month)), y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
      scale_y_continuous(labels=percent, limits = c(0, 0.12))+
      labs(x="", y="", fill="")+
      theme_traffic()
  })
  
  output$byRoadSurface <- renderPlot({
    allVehicles<-rbind(vehicles1(), vehicles2())
    allVehicles$group<-c(rep(graphLabel1(), nrow(vehicles1())), rep(graphLabel2(), nrow(vehicles2())))
    
    allVehicles<-allVehicles%>%group_by(group)%>%mutate(totalNumOfVehicles=n())%>%
      group_by(group, Vehicle_Type)%>%summarise(percentage=n()/max(totalNumOfVehicles))%>%
      filter(!(Vehicle_Type %in% c(NA, "Data missing or out of range", "Unknown", "Other")))
    
    ggplot(allVehicles, aes(x=Vehicle_Type, y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "identity", alpha = 0.5)+
      scale_y_continuous(labels=percent)+#, limits = c(0, 0.3)
      labs(x="", y="", fill="")+
      theme_traffic()
  })
  
  output$byJourneyPurpose <- renderPlot({
    allVehicles<-rbind(vehicles1(), vehicles2())
    allVehicles$group<-c(rep(graphLabel1(), nrow(vehicles1())), rep(graphLabel2(), nrow(vehicles2())))
    
    allVehicles<-allVehicles%>%group_by(group)%>%mutate(totalNumOfVehicles=n())%>%
      group_by(group, Journey_Purpose_of_Driver)%>%summarise(percentage=n()/max(totalNumOfVehicles))%>%
      filter(!(is.na(Journey_Purpose_of_Driver)))
    
    ggplot(allVehicles, aes(x=Journey_Purpose_of_Driver, y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5)+
      scale_y_continuous(labels=percent)+
      labs(x="", y="", fill="")+
      theme_traffic()
  })
  
  output$byManoeuvre <- renderPlot({
    allVehicles<-rbind(vehicles1(), vehicles2())
    allVehicles$group<-c(rep(graphLabel1(), nrow(vehicles1())), rep(graphLabel2(), nrow(vehicles2())))
    
    allVehicles<-allVehicles%>%group_by(group)%>%mutate(totalNumOfVehicles=n())%>%
      group_by(group, Vehicle_Manoeuvre)%>%summarise(percentage=n()/max(totalNumOfVehicles))%>%
      filter(!(is.na(Vehicle_Manoeuvre)) & Vehicle_Manoeuvre!="Data missing or out of range")
    
    ggplot(allVehicles, aes(x=Vehicle_Manoeuvre, y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5)+
      scale_y_continuous(labels=percent, limits = c(0, 0.55))+
      labs(x="", y="", fill="")+
      theme_traffic()
  })
  
  output$riskEstimation <- renderPlot({
    
    ggplot(riskPredictions(), aes(x=xLabels, y=predictions, colour=Area))+
      geom_line(aes(group=Area), size=1)+
      labs(x="", y="Risk", fill="")+
      scale_y_continuous(labels=percent, limits = c(0, 1))+
      theme_traffic()
    
  })
  
  
}
