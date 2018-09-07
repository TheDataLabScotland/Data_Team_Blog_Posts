library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(scales)
library(lattice)
library(dplyr)
library(scales)
library(ggplot2)



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
      mutate(Area=factor(Area, levels=districtList),
             Day_of_Week=factor(weekdays(datetime), levels=dayList),
             Month=factor(month.abb[as.numeric(format(datetime, "%m"))], levels = month.abb),
             Bank_Holiday=factor(ifelse(datetime %in% bankHolidayList$Date, "Yes", "No"), levels =c("No", "Yes")),
             TimeSegment=as.numeric(gsub(":.*", "", as.character(strftime(datetime, format="%H:%M"))))%/%6+1,
             datetime=NULL)

  })
  
  # produce predictions reactively
  
  riskPredictions<-reactive({
    
    preds <- predict(riskModel, riskPredictionData(), type="prob")
    
    
    riskPredictions<-data.frame(Area=riskPredictionData()["Area"],
                                TimeSegment=riskPredictionData()["TimeSegment"],
                                Day_of_Week=riskPredictionData()["Day_of_Week"],
                                predictions=preds[,2])%>%
      mutate(xLabels=paste(Day_of_Week, hourList[TimeSegment]))
    

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
      group_by(group, Road_Surface_Conditions)%>%summarise(percentage=n()/max(totalNumOfVehicles))%>%
      filter(Road_Surface_Conditions %in% c("Dry", "Frost or ice", "Snow", "Wet or damp"))%>%
      mutate(Road_Surface_Conditions=factor(Road_Surface_Conditions, levels=c("Snow", "Frost or ice", "Wet or damp", "Dry")))
    
    ggplot(allVehicles, aes(x=Road_Surface_Conditions, y=percentage, fill=group))+
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
      theme_traffic()+
      theme(axis.text.x=element_text(angle=0))
    
  })
  
  
}
