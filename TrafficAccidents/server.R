library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
library(scales)


function(input, output, session) {
  
  ## for the dashboard
  
  accidents1<-reactive({
    accidentData%>%filter(Accident_Severity %in% input$severity &
                            Light_Conditions %in% input$light &
                            Local_Authority_.District. %in% input$district &
                            Year %in% input$year &
                            Weather_Conditions %in% input$weather &
                            Speed_limit>=input$speed[1] &
                            Speed_limit<=input$speed[2] &
                            Day_of_Week %in% input$day &
                            Accident_Index %in% vehicles1()$Accident_Index)
  })
  
  accidents2<-reactive({
    accidentData%>%filter(Accident_Severity %in% input$severity2 &
                            Light_Conditions %in% input$light2 &
                            Local_Authority_.District. %in% input$district2 &
                            Year %in% input$year2 &
                            Weather_Conditions %in% input$weather2 &
                            Speed_limit>=input$speed2[1] &
                            Speed_limit<=input$speed2[2] &
                            Day_of_Week %in% input$day2 &
                            Accident_Index %in% vehicles2()$Accident_Index)
  })
  
  vehicles1<-reactive({
    vehicleData%>%filter(make %in% input$make)
  })
  
  vehicles2<-reactive({
    vehicleData%>%filter(make %in% input$make)
  })
  
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
  
  output$info1 <- renderValueBox({
    valueBox(
      paste(labels1()[1])
      ,paste0(labels1()[2], ", ", labels1()[3])
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$info2 <- renderValueBox({
    valueBox(
      paste(labels2()[1])
      ,paste0(labels2()[2], ", ", labels2()[3])
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")   
  })
  
  
  
  
  output$fatality1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Fatal"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=1, big.mark=','), "%")
      ,paste('Fatality Percentage')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$fatality2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Fatal"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=1, big.mark=','), "%")
      ,paste('Fatality Percentage')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$overTime1 <- renderPlot({
    totalAccidents<-nrow(accidents1())
    plotData<-accidents1()%>%group_by(TimeSegment)%>%summarise(percentage=n()/totalAccidents)%>%filter(!is.na(TimeSegment))
    ggplot(plotData, aes(x=TimeSegment, y=percentage))+
      geom_bar(stat = "identity")+
      scale_y_continuous(labels=percent)+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      xlab("Time of the Day")+
      ylab("Percentage of Total Accidents")
  })
  
  output$overTime2 <- renderPlot({
    totalAccidents<-nrow(accidents2())
    plotData<-accidents2()%>%group_by(TimeSegment)%>%summarise(percentage=n()/totalAccidents)%>%filter(!is.na(TimeSegment))
    ggplot(plotData, aes(x=TimeSegment, y=percentage))+
      geom_bar(stat = "identity")+
      scale_y_continuous(labels=percent)+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      xlab("Time of the Day")+
      ylab("Percentage of Total Accidents")
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
  
  output$overSpeed1 <- renderPlot({
    totalAccidents<-nrow(accidents1())
    plotData<-accidents1()%>%group_by(Speed_limit)%>%summarise(percentage=n()/totalAccidents)%>%filter(!is.na(Speed_limit))
    ggplot(plotData, aes(x=Speed_limit, y=percentage))+
      geom_bar(stat = "identity")+
      scale_y_continuous(labels=percent)+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      xlab("Speed Limit")+
      ylab("Percentage of Total Accidents")
  })
  
  output$overSpeed2 <- renderPlot({
    totalAccidents<-nrow(accidents2())
    plotData<-accidents2()%>%group_by(Speed_limit)%>%summarise(percentage=n()/totalAccidents)%>%filter(!is.na(Speed_limit))
    ggplot(plotData, aes(x=Speed_limit, y=percentage))+
      geom_bar(stat = "identity")+
      scale_y_continuous(labels=percent)+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      xlab("Speed Limit")+
      ylab("Percentage of Total Accidents")
  })
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })
  
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  output$mainMap <- renderLeaflet({
    m <- leaflet(accidentData) %>%
      #fitBounds(min(accidentData$Longitude), ~min(accidentData$Latitude), ~max(accidentData$Longitude), ~max(accidentData$Latitude)) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude,
                 clusterOptions = markerClusterOptions(),
                 popup = 
                   paste0("<b>Accident Severity: </b>", as.character(accidentData$Accident_Severity), "</br>",
                          "<b>Weather: </b>", as.character(accidentData$Weather_Conditions), "</br>",
                          "<b>Road Surface: </b>", as.character(accidentData$Road_Surface_Conditions), "</br>",
                          "<b>Light Conditions: </b>", as.character(accidentData$Light_Conditions), "</br>",
                          "<b>Number of Vehicles: </b>", as.character(accidentData$Number_of_Vehicles), "</br>",
                          "<b>Date: </b>", as.character(accidentData$Date))
      )
    
    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }
    
    m <- m %>% mapOptions(zoomToLimits = rezoom)
    
    m
  })
  
  output$timeline <- renderLeaflet({
    
    relevantVehicles<-vehicleData%>%filter(make %in% input$make)
    
    plotData<-accidentData%>%filter(Accident_Severity %in% input$severity &
                                      Light_Conditions %in% input$light &
                                      Local_Authority_.District. %in% input$district &
                                      Year==input$year &
                                      Weather_Conditions %in% input$weather &
                                      Speed_limit>=input$speed[1] &
                                      Speed_limit<=input$speed[2] &
                                      Number_of_Vehicles>=input$vehicles[1] &
                                      Number_of_Vehicles<=input$vehicles[2] &
                                      Accident_Index %in% relevantVehicles$Accident_Index)%>%
      mutate(Date=as.Date(Date))%>%
      mutate(start=as.Date(format(Date, "%d-%m-%Y"), format="%d-%m-%Y"))%>%
      select(Longitude, Latitude, start)%>%
      mutate(end=start+1)
    
    print(nrow(plotData))
    
    #power$end <- power$Date
    
    plotData_geo <- geojson_json(plotData,lat="Latitude",lon="Longitude")
    
    leaf <- leaflet() %>%
      addTiles()
    
    # add leaflet-timeline as a dependency
    #  to get the js and css
    # leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
    #   name = "leaflet-timeline",
    #   version = "1.0.0",
    #   src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
    #   script = "javascripts/leaflet.timeline.js",
    #   stylesheet = "stylesheets/leaflet.timeline.css"
    # )
    
    # use the new onRender in htmlwidgets to run
    #  this code once our leaflet map is rendered
    #  I did not spend time perfecting the leaflet-timeline
    #  options
    leaf<-leaf %>%
      onRender(sprintf(
        '
        function(el,x){
        var plotData = %s;
        
        var timelineControl = L.timelineSliderControl({
        formatOutput: function(date) {
        return new Date(date).toString();
        }
        });
        
        var timeline = L.timeline(plotData, {
        pointToLayer: function(data, latlng){
        var hue_min = 120;
        var hue_max = 0;
        var hue = hue_min;
        return L.circleMarker(latlng, {
        radius: 5,
        color: "#3388ff",
        fillColor: "hsl("+hue+", 100%%, 10%%)"
        });
        },
        steps: 1000,
        duration: 10000,
        showTicks: true
        });
        timelineControl.addTo(HTMLWidgets.find(".leaflet").getMap());
        timelineControl.addTimelines(timeline);
        timeline.addTo(HTMLWidgets.find(".leaflet").getMap());
        }
        ',
        data=plotData_geo
      ))%>%
      setView(lng = -3.9, lat = 57, zoom = 6.5)
    
    leaf
  })
  
  output$timeline2 <- renderLeaflet({
    
    relevantVehicles<-vehicleData%>%filter(make %in% input$make)
    
    plotData<-accidentData%>%filter(Accident_Severity %in% input$severity &
                                      Light_Conditions %in% input$light &
                                      Local_Authority_.District. %in% input$district &
                                      Year==input$year &
                                      Weather_Conditions %in% input$weather &
                                      Speed_limit>=input$speed[1] &
                                      Speed_limit<=input$speed[2] &
                                      Number_of_Vehicles>=input$vehicles[1] &
                                      Number_of_Vehicles<=input$vehicles[2] &
                                      Accident_Index %in% relevantVehicles$Accident_Index)%>%
      mutate(Date=as.Date(Date))%>%
      mutate(start=as.Date(format(Date, "%d-%m-%Y"), format="%d-%m-%Y"))%>%
      select(Longitude, Latitude, start)%>%
      mutate(end=start+1)
    
    print(nrow(plotData))
    
    #power$end <- power$Date
    
    plotData_geo <- geojson_json(plotData,lat="Latitude",lon="Longitude")
    
    leaf <- leaflet() %>%
      addTiles()
    
    # add leaflet-timeline as a dependency
    #  to get the js and css
    # leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
    #   name = "leaflet-timeline",
    #   version = "1.0.0",
    #   src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
    #   script = "javascripts/leaflet.timeline.js",
    #   stylesheet = "stylesheets/leaflet.timeline.css"
    # )
    
    # use the new onRender in htmlwidgets to run
    #  this code once our leaflet map is rendered
    #  I did not spend time perfecting the leaflet-timeline
    #  options
    leaf<-leaf %>%
      onRender(sprintf(
        '
        function(el,x){
        var plotData = %s;
        
        var timelineControl = L.timelineSliderControl({
        formatOutput: function(date) {
        return new Date(date).toString();
        }
        });
        
        var timeline = L.timeline(plotData, {
        pointToLayer: function(data, latlng){
        var hue_min = 120;
        var hue_max = 0;
        var hue = hue_min;
        return L.circleMarker(latlng, {
        radius: 5,
        color: "#3388ff",
        fillColor: "hsl("+hue+", 100%%, 10%%)"
        });
        },
        steps: 1000,
        duration: 10000,
        showTicks: true
        });
        timelineControl.addTo(HTMLWidgets.find(".leaflet").getMap());
        timelineControl.addTimelines(timeline);
        timeline.addTo(HTMLWidgets.find(".leaflet").getMap());
        }
        ',
        data=plotData_geo
      ))%>%
      setView(lng = -3.9, lat = 57, zoom = 6.5)
    
    leaf
})
  
}
