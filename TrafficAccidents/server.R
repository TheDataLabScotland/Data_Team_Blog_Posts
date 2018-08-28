library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(scales)
library(lattice)
library(dplyr)


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
  
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
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$centile,
         breaks = centileBreaks,
         main = "SuperZIP score (visible zips)",
         xlab = "Percentile",
         xlim = range(allzips$centile),
         col = '#00DD00',
         border = 'white')
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
    
    plotData<-accidentData%>%filter(Accident_Severity %in% input$severity &
                                      Light_Conditions %in% input$light &
                                      Local_Authority_.District. %in% input$district &
                                      Year==input$year &
                                      Weather_Conditions %in% input$weather &
                                      Speed_limit>=input$speed[1] &
                                      Speed_limit<=input$speed[2] &
                                      Number_of_Vehicles>=input$vehicles[1] &
                                      Number_of_Vehicles<=input$vehicles[2])%>%
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
