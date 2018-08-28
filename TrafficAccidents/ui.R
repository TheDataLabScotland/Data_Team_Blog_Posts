library(leaflet)
library(shinyWidgets)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("Traffic Accidents in Scotland", id="nav",
           
           tabPanel("Timeline Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js"),
                          includeScript("www/Timeline.js"),
                          includeScript("www/TimelineSliderControl.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("timeline", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Accident Explorer"),
                                      
                                      pickerInput("severity", "Accident Severity", as.character(severityList), options = list("actions-box" = TRUE), multiple = TRUE, selected=c("Serious")),
                                      pickerInput("light", "Light Conditions", as.character(lightList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Daylight"),
                                      pickerInput("weather", "Weather Conditions", as.character(weatherList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Fine no high winds"),
                                      pickerInput("district", "District", as.character(districtList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Highland"),
                                      pickerInput("make", "Car Make", choices=as.character(makeList), options = list("actions-box" = TRUE), multiple = T, selected = makeList),
                                      pickerInput("year", "Year", yearList, options = list("actions-box" = TRUE), selected=max(yearList)),
                                      sliderInput("vehicles", "Number of Involved Vehicles", min=min(vehicleNumberList), max=max(vehicleNumberList), value=c(min(vehicleNumberList), max(vehicleNumberList)), round=TRUE),
                                      sliderInput("speed", "Speed Limit", min=min(speedList, na.rm = TRUE), max=max(speedList, na.rm=TRUE), value=c(min(speedList, na.rm=TRUE), max(speedList, na.rm = TRUE)), round=TRUE),
                                      
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
                        )
                    )
           ),
           
           tabPanel("Interactive Map",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
