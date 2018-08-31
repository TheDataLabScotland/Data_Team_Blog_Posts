library(leaflet)
library(shinyWidgets)
library(shinydashboard)


navbarPage("Traffic Accidents in Scotland", id="nav",
           
           tabPanel("Comparison Dashboard",
            
                    dashboardPage(
                      dashboardHeader(titleWidth = 0),
                      dashboardSidebar(
                        width=400,
                      column(6,
                        pickerInput("severity", "Accident Severity", as.character(severityList), options = list("actions-box" = TRUE), multiple = TRUE, selected=severityList),
                        pickerInput("light", "Light Conditions", as.character(lightList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Daylight"),
                        pickerInput("weather", "Weather Conditions", as.character(weatherList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Fine no high winds"),
                        pickerInput("district", "District", as.character(districtList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Edinburgh, City of"),
                        pickerInput("make", "Car Make", choices=as.character(makeList), options = list("actions-box" = TRUE), multiple = TRUE, selected = makeList),
                        pickerInput("year", "Year", yearList, options = list("actions-box" = TRUE), selected=max(yearList), multiple = TRUE),
                        sliderInput("speed", "Speed Limit", min=min(speedList, na.rm = TRUE), max=max(speedList, na.rm=TRUE), value=c(min(speedList, na.rm=TRUE), max(speedList, na.rm = TRUE)), round=TRUE)
                             
                      ),
                      column(6,
                             pickerInput("severity2", "Accident Severity", as.character(severityList), options = list("actions-box" = TRUE), multiple = TRUE, selected=severityList),
                             pickerInput("light2", "Light Conditions", as.character(lightList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Daylight"),
                             pickerInput("weather2", "Weather Conditions", as.character(weatherList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Fine no high winds"),
                             pickerInput("district2", "District", as.character(districtList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Glasgow City"),
                             pickerInput("make2", "Car Make", choices=as.character(makeList), options = list("actions-box" = TRUE), multiple = TRUE, selected = makeList),
                             pickerInput("year2", "Year", yearList, options = list("actions-box" = TRUE), selected=max(yearList), multiple = TRUE),
                             sliderInput("speed2", "Speed Limit", min=min(speedList, na.rm = TRUE), max=max(speedList, na.rm=TRUE), value=c(min(speedList, na.rm=TRUE), max(speedList, na.rm = TRUE)), round=TRUE)
                      )
                      ),
                      
                      dashboardBody(
                        fluidRow(
                          valueBoxOutput("info1", width = 3),
                          valueBoxOutput("fatality1", width = 2),
                          valueBoxOutput("placeholder1", width = 1),
                          valueBoxOutput("info2", width = 3),
                          valueBoxOutput("fatality2", width = 2)
                        ),
                        fluidRow( 
                          box(
                            title = "Accidents by hour"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overTime1", height = "300px")
                          )
                          ,box(
                            title = "Accidents by hour"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overTime2", height = "300px")
                          ) 
                        ),
                        fluidRow( 
                          box(
                            title = "Accidents by Speed Limit"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overSpeed1", height = "300px")
                          )
                          ,box(
                            title = "Accidents by Speed Limit"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overSpeed2", height = "300px")
                          ) 
                        )
                        
                      ))
                    
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

# 
# div(class="outer",
#     
#     tags$head(
#       # Include our custom CSS
#       includeCSS("styles.css"),
#       includeScript("gomap.js"),
#       includeScript("www/Timeline.js"),
#       includeScript("www/TimelineSliderControl.js")
#     ),
#     
#     # If not using custom CSS, set height of leafletOutput to a number instead of percent
#     leafletOutput("timeline", width="100%", height="100%"),
#     # HTML("<hr>"),
#     leafletOutput("timeline2", width="100%", height="100%"),
#     
#     # Shiny versions prior to 0.11 should use class = "modal" instead.
#     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#                   width = 330, height = "auto",
#                   
#                   h2("Accident Explorer"),
#                   
#                   pickerInput("severity", "Accident Severity", as.character(severityList), options = list("actions-box" = TRUE), multiple = TRUE, selected=c("Serious")),
#                   pickerInput("light", "Light Conditions", as.character(lightList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Daylight"),
#                   pickerInput("weather", "Weather Conditions", as.character(weatherList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Fine no high winds"),
#                   pickerInput("district", "District", as.character(districtList), options = list("actions-box" = TRUE), multiple = TRUE, selected="Highland"),
#                   pickerInput("make", "Car Make", choices=as.character(makeList), options = list("actions-box" = TRUE), multiple = T, selected = makeList),
#                   pickerInput("year", "Year", yearList, options = list("actions-box" = TRUE), selected=max(yearList)),
#                   sliderInput("vehicles", "Number of Involved Vehicles", min=min(vehicleNumberList), max=max(vehicleNumberList), value=c(min(vehicleNumberList), max(vehicleNumberList)), round=TRUE),
#                   sliderInput("speed", "Speed Limit", min=min(speedList, na.rm = TRUE), max=max(speedList, na.rm=TRUE), value=c(min(speedList, na.rm=TRUE), max(speedList, na.rm = TRUE)), round=TRUE),
#                   
#                   conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
#                                    # Only prompt for threshold when coloring or sizing by superzip
#                                    numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
#                   ),
#                   
#                   plotOutput("histCentile", height = 200),
#                   plotOutput("scatterCollegeIncome", height = 250)
#     ),
#     
#     tags$div(id="cite",
#              'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
#     )
# )