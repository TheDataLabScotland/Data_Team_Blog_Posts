library(leaflet)
library(shinyWidgets)
library(shinydashboard)


navbarPage("Traffic Accidents in Scotland", id="nav",
           
           tabPanel("Comparison Dashboard",
            
                    dashboardPage(
                      dashboardHeader(titleWidth = 0),
                      dashboardSidebar(
                        
                        pickerInput("district", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="Edinburgh, City of"),
                        pickerInput("year", "Year", yearList, options = list("actions-box" = TRUE), selected=max(yearList), multiple = TRUE),
                        pickerInput("day", "Day", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList),

                        pickerInput("district2", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="Glasgow City"),
                        pickerInput("year2", "Year", yearList, options = list("actions-box" = TRUE), selected=max(yearList), multiple = TRUE),
                        pickerInput("day2", "Day", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList)
                        ),
                      
                      dashboardBody(
                        fluidRow(
                          valueBoxOutput("info1", width = 4),
                          valueBoxOutput("fatality1", width = 2),
                          valueBoxOutput("info2", width = 4),
                          valueBoxOutput("fatality2", width = 2)
                        ),
                        fluidRow( 
                          box(
                            title = "Distribution of Accidents During the Day"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overTime", height = "300px")
                          )
                          ,box(
                            title = "Distribution of Accidents Across Months"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overMonth", height = "300px")
                          ) 
                        ),
                        fluidRow( 
                          box(
                            title = "Vehicle Manoeuvre During Accident"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("byManoeuvre", height = "300px")
                          )
                          ,box(
                            title = "Age Distribution of Drivers Involved in Accidents"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("byDriverAge", height = "300px")
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
