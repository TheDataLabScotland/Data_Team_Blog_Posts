library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)



                    dashboardPage(
                      dashboardHeader(
                                          title = "Traffic Accidents in Scotland",
                                          titleWidth = 300,
                                          tags$li(class = "dropdown",
                                                  tags$li(class = "dropdown",
                                                          socialButton(
                                                            url = "http://github.com/TheDataLabScotland/Data_Team_Blog_Posts/tree/master/TrafficAccidents",
                                                            type = "github"
                                                          ),
                                                          style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px; "))
                                    
                                          ),
                      dashboardSidebar(
                         HTML("<br>"),
                         HTML("<div class='titles'>Selection 1</div>"),
                         pickerInput("district", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="Edinburgh, City of"),
                         pickerInput("year", "Year", yearList, options = list("actions-box" = TRUE), selected=c(2010:2016), multiple = TRUE),
                         pickerInput("day", "Day of Week", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList),
                         HTML("<br>"),
                         HTML("<div class='titles'>Selection 2</div>"),
                         pickerInput("district2", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="Glasgow City"),
                         pickerInput("year2", "Year", yearList, options = list("actions-box" = TRUE), selected=c(2010:2016), multiple = TRUE),
                         pickerInput("day2", "Day of Week", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList)
                        
                         ),
                      
                      dashboardBody(
                        
                        fluidRow(
                          valueBoxOutput("info1", width = 6),
                          valueBoxOutput("info2", width = 6)
                        ),
                        fluidRow(
                          
                          valueBoxOutput("fatal1", width = 2),
                          valueBoxOutput("severe1", width = 2),
                          valueBoxOutput("minor1", width = 2),
                          
                          valueBoxOutput("fatal2", width = 2),
                          valueBoxOutput("severe2", width = 2),
                          valueBoxOutput("minor2", width = 2)
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
                            title = "Road Surface Conditions During Accident"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("byJourneyPurpose", height = "300px")
                          ) 
                        ),
                        fluidRow(
                          box(
                            title = "Traffic Accident Risk Estimation",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = 12,
                            plotOutput("riskEstimation", height = "300px")
                          )
                        ),
                        tags$head(tags$style(HTML('.titles {font-weight:bold; text-align:center;
                                                  background-image: linear-gradient(#3c8dbc, #222d32)}')))
                        
                      ))
                    
