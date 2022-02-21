library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyalert)
library(shinycssloaders)



                    dashboardPage(
                      dashboardHeader(
                                          title = "Traffic Accidents in Scotland",
                                          titleWidth = 300,
                                          tags$li(class = "dropdown",
                                                  tags$li(class = "dropdown",
                                                          socialButton(
                                                            href = "http://github.com/TheDataLabScotland/Data_Team_Blog_Posts/tree/master/TrafficAccidents",
                                                            icon = icon(icon"github")
                                                          ),
                                                          style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px; "),
                                                  tags$li(class = "dropdown",
                                                          actionButton("about", "About"),
                                                          style = "padding-top: 7px; padding-bottom: 5px; padding-right: 20px; "))
                                    
                                          ),
                      dashboardSidebar(
                        useShinyalert(),
                         HTML("<br>"),
                         HTML("<div class='titles' id='selection1'>Selection 1</div>"),
                         pickerInput("district", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="Edinburgh"),
                         pickerInput("year", "Year", yearList, options = list("actions-box" = TRUE), selected=c(2010:2016), multiple = TRUE),
                         pickerInput("day", "Day of Week", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList),
                         HTML("<br>"),
                         HTML("<div class='titles' id='selection2'>Selection 2</div>"),
                         pickerInput("district2", "Area", sort(as.character(districtList)), options = list("actions-box" = TRUE), multiple = FALSE, selected="Glasgow City"),
                         pickerInput("year2", "Year", yearList, options = list("actions-box" = TRUE), selected=c(2010:2016), multiple = TRUE),
                         pickerInput("day2", "Day of Week", levels(dayList), options = list("actions-box" = TRUE), multiple = TRUE, selected = dayList),
                         bsTooltip("selection1", 
                                  "Choose any combination of Area, Year and Day of Week.", placement = "bottom", trigger = "hover",
                                  options = NULL),
                         bsTooltip("selection2", 
                                  "Choose any combination of Area, Year and Day of Week.", placement = "bottom", trigger = "hover",
                                  options = NULL)
                        
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
                            title = "Accident Frequency by Hour"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overTime", height = "300px") %>% withSpinner(color="#3c8dbc")
                          ), 
                          bsTooltip("overTime", 
                                    "The height of the curve represents the % of daily accidents that happen that time of the day.", placement = "bottom", trigger = "hover",
                                    options = NULL),
                          box(
                            title = "Distribution of Accidents Across Months"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("overMonth", height = "300px") %>% withSpinner(color="#3c8dbc")
                          ),
                          bsTooltip("overMonth", 
                                    "Each bar shows what % of the total number of accidents within a year happen in each respective month.", placement = "bottom", trigger = "hover",
                                    options = NULL)
                        ),
                        fluidRow( 
                          box(
                            title = "Vehicle Manoeuvre During Accident"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("byManoeuvre", height = "300px") %>% withSpinner(color="#3c8dbc")
                          ),
                          bsTooltip("byManoeuvre", 
                                    "The most frequently performed driving manoeuvres during accidents.", placement = "bottom", trigger = "hover",
                                    options = NULL),
                          box(
                            title = "Road Surface Conditions During Accident"
                            ,status = "primary"
                            ,solidHeader = TRUE 
                            ,collapsible = FALSE 
                            ,plotOutput("byRoadSurface", height = "300px") %>% withSpinner(color="#3c8dbc")
                          ),
                          bsTooltip("byRoadSurface", 
                                    "The most frequent road surface conditions during accidents.", placement = "bottom", trigger = "hover",
                                    options = NULL)
                        ),
                        fluidRow(
                          box(
                            title = "Traffic Accident Risk Estimation",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = 12,
                            plotOutput("riskEstimation", height = "300px") %>% withSpinner(color="#3c8dbc")
                          ),
                          bsTooltip("riskEstimation", 
                                    "A random forest has been trained to predict the probability that an accident will occur in any given 6-hour interval.", placement = "top", trigger = "hover",
                                    options = NULL)
                        ),
                        tags$head(tags$style(HTML('.titles {font-weight:bold; text-align:center;
                                                  background-image: linear-gradient(#3c8dbc, #222d32)}')),
                                  tags$style(HTML('#about {background-color: #3c8dbc; font-weight:bold; color:white;}')),
                                  tags$style(HTML('div[role=tooltip] {font-weight:bold; color:white;}')))
                        
                      ))
                    
