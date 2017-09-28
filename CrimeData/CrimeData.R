
library( data.table )
library( stringr )
library( lubridate )

library( ggplot2 )	
library( ggmap )	
library( mapproj )	
require( RColorBrewer )	


crime <- fread( "/home/caterina/Downloads/Crime_Data_from_2010_to_Present.csv" )

head( crime )
lat_and_long <- str_split( crime$Location, ", " )
lat <- sapply( lat_and_long, "[", 1 )
long <- sapply( lat_and_long, "[", 2 )

lat <- as.numeric( str_replace( lat, "\\(", "" ) )
long <- as.numeric( str_replace( long, "\\)", "" ) )

crime[ , lat := lat ]
crime[ , long := long ]

setnames( crime, str_replace( names( crime ), " ", "" ) )


crime[ , DateOccurred := as.Date( DateOccurred, format = "%m/%d/%Y" ) ]
crime[ , DateReported := as.Date( DateReported, format = "%m/%d/%Y" ) ]

crime[ , YearOccurred := year( DateOccurred ) ]
crime[ , YearReported := year( DateReported ) ]

table( crime$YearOccurred )
table( crime$YearReported )
table( crime$TimeOccurred )

map <- get_map( location = 'Los Angeles', zoom = 12, maptype = "roadmap" )
ggmap( map ) +
  geom_point( data = crime, aes( x = long, y = lat, color = TimeOccurred ), alpha = .35 ) + 
  facet_grid( ~ YearOccurred, ncol = 4 )

# source( "MyCloudMadeAPI.R")
# ?get_cloudmademap
# qmap( "baylor university", zoom = 14, maptype = 53428, api_key = cloudmade_api_key,
#       source = "cloudmade" )





