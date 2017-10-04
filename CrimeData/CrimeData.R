
library( data.table )
library( stringr )
library( lubridate )

library( ggplot2 )	
library( ggmap )	
library( mapproj )	
require( RColorBrewer )	


crime <- fread( "/home/caterina/Documents/Data_Team_Blog_Posts/CrimeData/Los_Angeles_Crime_Data_from_2010_to_Present.csv", na.strings = "" )

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

map <- get_map( location = 'Los Angeles', zoom = 20, maptype = "roadmap" )
ggmap( map ) +
  geom_point( data = crime[ TimeOccurred > 2300, ], aes( x = long, y = lat, 
                                 color = TimeOccurred ), 
              alpha = .35 ) + 
  facet_grid( ~ YearOccurred )

myplot <- with(crime, plot(TimeOccurred, lat))
pdf('./Desktop/CrimeDataBogusPlot.pdf', width = 10, height = 10)
myplot
dev.off()


# source( "MyCloudMadeAPI.R")
# ?get_cloudmademap
# qmap( "baylor university", zoom = 14, maptype = 53428, api_key = cloudmade_api_key,
#       source = "cloudmade" )



# Can I anonymize this data? ----------------------------------------------

# This is buggy right now:

library(fakeR)

x <- crime[ 1:200, ]
# fake_x <- simulate_dataset( data.frame( x$DRNumber ), use.levels = FALSE )
lapply(x, is.factor)

fake_cols <- list()
for ( column in 1 : ncol( x ) ) {
   fake_cols[[column]] <- simulate_dataset( as.vector( x[ , column, with = FALSE ] ), use.levels = FALSE, stealth.level = 3, level3.noise = TRUE )
}

# Strange. fakeR seems to not work on x as a whole data.table or data.frame. The only way I can get it to work is to cycle thru the columns individually - which then, the simulate_dataset() function outputs as 

# lapply(fake_cols, dim)
# fake_x <- rbindlist( fake_cols )
# y <- simulate_dataset( x )


xyz <- data.table( sapply( fake_cols, "[[", 1 ) )
setnames( xyz, names( x ) )


table( x$VictimSex )
table( xyz$VictimSex ) # So this thing isn't JUST shuffling the data.




