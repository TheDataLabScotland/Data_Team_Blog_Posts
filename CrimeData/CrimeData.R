
library( data.table )
library( tidyr )
library( stringr )
library( lubridate )

library( ggplot2 )	
library( ggmap )
library( ggrepel )
library( jsonlite )

setwd("/home/u010/c_constant/Documents/BlogPosts/CrimeData")

crime <- fread( "CrimeData.csv", na.strings = c( "", "NA", "-", "X" ) )

head( crime )
lat_and_long <- str_split( crime$Location, ", " )
lat <- sapply( lat_and_long, "[", 1 )
long <- sapply( lat_and_long, "[", 2 )

lat <- as.numeric( str_replace( lat, "\\(", "" ) )
long <- as.numeric( str_replace( long, "\\)", "" ) )

crime[ , lat := lat ]
crime[ , long := long ]

setnames( crime, str_replace_all( names( crime ), " ", "" ) )


crime[ , DateOccurred := as.Date( DateOccurred, format = "%m/%d/%Y" ) ]
crime[ , MonthOccurred := lubridate::month( DateOccurred, label = TRUE ) ]
crime[ , DateReported := as.Date( DateReported, format = "%m/%d/%Y" ) ]

crime[ , YearOccurred := year( DateOccurred ) ]
crime[ , YearReported := year( DateReported ) ]

# Break the multiple MOCodes specified per cell into different cells/columns
crime <- separate( crime, "MOCodes", into = paste( "MOCode", 1:10, sep = "_" ), sep = " " )



map <- get_map( location = 'Los Angeles', zoom = 10, maptype = "roadmap" )


# Version 1: simplest form
bare_version <- ggmap( map ) +
  geom_point( data = crime[ TimeOccurred > 2300 & VictimAge < 18 & YearOccurred > 2011, ], 
              aes( x = long, y = lat ), 
              alpha = .15, color = "black", size = 2 ) +
  labs( x = "Longitude", y = "Latitude", title = "LA criminal activity - all types" ) +
  theme_grey( base_size = 18 ) 
png( './CrimeDataVisuals/BareMap.png', width = 800, height = 800 )
bare_version
dev.off()


# Version 2: creating a facet
version_with_one_facet <- bare_version + facet_wrap( ~ YearOccurred, ncol = 3 )
png( './CrimeDataVisuals/Map_OneFacet.png', width = 1300, height = 800 )
version_with_one_facet
dev.off()


# Version 3: using multiple facets
version_with_two_facets <- bare_version + facet_grid( YearOccurred ~ MonthOccurred )
png( './CrimeDataVisuals/Map_TwoFacets.png', width = 1500, height = 1300 )
version_with_two_facets
dev.off()


# Version 4: Remove x and y axis tick marks & labels due to clutter
version_with_two_facets_no_labels <- version_with_two_facets +
  theme( axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() )
png( './CrimeDataVisuals/Map_TwoFacets_NoLabels.png', width = 1500, height = 1300 )
version_with_two_facets_no_labels
dev.off()

# Isolating a specific type of crime, to spot any trends: For this, I have to check all columns (MOCode_1 to MOCode_10) for the following values:

Internet_crimes <- c( "1904", "1905", "1908", "1909", "1911", "1914", "1915", "1916" )
narcotics_prostitution <- c( "0907", "0908" )


Internet_crime_data <- crime[ MOCode_1 %in% Internet_crimes | MOCode_2 %in% Internet_crimes | MOCode_3 %in% Internet_crimes |
                                MOCode_4 %in% Internet_crimes | MOCode_5 %in% Internet_crimes | MOCode_6 %in% Internet_crimes |
                                MOCode_7 %in% Internet_crimes | MOCode_8 %in% Internet_crimes | MOCode_9 %in% Internet_crimes |
                                MOCode_10 %in% Internet_crimes, ]




Narc_pros_crime_data <- crime[ MOCode_1 %in% narcotics_prostitution | MOCode_2 %in% narcotics_prostitution | MOCode_3 %in% narcotics_prostitution |
                                MOCode_4 %in% narcotics_prostitution | MOCode_5 %in% narcotics_prostitution | MOCode_6 %in% narcotics_prostitution |
                                MOCode_7 %in% narcotics_prostitution | MOCode_8 %in% narcotics_prostitution | MOCode_9 %in% narcotics_prostitution |
                                MOCode_10 %in% narcotics_prostitution, ]

# # Swap data used previously with this plot:
# replot <- get('%+%', 'package:ggplot2')
# replot(version_with_two_facets_no_labels, Internet_crime_data)
# attach(Internet_crime_data)
# version_with_two_facets_no_labels %+% Internet_crime_data
# detach(Internet_crime_data)  

# General form:
ggmap( map ) +
  geom_point( data = crime, 
              aes( x = long, y = lat ), 
              alpha = .05, color = "black", size = 2 ) +
  labs( x = "Longitude", y = "Latitude", 
        title = "LA criminal activity - across crime types" ) +
  facet_wrap( ~ YearOccurred ) +
  theme( axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() ) +
  theme_grey( base_size = 18 ) 


# Internet crime:
png( './CrimeDataVisuals/InternetCrimes_x_Year.png', width = 1000, height = 1000 )
ggmap( map ) +
  geom_point( data = Internet_crime_data, 
              aes( x = long, y = lat ), 
              alpha = .15, color = "black", size = 2 ) +
  labs( x = "Longitude", y = "Latitude", 
        title = "LA criminal activity - Internet crime" ) +
  facet_wrap( ~ YearOccurred ) +
  theme( axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() ) +
  theme_grey( base_size = 18 ) 
dev.off()

# Drugs and prostitution:
png( './CrimeDataVisuals/DrugsPros_x_Year.png', width = 1000, height = 1000 )
ggmap( map ) +
  geom_point( data = Narc_pros_crime_data, 
              aes( x = long, y = lat ), 
              alpha = .15, color = "black", size = 2 ) +
  labs( x = "Longitude", y = "Latitude", 
        title = "LA criminal activity - Narcotics and prostitution" ) +
  facet_wrap( ~ YearOccurred ) +
  theme( axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() ) +
  theme_grey( base_size = 18 ) 
dev.off()



# Now to get labels for some popular tourist landmarks - the assumption being that the drugs & prostituation activity should show little overlap with these famous / busy places.
# Inspired by : https://stackoverflow.com/questions/34800031/retrieve-location-coordinates-from-google-maps-in-r



LA_centre_coords <- c( 34.052235, -118.243683 )
get_your_own <- readLines( "GoogleMapsAPIKey.txt" )


pinpoint_locations <- function( location, radius, keyword, print.query = FALSE ){
  
  # radius is in meters
  # location will represent a pair of coordinates searched for by hand
  
  coord_pair <- paste( location[1], location[2], sep = "," )
  baseurl <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
  google_key <- get_your_own # Instructions in original stackoverflow link
  
  
  query <- paste( baseurl, 
                  "location=", coord_pair, "&radius=", radius, 
                  "&keyword=", 
                  # Originally: "&types=food|restaurant&keyword=",
                  # Can get other 'nearby' types from: 
                  # https://developers.google.com/places/supported_types
                  keyword,"&key=", google_key, 
                  sep = "" )

  if ( print.query == TRUE ) {
    print( query )
  }
  
  query_results <- jsonlite::fromJSON( URLencode( query ) )
  
  lat_long <- data.frame( lat = query_results$results$geometry$location$lat,
                          long = query_results$results$geometry$location$lng )
  
  places <- query_results$results$name
  
  output <- cbind( places, lat_long )
  return( output )
}


tourist_landmarks <- pinpoint_locations( location = LA_centre_coords, 
                                         radius = 30000, 
                                         keyword = "tourist attractions",
                                         print.query = FALSE )




# Label some of the points
png( './CrimeDataVisuals/CrimeDataLabelledMap.png', width = 1000, height = 1000 )
ggmap( map ) +
  geom_point( data = Narc_pros_crime_data, 
              aes( x = long, y = lat ), 
              alpha = .15, color = "black", size = 2.5 ) +
  labs( x = "Longitude", y = "Latitude", 
        title = "LA criminal activity (2010-2017): Narcotics and prostitution vs. typical tourist attractions" ) +
  theme( axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() ) +
  geom_point(data = tourist_landmarks,
             aes( x = long, y = lat ), color = "red", size = 2 ) +
  geom_label_repel( data = tourist_landmarks,
                    aes( x = long, y = lat, label = places ),
                    force = 40,
                    fill = "white", box.padding = unit( 0.3, "lines" ),
                    label.padding = unit( 0.1, "lines" ),
                    segment.color = "red", segment.size = 0.3 )
dev.off()



