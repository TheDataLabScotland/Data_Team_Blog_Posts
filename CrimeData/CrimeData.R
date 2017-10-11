
library( data.table )
library( tidyr )
library( stringr )
library( lubridate )

library( ggplot2 )	
library( ggmap )
library( ggrepel )
# library( mapproj )	
# require( RColorBrewer )	

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
              aes_string( x = long, y = lat ), 
              alpha = .15, color = "black", size = 2 ) +
  labs( x = "Longitude", y = "Latitude", title = "LA criminal activity" )
bare_version

# Version 2: creating a facet
version_with_one_facet <- bare_version + facet_wrap( ~ YearOccurred, ncol = 3 )
version_with_one_facet

# Version 3: using multiple facets
version_with_two_facets <- bare_version + facet_grid( YearOccurred ~ MonthOccurred )
version_with_two_facets

# Remove x and y axis tick marks & labels due to clutter
version_with_two_facets_no_labels <- version_with_two_facets +
  theme( axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() )
version_with_two_facets_no_labels


# Isolating a specific type of crime, to spot any trends: For this, I have to check all columns (MOCode_1 to MOCode_10) for the following values:

Internet_crimes <- c( "1904", "1905", "1908", "1909", "1911", "1914", "1915", "1916" )
narcostics_prostitution <- c( "0907", "0908" )
# crime[ MOCodes %in% ,  ]

Internet_crime_data <- crime[ MOCode_1 %in% Internet_crimes | MOCode_2 %in% Internet_crimes | MOCode_3 %in% Internet_crimes |
                                MOCode_4 %in% Internet_crimes | MOCode_5 %in% Internet_crimes | MOCode_6 %in% Internet_crimes |
                                MOCode_7 %in% Internet_crimes | MOCode_8 %in% Internet_crimes | MOCode_9 %in% Internet_crimes |
                                MOCode_10 %in% Internet_crimes, ]

# Swap data used previously with this plot:
replot <- get('%+%', 'package:ggplot2')


replot(version_with_two_facets_no_labels, Internet_crime_data)
attach(Internet_crime_data)
version_with_two_facets_no_labels %+% Internet_crime_data
detach(Internet_crime_data)  


# Label some of the points
pdf( 'CrimeDataBogusPlot.pdf', width = 10, height = 8 )
ggmap( map ) +
  geom_point( data = crime[ TimeOccurred > 2300 & VictimAge < 18 & YearOccurred > 2011, ], 
              aes( x = long, y = lat, color = StatusDescription ), 
              alpha = .35 ) +
  facet_grid( YearOccurred ~ MonthOccurred ) +
  theme( axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank() ) +
  geom_label_repel( data = crime[ TimeOccurred > 2300 & VictimAge < 18 & YearOccurred > 2011, ],
                    aes( x = long, y = lat, label = VictimDescent ),
                    fill = "white", box.padding = unit( 0.4, "lines" ),
                    label.padding = unit( 0.15, "lines" ),
                    segment.color = "red", segment.size = 1 )
dev.off()



