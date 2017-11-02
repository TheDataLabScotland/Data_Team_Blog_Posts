

# Packages ----------------------------------------------------------------



library( data.table )
library( ggplot2 )
library( stringi )
library( stringr )
library( plyr )
library( rworldmap )
library( devtools )
# install_github("dgrtwo/gganimate")
library( gganimate )

options( scipen = 999 )

setwd( "your/path/goes/here" )
load( "HistoricalDataRObjects.RData" )





# Get data ----------------------------------------------------------------



BBC_events <- fread( "BBC_Summary_of_World_Broadcasts_1979_2015_.csv", na.strings = "" )
BBC_meta <- fread( "BBC_Summary_of_World_Broadcasts_1979_2015_MetaData.csv", na.strings = "" )

# Understanding and tidying the data --------------------------------------



table( duplicated( BBC_events$aid ) ) # Duplicates here...
table( duplicated( BBC_events$eid ) ) # But not here... Hm.
# This is a combination between news source and event number.
# Which leads me to beliebe that events themselves may be repeated, but covered by different sources.
# In keepng with the a common data management recommendation, values within columns should represent atomic values, i.e., each cell should contain just one values, instead of two. So we shall try to fix this:
BBC_events[ , EIDSource := gsub( "[[:digit:]]","", BBC_events$eid ) ]
BBC_events[ , EIDEvent := gsub( "[[:alpha:]]","", BBC_events$eid ) ]


# Exclude events that have no date, or no lat and long:
BBC_events <- BBC_events[ ! is.na( story_date ), ]
BBC_events[ , story_date := as.Date( story_date, format = "%m/%d/%Y" ) ]

BBC_events <- BBC_events[ ! is.na( lat ) & ! is.na( lon ), ]

# These source and target codes dont mean much in themselves. So I'm going to match them up to the explanations extracted from the rworldmap package.

data( countryRegions, envir = environment(), package = "rworldmap" )

# Get continents / Stern report classification:
BBC_events[ , continent := mapvalues( BBC_events$countryname,
                                      from = countryRegions$ISO3, 
                                      to = countryRegions$GEO3 ) ]


# Get proper country names from ISO3 codes:
BBC_events[ , countryname := mapvalues( BBC_events$countryname,
                                        from = countryRegions$ISO3, 
                                        to = countryRegions$ADMIN ) ]

unrecognized_iso <- unique( BBC_events$countryname )[ nchar( unique( BBC_events$countryname ) ) < 4 ]

BBC_events[ , countryname := ifelse( countryname %in% unrecognized_iso, NA, countryname ) ]
BBC_events[ , continent := ifelse( continent %in% unrecognized_iso, NA, continent ) ]


# Cleaning source / target variables:
# Remove punctuation from these character vars:
BBC_events[ , target := str_trim( str_extract( target, "[[:alpha:]]+" ), side = "both" ) ]
BBC_events[ , source := str_trim( str_extract( source, "[[:alpha:]]+" ), side = "both" ) ]

table( nchar( BBC_events$target ) )
table( nchar( BBC_events$source ) )


# Gotta split strings into groups of 3 ... 
targets_list <- stri_extract_all_regex( BBC_events$target, '.{1,3}' )
targets_data_matrix <- plyr::ldply( targets_list, rbind )
setnames( targets_data_matrix, paste( "target", 1:9, sep = "_" ) )

sources_list <- stri_extract_all_regex( BBC_events$source, '.{1,3}' )
sources_data_matrix <- plyr::ldply( sources_list, rbind )
setnames( sources_data_matrix, paste( "source", 1:9, sep = "_" ) )


# Get a measure of how many sources and or targets an entry has. 
# Presumably, the bigger the event, the more sources it's picked up by, and the more targets it involves.
BBC_events[ , total_sources := unlist( lapply( sources_list, length ) ) ]
BBC_events[ , total_targets := unlist( lapply( targets_list, length ) ) ]


# Replace each code by its actual meaning to help with deciphering dataset:

source_target_dict <- fread( "SourceOrTargetCodes_ClineBBCData.csv" )

targets_data_matrix <- data.table( mapvalues( as.matrix( targets_data_matrix ), 
                                              from = source_target_dict$Code, 
                                              to = source_target_dict$`Source/Target` ) )
sources_data_matrix <- data.table( mapvalues( as.matrix( sources_data_matrix ), 
                                              from = source_target_dict$Code, 
                                              to = source_target_dict$`Source/Target` ) )

full_BBC_events <- data.table( BBC_events, sources_data_matrix, targets_data_matrix )

# Joining meta data with main data:
setnames( BBC_meta, "pubdate", "story_date" )
BBC_meta[ , story_date := as.Date( story_date, format = "%m/%d/%Y" ) ]

full_BBC_events_with_meta <- join( full_BBC_events, BBC_meta, by = c( "aid", "story_date" ) )


# Remove unnecessary columns for simplicity:
full_BBC_events_with_meta$original_source <- NULL
full_BBC_events_with_meta$process <- NULL

# Trying to understand the structure of this dataset:
table( table( full_BBC_events_with_meta$eid ) ) # Seems like events are unique
table( table( full_BBC_events_with_meta$aid ) ) # Seems like articles repeat themselves, confusingly
table( table( full_BBC_events_with_meta$code ) ) # event's code for: Conflict and Mediation Event Observation (CAMEO code)
table( table( full_BBC_events_with_meta$root_code ) ) # Super-ordinate CAMEO code, with following dictionary:

CAMEO_root_code <- 1 : 20
CAMEO_label <- c( "Make public statement", "Appeal", "Express intent to cooperate",
                  "Consult", "Engage in diplomatic cooperation", 
                  "Engage in diplomatic cooperation", "Provide aid", "Yield", 
                  "Investigate", "Demand", "Disapprove", "Reject", "Threaten", 
                  "Protest", "Exhibit force posture", "Reduce relations", "Coerce",
                  "Assault", "Fight", "Use unconventional mass violence")

full_BBC_events_with_meta[ , root_code := mapvalues( root_code, CAMEO_root_code, CAMEO_label ) ]

# Country names:

table( full_BBC_events_with_meta$placename )
table( full_BBC_events_with_meta$countryname )
table( full_BBC_events_with_meta$statename )





# Visualizing the data ----------------------------------------------------

# Many dimensions with many sub-categories each... hard to pick what to show.

# Averaging goldstein ratings over country and by year:
goldstein_country_in_continent_by_year <- data.table( aggregate( cbind( goldstein, total_sources ) ~ 
                                                                   year + countryname + continent,
                                                                 FUN = mean,
                                                                 data = full_BBC_events_with_meta ) )
# goldstein_country_in_continent_by_year <- goldstein_country_in_continent_by_year[ order( continent, -goldstein ), ]
# goldstein_country_in_continent_by_year[ , ]

# And now collapsing across years:
goldstein_country_in_continent <- data.table( aggregate( cbind( goldstein, total_sources ) ~ 
                                                                   countryname + continent,
                                                                 FUN = mean,
                                                                 data = full_BBC_events_with_meta ) )
goldstein_country_in_continent <- goldstein_country_in_continent[ order( continent, goldstein ), ]
corect_order <- unique( goldstein_country_in_continent$countryname )

setnames( goldstein_country_in_continent, 
          c( "goldstein", "total_sources" ), 
          c( "country_goldstein", "country_total_sources" ) )

two_level_aggregates <- join( goldstein_country_in_continent_by_year, 
                              goldstein_country_in_continent )
two_level_aggregates[ , countryname := ordered( countryname, levels = corect_order ) ] 

# First 5 years seem to have next to no data. 
# Since they are not particularly interesting, they shall be removed.

# Check spread for each country, in this pseudo-caterpillar plot which includes information on all the continents plotted together:

png( "CaterpillarGeneral.png", width = 1000, height = 2400 )
ggplot( two_level_aggregates[ year > 1977, ], 
        aes( x = goldstein, y = countryname, 
             group = countryname, color = continent ) ) +
  geom_vline( xintercept = 0, color = "black", lty = "dashed", size = 0.3 ) +
  geom_vline( xintercept = -5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_vline( xintercept = 5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_line( stat = "identity" ) +
  geom_point( aes( x = country_goldstein, y = countryname, color = continent ) ) +
  # facet_wrap( ~ continent, ncol = 6 ) + 
  xlim( -10, 10 ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = 12 ),
         text = element_text( size = 22 ) ) +
  guides( color = guide_legend( ncol = 1 ) ) +
  labs( color = "Area", x = "Average Goldstein rating", y = "Country" )
dev.off()

# Here we split the graph above by continent - and show each one separately within its own panel:
png( "CaterpillarPlotByContinent.png", width = 2800, height = 2400 )
ggplot( two_level_aggregates[ year > 1977, ], 
        aes( x = goldstein, y = countryname, 
             group = countryname, color = continent ) ) +
  geom_vline( xintercept = 0, color = "black", lty = "dashed", size = 0.3 ) +
  geom_vline( xintercept = -5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_vline( xintercept = 5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_line( stat = "identity" ) +
  geom_point( aes( x = country_goldstein, y = countryname, color = continent ) ) +
  facet_wrap( ~ continent, ncol = 4, scales = "free" ) + 
  xlim( -10, 10 ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = 12 ),
         text = element_text( size = 22 ) ) +
  guides( color = FALSE ) +
  labs( color = "Area", x = "Average Goldstein rating", y = "Country" )
dev.off()




# Using violin plots to show whether some countries spend more time at the extremes of the Goldstein scale.
# Let's pick a couple of continents / areas for this:
png( "ViolinCentralEurope.png", width = 2600, height = 3000 )
ggplot( two_level_aggregates[ year > 1977 & continent == "Central Europe", ], 
        aes( y = goldstein, x = countryname, 
             group = countryname, fill = countryname ) ) +
  geom_hline( yintercept = 0, color = "black", lty = "dashed", size = 0.3 ) +
  geom_hline( yintercept = -5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_hline( yintercept = 5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_violin( position = "dodge", 
               draw_quantiles = c( 0.25, 0.50, 0.75), 
               trim = TRUE ) +
  geom_text( aes( x = countryname, y = 8.5, label = countryname ), angle = 90, size = 23 ) +
  ylim( -10, 10 ) +
  guides( fill = FALSE ) +
  theme( axis.text.y = element_text( size = 60 ),
         axis.title.y = element_text( size = 70 ),
	 axis.title.x = element_blank(),
	 axis.text.x = element_blank(),
	 axis.ticks.x = element_blank()	) +
  labs( y = "Average Goldstein rating" )
dev.off()


png( "ViolinWesternEurope.png", width = 3000, height = 3000 )
ggplot( two_level_aggregates[ year > 1977 & continent == "Western Europe", ], 
        aes( y = goldstein, x = countryname, 
             group = countryname, fill = countryname ) ) +
  geom_hline( yintercept = 0, color = "black", lty = "dashed", size = 0.3 ) +
  geom_hline( yintercept = -5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_hline( yintercept = 5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_violin( position = "dodge", 
               draw_quantiles = c( 0.25, 0.50, 0.75), 
               trim = TRUE ) +
  geom_text( aes( x = countryname, y = 8.5, label = countryname ), angle = 90, size = 23 ) +
  ylim( -10, 10 ) +
  guides( fill = FALSE ) +
  theme( axis.text.y = element_text( size = 60 ),
	 axis.title.y = element_text( size = 70 ),
	 axis.title.x = element_blank(),
	 axis.text.x = element_blank(),
	 axis.ticks.x = element_blank()	) +
  labs( y = "Average Goldstein rating" )
dev.off()


# Ridiculously overplotted, and in such a way that merely setting alpha or changing symbols will not help. Also cannot use facetting really, since we already *are* within a facet (i.e., Western Europe):
png( "TimeTrendPerCountry_Overcrowded.png", width = 1800, height = 1200 )
ggplot( two_level_aggregates[ year > 1977 & continent == "Western Europe", ], 
        aes( y = goldstein, x = year, 
             group = countryname, color = countryname ) ) +
  geom_point() + geom_line( stat = "identity" ) +
  guides( color = guide_legend( ncol = 1 ) ) +
  ylim( -10, 10 ) + 
  theme( text = element_text( size = 35 ) ) +
  labs( color = "Country", y = "Average Goldstein rating", group = "Country" )
dev.off()



# So what can we do to streamline all this information and present it gradually, but without creating extremely coarse-grained aggregates (e.g., continent-wide Goldstein averages for each year)? 
# A rudimentary method we can try is to just select a few countries we are interested in
png( "TimeTrendForHandfulOfCountries.png", width = 1800, height = 1200 )
ggplot( two_level_aggregates[ year > 1977 & 
                                countryname %in% c( "France", "Italy",
                                                    "Finland", "Spain" ), ], 
        aes( y = goldstein, x = year, 
             group = countryname, 
             color = countryname ) ) +
  geom_point() + geom_line( stat = "identity", size = 4 ) +
  theme( text = element_text( size = 35 ) ) +
  labs( color = "Country", y = "Average Goldstein rating", group = "Country" ) +
  ylim( -10, 10 )  
dev.off()



# Ok. But how can we insert both 'event importance' information, AND show changes in Goldstein ratings over time?
# png( "test1.png", width = 1800, height = 1200 )
ggplot( goldstein_country_in_continent[ continent == "Western Europe", ], 
        aes( x = country_goldstein, y = countryname, 
             group = countryname, 
             color = countryname ) ) +
  geom_point() + guides( color = FALSE ) +
  xlim( -10, 10 ) 
# dev.off()





# > unique(goldstein_country_in_continent_by_year$continent)
# [1] "Antarctic"                 "Arabian Peninsula"         "Australia and New Zealand" "Canada"                   
# [5] "Caribbean"                 "Central Africa"            "Central Asia"              "Central Europe"           
# [9] "Eastern Africa"            "Eastern Europe"            "Mashriq"                   "Meso-America"             
# [13] "North Africa"              "NW Pacific and East Asia"  "Polar"                     "South America"            
# [17] "South Asia"                "South Pacific"             "Southeast Asia"            "Southern Africa"          
# [21] "US"                        "Western Africa"            "Western Europe"            "Western Indian Ocean" 

p <- ggplot( goldstein_country_in_continent_by_year[ continent == "Western Europe" | continent == "South Asia", ], 
             aes( x = goldstein, y = countryname, 
                  group = countryname, 
                  color = countryname,
                  size = total_sources,
                  frame = year,
                  cumulative = FALSE ) ) +
  geom_vline( xintercept = 0, color = "black", lty = "dashed", size = 0.3 ) +
  geom_vline( xintercept = -5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_vline( xintercept = 5, color = "black", lty = "dashed", size = 0.3 ) +
  geom_point( ) + guides( color = FALSE ) +
  xlim( -10, 10 ) +
  theme( text = element_text( size = 16 ) ) +
  labs( size = "Event\n importance", x = "Average Goldstein rating", y = "Country" ) +
  facet_wrap( ~ continent, ncol = 2, scales = "free"  )

gganimate( p, filename = "~/Desktop/WesternEuropeVsSouthAsia.gif", ani.width = 1400, ani.height = 500 ) # , interval = 1.5




# Now can swap this subset for another:

p2 <- p %+% goldstein_country_in_continent_by_year[ continent == "Western Africa" | continent == "Eastern Africa", ]
gganimate( p2, filename = "~/Desktop/EasternVsWesternAfrica.gif", ani.width = 1200, ani.height = 500 ) 


p3 <- p %+% goldstein_country_in_continent_by_year[ continent == "Western Europe" | continent == "Central Europe", ]
gganimate( p3, filename = "~/Desktop/CentralVsWesternEurope.gif", ani.width = 1400, ani.height = 500 ) 



# save.image( "HistoricalDataRObjects.RData" )




