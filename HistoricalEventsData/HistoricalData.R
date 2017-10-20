

# Packages ----------------------------------------------------------------



library( data.table )
library( ggplot2 )
library( stringi )
library( stringr )
library( plyr )
library( rworldmap )

options( scipen = 999 )

setwd( "C:/Users/cconsta2/WinToUbuntuAndBackAgain/HistoricalEventsData" )
# load("HistoricalDataRObjects.RData")



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


average_goldstein <- aggregate( cbind( goldstein, total_sources ) ~ 
                                  year + countryname + continent,
                                FUN = mean,
                                data = full_BBC_events_with_meta )

# pdf("C:/Users/cconsta2/Desktop/Test.pdf", width = 25, height = 15 )
ggplot( average_goldstein, 
        aes( y = goldstein, x = as.factor( year ) ) ) +
  geom_boxplot( position = "dodge", varwidth = FALSE ) +
  facet_grid( ~ continent, drop = TRUE ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, size = 0.7 ) )
# dev.off()

# Too much data, so this is illegible. Further work pending.

# save.image("HistoricalDataRObjects.RData")



