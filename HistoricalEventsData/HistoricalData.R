
library( data.table )
library( ggplot2 )
library( stringr )

options( scipen = 999 )

setwd( "/home/u010/c_constant/Documents/BlogPosts/HistoricalEventsData" )

BBC_events <- fread( "BBC_Summary_of_World_Broadcasts_1979_2015_.csv", na.strings = "" )
BBC_meta <- fread( "BBC_Summary_of_World_Broadcasts_1979_2015_MetaData.csv", na.strings = "" )

names(BBC_events)

table( duplicated( BBC_events$aid ) ) # Duplicates here...
table( duplicated( BBC_events$eid ) ) # But not here... Hm.
# This is a combination between news source and event number.
# Which leads me to beliebe that events themselves may be repeated, but covered by different sources.
# In keepng with the a common data management recommendation, values within columns should represent atomic values, i.e., each cell should contain just one values, instead of two. So we shall try to fix this:
BBC_events[ , EIDSource := gsub( "[[:digit:]]","", BBC_events$eid ) ]
BBC_events[ , EIDEvent := gsub( "[[:alpha:]]","", BBC_events$eid ) ]

table( BBC_events$EIDSource )
table( BBC_events$EIDEvent )

summary( BBC_events )
head( BBC_events )

# Exclude events that have no date, or no lat and long:
BBC_events <- BBC_events[ ! is.na( story_date ), ]
BBC_events[ , story_date := as.Date( story_date, format = "%m/%d/%Y" ) ]

BBC_events <- BBC_events[ ! is.na( lat ) & ! is.na( lon ), ]


min( BBC_events$story_date )
max( BBC_events$story_date )

table( BBC_events$quad_class )
hist( BBC_events$goldstein )

length( unique( BBC_events$source ) )
length( unique( BBC_events$target ) )




# These source and target codes dont mean much in themselves. So I'm going to match them up to the explanations from the codebook.

source_target_codes <- fread( "SourceOrTargetCodes_ClineBBCData.csv" )

BBC_events[ , target := str_trim( str_extract( target, "[[:alpha:]]+" ), side = "both" ) ]
BBC_events[ , source := str_trim( str_extract( source, "[[:alpha:]]+" ), side = "both" ) ]

# Gotta split strings into groups of 3... This is not working yet:
str_replace( head( BBC_events$target ), "(.{3})", "\\1" )
# gsub("(.{3})", "\\1", head( BBC_events$target ) )
