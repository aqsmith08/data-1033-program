library(XML)
library(dplyr)
library(stringr)
library(googlesheets)
library(tidyr)
library(RCurl)

# Read in data from FCC
addresses_with_xml <- read.csv("~/Documents/R Programming/Repositories/data-1033-program/DataSets/addresses_with_xml.csv", stringsAsFactors=FALSE)

# There are bad responses! Replace bad response XML (that contains "not found" garbage) with simple NA
addresses_with_xml$getURL[grepl("Not Found", addresses_with_xml$getURL)] <- NA

# Separate good from bad (NA) responses

# Good XML Responses
addresses.ok <- addresses_with_xml[(complete.cases(addresses_with_xml$getURL)),]
  # glimpse(addresses.ok)
  # 6070 ok cases

# Bad XML Responses
addresses.not.ok <- addresses_with_xml[!(complete.cases(addresses_with_xml$getURL)),]
   # glimpse(addresses.not.ok)
   # 489 not ok cases

# BIG LOOP to parse XML data!

# Create a blank dataframe
df <-data.frame()

# Fill blank dataframe with parsed XML data
## Using only "good" records (addresses.ok), parse XML in each row
## ...and bind onto a blank dataframe

for(i in 1:nrow(addresses.ok)){
  data <- xmlParse(addresses.ok$getURL[i])
  xml_data <- xmlToList(data)
  xml_data <- t(unlist(xml_data))
  xml_data <- as.data.frame(xml_data)
  df <- bind_rows(df, xml_data)
}

# New dataframe now contains only parsed XML data
# Merge it back with original dataframe

# Test to make sure each has identical length
nrow(addresses.ok)
nrow(df)

# Merge parsed XML data with original address data
addresses_with_xml_and_FCC <- bind_cols(addresses.ok,df)

# Post-Merge test
#Check for NAs (using Block.FIPS field) and assign any bad results to a new dataset for cleaning
addresses_with_xml_and_FCC_BAD <- addresses_with_xml_and_FCC[!(complete.cases(addresses_with_xml_and_FCC$Block.FIPS)),]
nrow(addresses_with_xml_and_FCC_BAD)
# 96 More NAs...damn!

# Good Addresses: Save good addresses
addresses_with_xml_and_FCC_GOOD <- addresses_with_xml_and_FCC[(complete.cases(addresses_with_xml_and_FCC$Block.FIPS)),]
glimpse(addresses_with_xml_and_FCC_GOOD)

# Write Good Addresses to file, for safety
write.csv(addresses_with_xml_and_FCC_GOOD, "~/Documents/R Programming/Repositories/data-1033-program/DataSets/addresses_with_xml_and_FCC.csv")

# With the good addresses secured, turn back to the bad ones

# There have been a lot of bad addresses. I've lost track.
# Wrangle them all back into one datasest, and fix them (if possible).

# Anti-join: show all addresses in x where there are not matching values in y, keeping just x
x <- addresses_with_xml               # All Addresses, both good and bad
y <- addresses_with_xml_and_FCC_GOOD  # Good Addresses
# This operation should leave only bad addresses
all.bad.xml.addresses.from.fcc <- anti_join( x, y )

# Examine Results
View(all.bad.xml.addresses.from.fcc)
glimpse(all.bad.xml.addresses.from.fcc)  #  585 bad responses
glimpse(addresses_with_xml_and_FCC_GOOD) # 5974 good responses
5974 + 585                               # 6559 total responses: TRUE!
# Synopsis: Result shows that all records accounted for

# Assume all data related to these bad records is Crap.
all.bad.xml.addresses.from.fcc <- all.bad.xml.addresses.from.fcc %>%
  select(address = Station.Name)

write.csv(all.bad.xml.addresses.from.fcc,"~/Documents/R Programming/Repositories/data-1033-program/DataSets/all.bad.xml.addresses.from.fcc.csv")

# Try geocoding bad addresses again based on Station Name. Google Maps recognizes them on the website.
# Why won't it see them on in the API?

# Try Bing
# all.bad.xml.addresses.from.fcc$bing <- geocode(all.bad.xml.addresses.from.fcc, service = "bing", output = "more")
# options(BingMapsKey='Aj8Ml0zfRsOL00abNX4AqYCEr4CzM07lPZhvPGXtq1eIJn9sVwudBUHcUpmp0ZM7')

# And Google (Again)
# all.bad.xml.addresses.from.fcc$google <- geocode(all.bad.xml.addresses.from.fcc, service = "google", output = "more")

# Nothing. Mostly zero results, but Google Maps DOES find the stations when I search on the actual website, one at a time. 
# So I'm off to search these 500+ stations by hand using the website. Awful.

# Upload the bad addresses into a GoogleSheet
# googlesheet.fixing.bad.addresses.by.hand <- "https://docs.google.com/spreadsheets/d/1yrBOX3PrJ2v836AW9_1jPtnHIaOAPia2hoK1WdWYL2s/pubhtml"

# Access the sheet with R (using googlesheets)
# devtools::install_github("jennybc/googlesheets")
 library(googlesheets)

# For first time users of google sheets
#Authenticate My account
gs_auth(new_user = T)

# Register the individual spreadsheet
badg <- gs_title("Bad FCC Addresses")

# Read completed data from  GoogleSpreadsheet (585 rows)
fixing <- gs_read_csv(badg, ws = 1)
colnames(fixing) <- c("station.name", "address")

# Omit NAs (564 rows left)
fixing <- na.omit(fixing)

# Construct FCC addresses
#Split address column
fixing <- fixing %>% separate(address, c("latitude", "longitude"), ",")

# Build a FCC URL column based on the lat/lon and the template below 
# http://data.fcc.gov/api/block/find?latitude=[latitude]&longitude=[longitude]&showall=[true/false]
# Create template parts
x <- "http://data.fcc.gov/api/block/find?latitude="
y <- "&longitude="
z <- "&showall=[true]"

# Paste content and template parts to create completed url
fixing$fcc.url <- paste0(x,fixing$latitude,y,fixing$longitude,z)

# Download list of URLs (Aaron's brilliant code)
fixing.fcc.results <- sapply(fixing$fcc.url, function(x) getURL(x))
fixing.fcc.results <- as.data.frame(fixing.fcc.results)
fixing.fcc.results <- tbl_df(fixing.fcc.results)

# Bind results with stations names
fixing.with.fcc <- bind_cols(fixing, fixing.fcc.results)


# Time to clean, clean clean (Again!)
# Assign not found values "NA"
#fixing.fcc.results[grepl("Not Found", fixing.with.fcc$fixing.fcc.results)] <- NA
fixing.with.fcc <- fixing.with.fcc[(!complete.cases(fixing.with.fcc$fixing.fcc.results)),]

# FCC result column is a factor, convert to character
fixing.with.fcc$fixing.fcc.results <- as.character(fixing.with.fcc$fixing.fcc.results)


# BIG LOOP to parse XML data!

# Create a blank dataframe
df <-data.frame()

# Fill blank dataframe with parsed XML data
## Using only "good" records (addresses.ok), parse XML in each row
## ...and bind onto a blank dataframe

for(i in 1:nrow(fixing.with.fcc)){
  data <- xmlParse(fixing.with.fcc$fixing.fcc.results[i])
  xml_data <- xmlToList(data)
  xml_data <- t(unlist(xml_data))
  xml_data <- as.data.frame(xml_data)
  df <- bind_rows(df, xml_data)
}


# Merge the parsed xml (df) with fixing, the bad dataset with station names
fixed.bad.addresses <- bind_cols(fixing, df)
View(fixed.bad.addresses)
fixed.bad.addresses <- fixed.bad.addresses %>% select(station.name:State.name)
View(fixed.bad.addresses)
write.csv(fixed.bad.addresses, "hand.coded.csv")



# Merge 
# Merge main station name dataset
addresses_with_xml_and_FCC
addresses_with_xml_and_FCC <- addresses_with_xml_and_FCC %>% select(station.name = Station.Name, lat=as.character(lat), lon=as.character(lon), Block.FIPS, County.FIPS, County.name, State.FIPS, State.code, State.name)
glimpse(addresses_with_xml_and_FCC)

# Merge minor dataset
glimpse(fixed.bad.addresses)
fixed.bad.addresses <- fixed.bad.addresses %>% select(station.name, lat = latitude, lon = longitude, Block.FIPS, County.FIPS, County.name, State.FIPS, State.code, State.name)
# Fix lat lon numerics
fixed.bad.addresses$lat <- as.numeric(fixed.bad.addresses$lat)
fixed.bad.addresses$lon <- as.numeric(fixed.bad.addresses$lon)

# Bind
FCC.complete <- bind_rows(addresses_with_xml_and_FCC, fixed.bad.addresses)

# Examine
# There are too many. Must Show distinct only
# distinct(select(FCC.complete, station.name))
# Does not work...distinct rarely does

# Show unique entries only
FCC.complete.uniques <- FCC.complete[!duplicated(FCC.complete$station.name),] %>% arrange(station.name)
write.csv(FCC.complete.uniques,"FCC.complete.uniques.csv" )

# Show duplicate entries only
FCC.complete.duplicates <- FCC.complete[duplicated(FCC.complete$station.name),] %>% arrange(station.name)
write.csv(FCC.complete.duplicates, "FCC.complete.duplicates.csv")

# Check for duplicates with different method: FCC.complete.uniques Tests Good for no duplicate (station.names)
View(FCC.complete.uniques %>% group_by(station.name, County.name) %>% summarise(n = n()) %>% filter(n>1))

# Prepare FCC.complete.uniques dataset for merge with main transaction dataset
# FCC.complete.uniques (6,539 observations)
# Read in cleaned dla transaction data (194,792 observations)
dla <- read.csv("~/Documents/R Programming/Repositories/data-1033-program/DataSets/1033_DLA_data_as_of_march_2015_CLEANED.csv", stringsAsFactors=FALSE)
tbl_df(dla)
# BIG JOIN!
dla.joined <- left_join(dla, FCC.complete.uniques, by = "station.name")
dla.joined <- tbl_df(dla.joined)
write.csv(dla.joined, "1033_DLA_data_cleaned-geocoded_Updated 06-20-2015,1455")

## Note: This file is too large to upload to GitHub and can be found in the google folder, here:
https://drive.google.com/folderview?id=0BxmJlJI0iVpzfmE1QVJhTTZOV0VtWmJMZkhSTlEyYkFBV0JYSG5sYlplS3h5cW1MRFlmQ2c&usp=sharing


