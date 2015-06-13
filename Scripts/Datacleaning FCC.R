library(XML)
library(dplyr)
library(stringr)

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

# Geocode using lat/lon 
# Create an "address" column of latitude and longitude for google to recognize
geocoding.bad.a <- all.bad.xml.addresses.from.fcc %>% 
  na.omit() %>%
  mutate(address = str_c(geocoding.bad.a$lat, geocoding.bad.a$lon, sep = ", "))

# Geocode bad addresses based on lon/lat
geocoding.bad.a$Google <- geocode(geocoding.bad.a$address, output = "more")
View(geocoding.bad.a)
# Wow. These results (based on lat/lon) are outside the U.S. in places like Iran. That Did Not Work.

# Try geocoding bad addresses again based on Station Name
# Rename station name file to "address" so google will recognize it
colnames(all.bad.xml.addresses.from.fcc)[2] <- "address"
geocoding.bad.b <- all.bad.xml.addresses.from.fcc
geocoding.bad.b$Google <- geocode(geocoding.bad.b$address, output = "all")
# Examine results
View(geocoding.bad.b)
# Nothing. Mostly zero results, but Google Mpas DOES find the stations when I search on actual website. Do Not Understand.





