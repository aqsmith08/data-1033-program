# Source: https://www.fcc.gov/developers/census-block-conversions-api
# http://data.fcc.gov/api/block/find?latitude=[latitude]&longitude=[longitude]&showall=[true/false]
library(dplyr)
library(RCurl)

# Read in data from online GitHub Source File
#x <- getURL("https://raw.githubusercontent.com/aqsmith08/data-1033-program/master/DataSets/dla.csv")

# Had to get the file because I got this error: 
# Error in function (type, msg, asError = TRUE)  : 
#   Unknown SSL protocol error in connection to raw.githubusercontent.com:44

dla <- read.csv("dla.csv", header = TRUE)
dla <- tbl_df(dla)

# Re-Establish dataset of addresses
addresses <- distinct(select(dla, Station.Name, lat, lon))

# Build a FCC URL column based on the lat/lon and the template below 
# http://data.fcc.gov/api/block/find?latitude=[latitude]&longitude=[longitude]&showall=[true/false]
# Create template parts
x <- "http://data.fcc.gov/api/block/find?latitude="
y <- "&longitude="
z <- "&showall=[true]"

# Paste content and template parts to create completed url
fcc.url <- paste0(x,addresses$lat,y,addresses$lon,z)

# bind with original dataset
fcc.url  <- as.data.frame(fcc.url)
addresses <- bind_cols(addresses,fcc.url)

# Question: Now how do I download the column of urls?
# I've tried using GET (RCURL library) but can't seem to get it to work....
addresses$getURL <- sapply(addresses$fcc.url, function(x) getURL(x))

# Saving the addresses dataframe because it took so long
# Strongly recommend you don't run the command below
# I'm commenting it out so it's safer :)
# write.csv(addresses, file = "addresses_with_xml.csv")