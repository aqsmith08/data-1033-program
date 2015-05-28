# This uses variables from Import and DataCleaning.R file! 
# Run that before using this script

# load up the ggmap library
library(ggmap)
library(dplyr)

# Run "Import and DataCleaning" script, then...
# break uniqe dla station names (addresses) into three subgroups...each less than 2,500
# because Google will only bulk geocode 2,500 at a time

##  Batch 1 ##
# Create subgroup based on unique station name
addresses <- distinct(select(dla, station.name))

# alter the Station.Name to address, so Google will recognize it (ggmap constraint)
colnames(addresses)[1] <- "address"

# Create variable for first of three batches (2,500 max0
geo1 <- addresses$address[1:2450,]

# Geocoded: Sunday, May 25
geo1 <- geocode(geo1)

# Oh noes! Google replaced my addresses with lat and lon!
# Merge back the original station names to the new lat/lon data dataset
geo1.addresses <- addresses[1:2450,]

# helpful renaming
geo1.lat_lon <- geo1
geo1.complete <- bind_cols(geo1.addresses, geo1.lat_lon)

# Explore new dataset: How many NAs? 146
nrow(geo1.complete[geo1.complete$lon %in% NA, ])

# Export as csv, in case R crashes
write.csv(geo1.complete, "dla.geocoordinates.1of3.csv")


## Batch 2 ##
# Create subgroup based on unique station names
addresses <- distinct(select(dla, station.name))

# alter the Station.Name to address, so Google will recognize it
colnames(addresses)[1] <- "address"

# Create variable for first of three batches (2,500 max)
geo2 <- addresses[2451:4900,]

#Geocode
geo2 <- geocode(geo2$address)

# Google has replaced our station names!
# Add them back in 
geo2.addresses <- addresses[2451:4900,]

# helpful renaming
geo2.lat_lon <- geo2
geo2.complete <- bind_cols(geo2.addresses, geo2.lat_lon)

# Save, in case R crashes
write.csv(geo2.complete, "dla.geocoordinates.2of3.csv")


## Batch 3 ##
# Create subgroup based on unique station names
addresses <- distinct(select(dla, station.name))

# alter the Station.Name to address, so Google will recognize it
colnames(addresses)[1] <- "address"

# Identify the remaining rows to be geocoded
geo3.address <- addresses$address[4901:6559]

# Geocode
geo3 <- geocode(geo3)

# bind address and lat/long together
geo3.addresses <- addresses[4901:6559,]

# helpful renaming
geo3.lat_lon <- geo3

# bind
geo3.complete <- bind_cols(geo3.addresses, geo3.lat_lon)
geo3.complete <- tbl_df(geo3.complete)


# save to csv, in case R crashes
write.csv(geo3.complete, "~/Documents/R Programming/data-1033-program/DataSets/dla.geocoordinates.3of3.csv")


## Merge three batches for complete list of geocoded unique names
# Read in three files
dla.geocoordinates.1of3 <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/dla.geocoordinates.1of3.csv", stringsAsFactors=FALSE)
dla.geocoordinates.2of3 <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/dla.geocoordinates.2of3.csv")
dla.geocoordinates.3of3 <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/dla.geocoordinates.3of3.csv")



## Clean datasets: alter col names to be identical, in prep for merge
# Check for identical colnames in datasets
names(dla.geocoordinates.1of3)
names(dla.geocoordinates.2of3)
names(dla.geocoordinates.3of3)

# Make colnames consistent
colnames(dla.geocoordinates.2of3)[2] <- "Station.Name"
colnames(dla.geocoordinates.3of3)[2] <- "Station.Name"

# bind three dataframes
dla.geocoordinates.complete <- bind_rows(list(dla.geocoordinates.1of3, dla.geocoordinates.2of3, dla.geocoordinates.3of3))

# write to new file, in case R crashes
write.csv(dla.geocoordinates.complete, "~/Documents/R Programming/data-1033-program/DataSets/dla.geocoordinates.complete.csv")


# Identify the NAs (489); lament
filter(dla.geocoordinates.complete, lon %in% NA) %>%
  arrange(Station.Name)

# Merge with main dataset
dla <- left_join(dla, dla.geocoordinates.complete)

# Save, in case R crashes
write.csv(dla, "~/Documents/R Programming/data-1033-program/DataSets/dla.csv")


### End ####

