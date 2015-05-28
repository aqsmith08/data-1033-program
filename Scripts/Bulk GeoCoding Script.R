# This uses variables from Import and DataCleaning.R file! 
# Run that before using this script

# load up the ggmap library
library(ggmap)
library(dplyr)

# Batch 1
# get the input data; assumes dla is currently in workspace
# Run "Import and DataCleaning" script, then...
# break dla addresses into three subgroups...each less than 2,500
# because Google will only bulk geocode 2,500 at a time

# Create Subgroup named addresses
addresses <- distinct(select(dla, Station.Name))

# alter the Station.Name to address, so Google will recognize it
colnames(addresses)[1] <- "address"
addresses

# GeoCode first batch of locations- google only allows 2500 ance
#Sunday, May 25
geo1 <- addresses$address[1:2450,]
geo1 <- geocode(geo1)
geo1 <- tbl_df(geo1)

# bind address and lat/long together
geo1.addresses <- addresses[1:2450,]
geo1.lat_lon <- geo1
geo1.complete <- bind_cols(geo1.addresses, geo1.lat_lon)

# NAs
# How many NAs? 146
nrow(geo1.complete[geo1.complete$lon %in% NA, ])
# Save NAs
geo1.complete.nas <- geo1.complete[geo1.complete$lon %in% NA, ]
# Export as csv
write.csv(geo1.complete.nas, "dla.geocoordinates.1of3.nas.csv")


# save to csv, in case R crashes
write.csv(geo1.complete, "dla.geocoordinates.1of3.csv")


# Batch 2
# Run "Import and DataCleaning" script, then
# break dla addresses into three subgroups...each less than 2,500
# because Google will only bulk geocode 25000 at a time

# Create Subgroup named addresses
addresses <- distinct(select(dla, Station.Name))

# alter the Station.Name to address, so Google will recognize it
colnames(addresses)[1] <- "address"

geo2 <- addresses[2451:4900,]
geo2 <- geocode(geo2$address)
geo2 <- tbl_df(geo2)


# bind address and lat/long together
geo2.addresses <- addresses[2451:4900,]
geo2.lat_lon <- geo2
geo2.complete <- bind_cols(geo2.addresses, geo2.lat_lon)

# How many NAs?
nrow(geo2.complete[geo2.complete$lon %in% NA, ])
# Save NAs
geo2.complete.nas <- geo2.complete[geo2.complete$lon %in% NA, ]


# save NA results to a csv, in case R crashes; 
# We need to resolve these NAs if possible

write.csv(geo2.complete, "dla.geocoordinates.2of3.csv")
write.csv(geo2.complete.nas, "dla.geocoordinates.2of3.nas.csv")


# Batch 3
# Run "Import and DataCleaning" script, then
# break dla addresses into three subgroups...each less than 2,500
# because Google will only bulk geocode 25000 at a time

# Create Subgroup named addresses
addresses <- distinct(select(dla, Station.Name))

# alter the Station.Name to address, so Google will recognize it
colnames(addresses)[1] <- "address"

geo3 <- addresses[4901:6559]
geo3 <- geocode(geo3$address)
geo3 <- tbl_df(geo3)


# bind address and lat/long together
geo3.addresses <- addresses[2451:4900,]
geo3.lat_lon <- geo3
geo3.complete <- bind_cols(geo3.addresses, geo3.lat_lon)

# save to csv, in case R crashes
write.csv(geo3.complete, "dla.geocoordinates.3of3.csv")


### End ####

