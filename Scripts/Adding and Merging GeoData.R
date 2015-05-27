library(RDSTK)
library(dplyr)
# Import dla geocoordinates
dla.geocoordinates.1of3 <- tbl_df(read.csv("~/Documents/R Programming/data-1033-program/DataSets/dla.geocoordinates.1of3.csv", stringsAsFactors=FALSE))
# Remove NAs

# Delete a column
dla.geocoordinates.1of3 <- dla.geocoordinates.1of3[,2:4]

# Get political info JSON
coordinates2politics( 61.21806,-149.9003)

# Use JSONLite to convert from JSON to list
j <- fromJSON(anchorage.normalized,simplifyVector = FALSE)

# Convert list to dataframe
j <- as.data.frame(j)
j <- tbl_df(j)
View(j)




