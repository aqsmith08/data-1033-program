library(XML)
library(dplyr)


# Read in data from FCC
addresses_with_xml <- read.csv("~/Documents/R Programming/Repositories/data-1033-program/DataSets/addresses_with_xml.csv", stringsAsFactors=FALSE)

# There are bad responses! Set bad responses to NA
addresses_with_xml$getURL[grepl("Not Found", addresses_with_xml$getURL)] <- NA


# Good XML Responses
addresses.ok <- addresses_with_xml[(complete.cases(addresses_with_xml$getURL)),]
  # glimpse(addresses.ok)
  # 6070 ok cases

# Bad XML Responses
addresses.not.ok <- addresses_with_xml[!(complete.cases(addresses_with_xml$getURL)),]
   # glimpse(addresses.not.ok)
   # 489 not ok cases

# BIG LOOP!
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

# ToDo: Merge into main dataframe
# When merging, check total row numbers to ensure merge was successful

# Merge "addresses ok" (original data) columns with df (new metadata from FCC)
# Test
nrow(addresses.ok)
nrow(df)

# Combine
addresses_with_xml_and_FCC <- bind_cols(addresses.ok,df)

#Check for NAs one more time, this time in the Block.FIPS field
addresses_with_xml_and_FCC_BAD <- addresses_with_xml_and_FCC[!(complete.cases(addresses_with_xml_and_FCC$Block.FIPS)),]
nrow(addresses_with_xml_and_FCC_BAD)
# 96 More NAs...damn!

# Good Addresses
addresses_with_xml_and_FCC_GOOD <- addresses_with_xml_and_FCC[!(complete.cases(addresses_with_xml_and_FCC$Block.FIPS)),]


# Write to file, for safety
write.csv(addresses_with_xml_and_FCC_GOOD, "~/Documents/R Programming/Repositories/data-1033-program/DataSets/addresses_with_xml_and_FCC.csv")


