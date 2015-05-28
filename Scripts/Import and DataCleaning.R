
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)
library(ggvis)


################################################################
#IMPORT and Data Wrangling: DLA Dataset
################################################################

# Import DLA dataset
dla.orig <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033_DLA_data_as_of_march_2015.csv", stringsAsFactors=FALSE)

# Rename and convert to dataframe
dla <- tbl_df(dla.orig)

# Rename "station.name..LEA."
colnames(dla)[2]<- "station.name"

## Clean Up Acquisition.Value data ##
  # Remove Commas and dollar signs from Cost Acquistion
  dla$Acquisition.Value <- gsub('\\$|,', '', dla$Acquisition.Value)
  
  # Convert to numeric
  dla$Acquisition.Value <- as.numeric(dla$Acquisition.Value )


## Clean Up Ship.Date

# 1. Remove Time of Day
      t <- dla$Ship.Date
      str_sub(t, -12, -1) <- ""

# 2. Then spell out each month
      t<- gsub("Jan","January", t)
      t <- gsub("Feb","February", t)
      t<- gsub("Mar","March", t)
      t <- gsub("Apr","April", t)
      t <- gsub("May","May", t)
      t <- gsub("Jun","June", t)
      t <- gsub("Jul","July", t)
      t <- gsub("Aug","August", t)
      t <- gsub("Sept","September", t)
      t <- gsub("Oct","October", t)
      t <- gsub("Nov","November", t)
      t <- gsub("Dec","December", t)

# 3. Then convert to dates
      t <- mdy(t)

# 4. Reassign to dataframe
      dla$Ship.Date <- t 


## Add some columns ###
  # - Add COST column
  dla<- mutate(dla, Calculated.Cost = Acquisition.Value * Quantity)
  
  # Add UPDATED column 
  dla$Updated_On <- mdy("03-15-2015")


################################################################
IMPORT and Data Wrangling: State Names
################################################################
# ------------ State Names --------------------- #
# The original dataset doesn't list full state names. So import and add state names table.

# Import State Names
y <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/us_states abbreviations.csv", header=FALSE, stringsAsFactors=FALSE)

# Datawrangle State Names Dataset #
# In new dataset, convert to dataframe
y<- tbl_df(y)  

# Assign  Column Names, remove extra column
colnames(y)[2] <- "state.proper"
colnames(y)[3] <- "State"

# Delete First Column
y <- y[2:3]

# JOIN: Add State names to main dataset
dla<- left_join(dla,y)

################################################################
IMPORT and Data Wrangling: FSG and FSGC Categories
################################################################

# This New Metadata is associated with official FSG and FSGC codes (the numbers at the 
# beginning of the NSN), so they must be extracted from NSN in main dataset

# Purpose: Prepare main dataset for import
  # Create new column FSG, then extract FSG codes to new column  
  dla["FSG"]<-substr(dla$NSN, 1,2)
  
  # convert to numeric
  dla$FSG<- as.integer(dla$FSG)

# In main dataset extract FSGC codes to new column and convert to numeric
dla$FSGC <- substr(dla$NSN,1,4)
dla$FSGC <- as.integer(dla$FSGC)

#------- Import Official FSGC Codes ------#

# Import new data and convert to dataframe
FSGC.codes <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033 Categories - FSGC Codes.csv", stringsAsFactors=FALSE)
FSGC.codes <- tbl_df(FSGC.codes)

#Join the the tables
dla<- left_join(dla,FSGC.codes)

#------- Import CTM's FSGC Metadata: FSG Categories ------#

#Import based on CTM's FSG meta categories
#Import dataset
FSG.categories <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033 Categories - FSGC_Metadata.csv", stringsAsFactors=FALSE)

# This has some extra blank columns. Delete them
FSG.categories <- select(FSG.categories, c(FSG, FSG.title, FSG.categorized))

# Convert to dframe
FSG.categories <- tbl_df(FSG.categories)

# Merge FSG categories with dla 
dla<- left_join(dla, FSG.categories)
View(dla)

#------- Import CTM's FSGC Metadata: Agency Authority ------#

# Purpose: Prepare main dataset for import
# Create Agency Authority Column
dla$agency.authority <- NA
colnames(dla)

# Assign Metadata Based on Agency Name

# Tag "Agency Authority" with my own categories, based on "Agency Name"
# I have no idea how to organize this better

dla$agency.authority[grepl(" pd", dla$station.name, ignore.case=TRUE)] "Police"
dla$agency.authority[grepl("police", dla$station.name, ignore.case=TRUE)] <- "Police" 
dla$agency.authority[grepl("inv unit", dla$station.name, ignore.case=TRUE)] <- "Police" 
dla$agency.authority[grepl("METRO PD", dla$station.name, ignore.case=TRUE)] <- "Police" 
dla$agency.authority[grepl("DEPT OF LAW ENF", dla$station.name, ignore.case=TRUE)] <- "Police" 
dla$agency.authority[grepl("municip", dla$station.name, ignore.case=TRUE)] <- "Police" 
dla$agency.authority[grepl("WASHINGTON TWP PD TOLEDO", dla$station.name, ignore.case=TRUE)] <- "Police"
dla$agency.authority[grepl("HEIGHTS PD", dla$station.name, ignore.case=TRUE)] <- "Police"
dla$agency.authority[grepl("TOWNSHIP PD", dla$station.name, ignore.case=TRUE)] <- "Police"
dla$agency.authority[grepl("GOBIERNO DE QUEBRADILLAS/POLICIA", dla$station.name, ignore.case=TRUE)] <- "Police"


dla$agency.authority[grepl("trooper", dla$station.name, ignore.case=TRUE)] <- "Trooper" 
dla$agency.authority[grepl("mounted", dla$station.name, ignore.case=TRUE)] <- "Trooper" 

dla$agency.authority[grepl("sheriff", dla$station.name, ignore.case=TRUE)] <- "Sheriff" 
dla$agency.authority[grepl("SHERRIFF", dla$station.name, ignore.case=TRUE)] <- "Sheriff" 
dla$agency.authority[grepl("SHERRIF", dla$station.name, ignore.case=TRUE)] <- "Sheriff" 

dla$agency.authority[grepl("constable", dla$station.name, ignore.case=TRUE)] <- "Constable"

dla$agency.authority[grepl("marshal", dla$station.name, ignore.case=TRUE)] <- "Marshals" 

dla$agency.authority[grepl("highway", dla$station.name, ignore.case=TRUE)] <- "Highway Patrol"
dla$agency.authority[grepl("CHIP", dla$station.name, ignore.case=FALSE)] <- "Highway Patrol"
dla$agency.authority[grepl("STATE PATROL", dla$station.name, ignore.case=FALSE)] <- "Highway Patrol"

dla$agency.authority[grepl("safety", dla$station.name, ignore.case=TRUE)] <- "Public Safety"
dla$agency.authority[grepl("DEPT OF PUB SAF", dla$station.name, ignore.case=TRUE)] <- "Public Safety"
dla$agency.authority[grepl("DEPT OF PUBLIC SAFETY", dla$station.name, ignore.case=TRUE)] <- "Public Safety"
View(dla)

dla$agency.authority[grepl("drug", dla$station.name, ignore.case=TRUE)] <- "Drug Task Force"
dla$agency.authority[grepl("narcotics", dla$station.name, ignore.case=TRUE)] <- "Drug Task Force"
dla$agency.authority[grepl("dtf", dla$station.name, ignore.case=TRUE)] <- "Drug Task Force"
dla$agency.authority[grepl("ntf", dla$station.name, ignore.case=TRUE)] <- "Drug Task Force"

dla$agency.authority[grepl("correction", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("prison", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("detention", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("PENITENTIARY", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("probation", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("Jail", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("VT CRIMINAL JUSTICE TRN COUN", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"
dla$agency.authority[grepl("Court", dla$station.name, ignore.case=TRUE)] <- "Court and Corrections"

dla$agency.authority[grepl("COUNTY DA", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
#dla$agency.authority[grepl("DA", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("ATTORNEY", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("CO LAW ENF", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("ATTY GEN", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("PROSECUTOR", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("DA'S OFFICE", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("LOS ANGELES DA", dla$station.name, ignore.case=TRUE)] <- "District Attorney"
dla$agency.authority[grepl("TN 9TH JUDICIAL DISTRICT DA", dla$station.name, ignore.case=TRUE)] <- "District Attorney"



dla$agency.authority[grepl("METRO", dla$station.name, ignore.case=TRUE)] <- "Local Services"
dla$agency.authority[grepl("YOUTH AFFAIRS", dla$station.name, ignore.case=TRUE)] <- "Local Services"

dla$agency.authority[grepl("Arson", dla$station.name, ignore.case=TRUE)] <- "Fire Dept"
dla$agency.authority[grepl("Fire", dla$station.name, ignore.case=TRUE)] <- "Fire Dept"

dla$agency.authority[grepl("airport", dla$station.name, ignore.case=TRUE)] <- "Airports"


dla$agency.authority[grepl("county", dla$station.name, ignore.case=TRUE)] <- "County"
dla$agency.authority[grepl("coroner", dla$station.name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
dla$agency.authority[grepl("animal", dla$station.name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
dla$agency.authority[grepl("PORT DIST HAR", dla$station.name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"

dla$agency.authority[grepl("agriculture", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("Sergeant", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("RIVER AND BAY", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("MOTOR VEHICLE", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("IL DEPT OF CENTRAL MANAGEMENT", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("pardons", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("tobacco", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"

dla$agency.authority[grepl("DEPT OF VA", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("US EPA", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("Postal", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("customs", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("usps", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("US DEPT AGRIC", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("DEPT OF HUD", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$agency.authority[grepl("IRS EL PASO", dla$station.name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"


dla$agency.authority[grepl("aapc", dla$station.name, ignore.case=TRUE)] <- "Unknown"
dla$agency.authority[grepl("GLEN CANYON", dla$station.name, ignore.case=TRUE)] <- "Unknown"


dla$agency.authority[grepl("doj", dla$station.name, ignore.case=TRUE)] <- "DOJ"
dla$agency.authority[grepl("DEPARTMENT OF JUSTICE", dla$station.name, ignore.case=TRUE)] <- "DOJ"
dla$agency.authority[grepl("DEA", dla$station.name, ignore.case=TRUE)] <- "DEA"
dla$agency.authority[grepl("fbi", dla$station.name, ignore.case=TRUE)] <- "FBI"
dla$agency.authority[grepl("DHS", dla$station.name, ignore.case=TRUE)] <- "DHS"
dla$agency.authority[grepl("atf", dla$station.name, ignore.case=TRUE)] <- "ATF"
dla$agency.authority[grepl("US ICE", dla$station.name, ignore.case=TRUE)] <- "DHS"
dla$agency.authority[grepl("Border", dla$station.name, ignore.case=TRUE)] <- "DHS"
dla$agency.authority[grepl("US DOI", dla$station.name, ignore.case=TRUE)] <- "Dept of Interior"
dla$agency.authority[grepl("doi bur", dla$station.name, ignore.case = TRUE)] <- "Dept of Interior"
dla$agency.authority[grepl("DEPT OF STATE", dla$station.name, ignore.case = TRUE)] <- "Dept of State"
dla$agency.authority[grepl("immigration", dla$station.name, ignore.case=TRUE)] <- "DHS"
dla$agency.authority[grepl("HOMELAND SEC", dla$station.name, ignore.case=TRUE)] <- "DHS"
dla$agency.authority[grepl("DEPT OF TRANSPORTATION", dla$station.name, ignore.case=TRUE)] <- "DOT"
dla$agency.authority[grepl("US CIA", dla$station.name, ignore.case=TRUE)] <- "CIA"
dla$agency.authority[grepl("treasury", dla$station.name, ignore.case=TRUE)] <- "Treasury"


dla$agency.authority[grepl("park", dla$station.name, ignore.case=TRUE)] <- "Park Service"
dla$agency.authority[grepl("ranger", dla$station.name, ignore.case=TRUE)] <- "Park Service"
dla$agency.authority[grepl("marine", dla$station.name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$agency.authority[grepl("conservation", dla$station.name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$agency.authority[grepl("fish", dla$station.name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$agency.authority[grepl("Forest", dla$station.name, ignore.case=TRUE)] <- "Forest Service"
dla$agency.authority[grepl("CONSERV AUTH", dla$station.name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$agency.authority[grepl("wildlife", dla$station.name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$agency.authority[grepl("PR DEPT OF NATURAL & ENV RESOURCES", dla$station.name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$agency.authority[grepl("National", dla$station.name, ignore.case=TRUE)] <- "Forest Service"
dla$agency.authority[grepl("GA DNR SOCIAL CIRCLE", dla$station.name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$agency.authority[grepl("DNR", dla$station.name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$agency.authority[grepl("NATURAL RESOURCES", dla$station.name, ignore.case=TRUE)] <- "Fish and Wildlife"


dla$agency.authority[grepl("college", dla$station.name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$agency.authority[grepl("university", dla$station.name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$agency.authority[grepl("UNIV", dla$station.name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$agency.authority[grepl("SCH DIST PD", dla$station.name, ignore.case=TRUE)] <- "Colleges, Universities"

dla$agency.authority[grepl("academy", dla$station.name, ignore.case=TRUE)] <- "Training Academies"
dla$agency.authority[grepl("TNG CTR", dla$station.name, ignore.case=TRUE)] <- "Training Academies"
dla$agency.authority[grepl("TNG CENTER", dla$station.name, ignore.case=TRUE)] <- "Training Academies"
dla$agency.authority[grepl("TRNG CTR", dla$station.name, ignore.case=TRUE)] <- "Training Academies"

dla$agency.authority[grepl("tribal", dla$station.name, ignore.case=TRUE)] <- "Indian"
dla$agency.authority[grepl("YAKAMA NATION", dla$station.name, ignore.case=TRUE)] <- "Indian"
dla$agency.authority[grepl("indian", dla$station.name, ignore.case=TRUE)] <- "Indian"
dla$agency.authority[grepl("CITIZENS POTAWATOMI NATION PD", dla$station.name, ignore.case=TRUE)] <- "Indian"

dla$agency.authority[grepl("secret service", dla$station.name, ignore.case=TRUE)] <- "Secret Service"
dla$agency.authority[grepl("BUREAU OF INVESTIGATIO", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("EMERGENCY MGMT", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("OF REVENUE", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("livestock", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("DEPT OF HEALTH AND HUMAN", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$agency.authority[grepl("DEPT OF MGMT SVC", dla$station.name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"


dla$agency.authority[grepl("RAILWAY", dla$station.name, ignore.case=TRUE)] <- "Unusual"
dla$agency.authority[grepl("LIVE STOCK BOARD", dla$station.name, ignore.case=TRUE)] <- "Unusual"
dla$agency.authority[grepl("SKI VALLEY", dla$station.name, ignore.case=TRUE)] <- "Unusual"
dla$agency.authority[grepl("HOSPITAL PD", dla$station.name, ignore.case=TRUE)] <- "Unusual"

dla$agency.authority[grepl("GAMING AGENCY", dla$station.name, ignore.case=TRUE)] <- "Unusual"


dla$agency.authority[grepl("harbor", dla$station.name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$agency.authority[grepl("river", dla$station.name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$agency.authority[grepl("lake", dla$station.name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$agency.authority[grepl("resevoir", dla$station.name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$agency.authority[grepl("port", dla$station.name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$agency.authority[grepl("HARBORMASTER", dla$station.name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"




#---------- This is a test for NAs -----------------
#agency.authority.test <-
# dla %>%
# select(station.name, agency.authority, agency.jurisdiction)  %>%
# filter(is.na(agency.authority))
#View(distinct(agency.authority.test))


# ################################ ADD METADATA: Agency Jurisdiction ###################################################
# Tag "Agency Jurisdiction" with my own categories, based on "Agency Name"

### Import new dataset Agency Jurisdiction/Agency Authority dataset
tbl.agency.authority <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033 Categories - Agency Authority.csv", stringsAsFactors=FALSE)


# JOIN Agency Jurisdiction
dla<- left_join(dla,tbl.agency.authority)


# Correct Agency Jurisdiction Missing values
dla$agency.jurisdiction[grepl("DEA", dla$agency.authority, ignore.case = FALSE)] <- "Federal"
dla$agency.jurisdiction[grepl("FBI", dla$agency.authority, ignore.case = FALSE)] <- "Federal"
dla$agency.jurisdiction[grepl("DOT", dla$agency.authority, ignore.case = FALSE)] <- "Federal"
dla$agency.jurisdiction[grepl("CIA", dla$agency.authority, ignore.case = FALSE)] <- "Federal"


dla$agency.jurisdiction[grepl("corrections", dla$agency.authority, ignore.case = TRUE)] <- "Other"

dla$agency.jurisdiction[grepl("Fish", dla$agency.authority, ignore.case = TRUE)] <- "State"
dla$agency.jurisdiction[grepl("Park", dla$agency.authority, ignore.case = TRUE)] <- "State"
dla$agency.jurisdiction[grepl("Coroner", dla$agency.authority, ignore.case = TRUE)] <- "County"
dla$agency.jurisdiction[grepl("County", dla$agency.authority, ignore.case = TRUE)] <- "County"
dla$agency.jurisdiction[grepl("Unknown", dla$agency.authority, ignore.case = TRUE)] <- "Unknown"
dla$agency.jurisdiction[grepl("Interior", dla$agency.authority, ignore.case = TRUE)] <- "Federal"
dla$agency.jurisdiction[grepl("Forest", dla$agency.authority, ignore.case = TRUE)] <- "Federal"
dla$agency.jurisdiction[grepl("State", dla$agency.authority, ignore.case = TRUE)] <- "State"
dla$agency.jurisdiction[grepl("District", dla$agency.authority, ignore.case = TRUE)] <- "County"
dla$agency.jurisdiction[grepl("Highway Patrol", dla$agency.authority, ignore.case = TRUE)] <- "State"
dla$agency.jurisdiction[grepl("Fire Dept", dla$agency.authority, ignore.case = TRUE)] <- "Municipal"
dla$agency.jurisdiction[grepl("TWP", dla$agency.authority, ignore.case = TRUE)] <- "Municipal"


dla$agency.jurisdiction[grepl("forest", dla$agency.authority, ignore.case = TRUE)] <- "Federal"
dla$agency.jurisdiction[grepl("indian", dla$agency.authority, ignore.case = TRUE)] <- "Tribal"
dla$agency.jurisdiction[grepl("Secret Service", dla$agency.authority, ignore.case = TRUE)] <- "Federal"
dla$agency.jurisdiction[grepl("DHS", dla$agency.authority, ignore.case = TRUE)] <- "Federal"
dla$agency.jurisdiction[grepl("Public Safety", dla$agency.authority, ignore.case = TRUE)] <- "State"



#  TEST FOR MISSING VALUYES IN Jurisdiction Show NAs #####################
#agency.jurisdiction.test <-
 # dla %>%
  #select(station.name, agency.authority, agency.jurisdiction)  %>%
  #filter(is.na(agency.jurisdiction))
#View(distinct(agency.jurisdiction.test))

# ################################ ADD METADATA: State Regions ###################################################

# Import State Regions and subregions
state.regions <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033 Categories - State Regions.csv", stringsAsFactors=FALSE)
colnames(state.regions)


#Join 
dla <- left_join(dla, state.regions)

### End ###

# write.csv(dla, "~/Documents/R Programming/data-1033-program/DataSets/1033_DLA_data_as_of_march_2015_CLEANED.csv")

