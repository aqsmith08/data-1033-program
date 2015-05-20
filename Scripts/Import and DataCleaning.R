
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

# Rename "Station.Name..LEA."
colnames(dla)[2]<- "Station.Name"

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

# Reassign to y
y <- FSGC.codes 
str(y)

#Join the the tables
dla<- left_join(dla,y)

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
dla$Agency_Authority <- NA
colnames(dla)

# Assign Metadata Based on Agency Name

# Tag "Agency Authority" with my own categories, based on "Agency Name"
# I have no idea how to organize this better

dla$Agency_Authority[grepl(" pd", dla$Station.Name, ignore.case=TRUE)] "Police"
dla$Agency_Authority[grepl("police", dla$Station.Name, ignore.case=TRUE)] <- "Police" 
dla$Agency_Authority[grepl("inv unit", dla$Station.Name, ignore.case=TRUE)] <- "Police" 
dla$Agency_Authority[grepl("METRO PD", dla$Station.Name, ignore.case=TRUE)] <- "Police" 
dla$Agency_Authority[grepl("DEPT OF LAW ENF", dla$Station.Name, ignore.case=TRUE)] <- "Police" 
dla$Agency_Authority[grepl("municip", dla$Station.Name, ignore.case=TRUE)] <- "Police" 
dla$Agency_Authority[grepl("WASHINGTON TWP PD TOLEDO", dla$Station.Name, ignore.case=TRUE)] <- "Police"
dla$Agency_Authority[grepl("HEIGHTS PD", dla$Station.Name, ignore.case=TRUE)] <- "Police"
dla$Agency_Authority[grepl("TOWNSHIP PD", dla$Station.Name, ignore.case=TRUE)] <- "Police"
dla$Agency_Authority[grepl("GOBIERNO DE QUEBRADILLAS/POLICIA", dla$Station.Name, ignore.case=TRUE)] <- "Police"


dla$Agency_Authority[grepl("trooper", dla$Station.Name, ignore.case=TRUE)] <- "Trooper" 
dla$Agency_Authority[grepl("mounted", dla$Station.Name, ignore.case=TRUE)] <- "Trooper" 

dla$Agency_Authority[grepl("sheriff", dla$Station.Name, ignore.case=TRUE)] <- "Sheriff" 
dla$Agency_Authority[grepl("SHERRIFF", dla$Station.Name, ignore.case=TRUE)] <- "Sheriff" 
dla$Agency_Authority[grepl("SHERRIF", dla$Station.Name, ignore.case=TRUE)] <- "Sheriff" 

dla$Agency_Authority[grepl("constable", dla$Station.Name, ignore.case=TRUE)] <- "Constable"

dla$Agency_Authority[grepl("marshal", dla$Station.Name, ignore.case=TRUE)] <- "Marshals" 

dla$Agency_Authority[grepl("highway", dla$Station.Name, ignore.case=TRUE)] <- "Highway Patrol"
dla$Agency_Authority[grepl("CHIP", dla$Station.Name, ignore.case=FALSE)] <- "Highway Patrol"
dla$Agency_Authority[grepl("STATE PATROL", dla$Station.Name, ignore.case=FALSE)] <- "Highway Patrol"

dla$Agency_Authority[grepl("safety", dla$Station.Name, ignore.case=TRUE)] <- "Public Safety"
dla$Agency_Authority[grepl("DEPT OF PUB SAF", dla$Station.Name, ignore.case=TRUE)] <- "Public Safety"
dla$Agency_Authority[grepl("DEPT OF PUBLIC SAFETY", dla$Station.Name, ignore.case=TRUE)] <- "Public Safety"
View(dla)

dla$Agency_Authority[grepl("drug", dla$Station.Name, ignore.case=TRUE)] <- "Drug Task Force"
dla$Agency_Authority[grepl("narcotics", dla$Station.Name, ignore.case=TRUE)] <- "Drug Task Force"
dla$Agency_Authority[grepl("dtf", dla$Station.Name, ignore.case=TRUE)] <- "Drug Task Force"
dla$Agency_Authority[grepl("ntf", dla$Station.Name, ignore.case=TRUE)] <- "Drug Task Force"

dla$Agency_Authority[grepl("correction", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("prison", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("detention", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("PENITENTIARY", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("probation", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("Jail", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("VT CRIMINAL JUSTICE TRN COUN", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"
dla$Agency_Authority[grepl("Court", dla$Station.Name, ignore.case=TRUE)] <- "Court and Corrections"

dla$Agency_Authority[grepl("COUNTY DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
#dla$Agency_Authority[grepl("DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("ATTORNEY", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("CO LAW ENF", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("ATTY GEN", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("PROSECUTOR", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("DA'S OFFICE", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("LOS ANGELES DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("TN 9TH JUDICIAL DISTRICT DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"



dla$Agency_Authority[grepl("METRO", dla$Station.Name, ignore.case=TRUE)] <- "Local Services"
dla$Agency_Authority[grepl("YOUTH AFFAIRS", dla$Station.Name, ignore.case=TRUE)] <- "Local Services"

dla$Agency_Authority[grepl("Arson", dla$Station.Name, ignore.case=TRUE)] <- "Fire Dept"
dla$Agency_Authority[grepl("Fire", dla$Station.Name, ignore.case=TRUE)] <- "Fire Dept"

dla$Agency_Authority[grepl("airport", dla$Station.Name, ignore.case=TRUE)] <- "Airports"


dla$Agency_Authority[grepl("county", dla$Station.Name, ignore.case=TRUE)] <- "County"
dla$Agency_Authority[grepl("coroner", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
dla$Agency_Authority[grepl("animal", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
dla$Agency_Authority[grepl("PORT DIST HAR", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"

dla$Agency_Authority[grepl("agriculture", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("Sergeant", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("RIVER AND BAY", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("MOTOR VEHICLE", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("IL DEPT OF CENTRAL MANAGEMENT", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("pardons", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("tobacco", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"

dla$Agency_Authority[grepl("DEPT OF VA", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("US EPA", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("Postal", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("customs", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("usps", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("US DEPT AGRIC", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("DEPT OF HUD", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"
dla$Agency_Authority[grepl("IRS EL PASO", dla$Station.Name, ignore.case=TRUE)] <- "Other Federal Agencies (Non Security)"


dla$Agency_Authority[grepl("aapc", dla$Station.Name, ignore.case=TRUE)] <- "Unknown"
dla$Agency_Authority[grepl("GLEN CANYON", dla$Station.Name, ignore.case=TRUE)] <- "Unknown"


dla$Agency_Authority[grepl("doj", dla$Station.Name, ignore.case=TRUE)] <- "DOJ"
dla$Agency_Authority[grepl("DEPARTMENT OF JUSTICE", dla$Station.Name, ignore.case=TRUE)] <- "DOJ"
dla$Agency_Authority[grepl("DEA", dla$Station.Name, ignore.case=TRUE)] <- "DEA"
dla$Agency_Authority[grepl("fbi", dla$Station.Name, ignore.case=TRUE)] <- "FBI"
dla$Agency_Authority[grepl("DHS", dla$Station.Name, ignore.case=TRUE)] <- "DHS"
dla$Agency_Authority[grepl("atf", dla$Station.Name, ignore.case=TRUE)] <- "ATF"
dla$Agency_Authority[grepl("US ICE", dla$Station.Name, ignore.case=TRUE)] <- "DHS"
dla$Agency_Authority[grepl("Border", dla$Station.Name, ignore.case=TRUE)] <- "DHS"
dla$Agency_Authority[grepl("US DOI", dla$Station.Name, ignore.case=TRUE)] <- "Dept of Interior"
dla$Agency_Authority[grepl("doi bur", dla$Station.Name, ignore.case = TRUE)] <- "Dept of Interior"
dla$Agency_Authority[grepl("DEPT OF STATE", dla$Station.Name, ignore.case = TRUE)] <- "Dept of State"
dla$Agency_Authority[grepl("immigration", dla$Station.Name, ignore.case=TRUE)] <- "DHS"
dla$Agency_Authority[grepl("HOMELAND SEC", dla$Station.Name, ignore.case=TRUE)] <- "DHS"
dla$Agency_Authority[grepl("DEPT OF TRANSPORTATION", dla$Station.Name, ignore.case=TRUE)] <- "DOT"
dla$Agency_Authority[grepl("US CIA", dla$Station.Name, ignore.case=TRUE)] <- "CIA"
dla$Agency_Authority[grepl("treasury", dla$Station.Name, ignore.case=TRUE)] <- "Treasury"


dla$Agency_Authority[grepl("park", dla$Station.Name, ignore.case=TRUE)] <- "Park Service"
dla$Agency_Authority[grepl("ranger", dla$Station.Name, ignore.case=TRUE)] <- "Park Service"
dla$Agency_Authority[grepl("marine", dla$Station.Name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$Agency_Authority[grepl("conservation", dla$Station.Name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$Agency_Authority[grepl("fish", dla$Station.Name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$Agency_Authority[grepl("Forest", dla$Station.Name, ignore.case=TRUE)] <- "Forest Service"
dla$Agency_Authority[grepl("CONSERV AUTH", dla$Station.Name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$Agency_Authority[grepl("wildlife", dla$Station.Name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$Agency_Authority[grepl("PR DEPT OF NATURAL & ENV RESOURCES", dla$Station.Name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$Agency_Authority[grepl("National", dla$Station.Name, ignore.case=TRUE)] <- "Forest Service"
dla$Agency_Authority[grepl("GA DNR SOCIAL CIRCLE", dla$Station.Name, ignore.case=TRUE)] <- "Fish and WildLife"
dla$Agency_Authority[grepl("DNR", dla$Station.Name, ignore.case=TRUE)] <- "Fish and Wildlife"
dla$Agency_Authority[grepl("NATURAL RESOURCES", dla$Station.Name, ignore.case=TRUE)] <- "Fish and Wildlife"


dla$Agency_Authority[grepl("college", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$Agency_Authority[grepl("university", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$Agency_Authority[grepl("UNIV", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$Agency_Authority[grepl("SCH DIST PD", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"

dla$Agency_Authority[grepl("academy", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"
dla$Agency_Authority[grepl("TNG CTR", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"
dla$Agency_Authority[grepl("TNG CENTER", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"
dla$Agency_Authority[grepl("TRNG CTR", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"

dla$Agency_Authority[grepl("tribal", dla$Station.Name, ignore.case=TRUE)] <- "Indian"
dla$Agency_Authority[grepl("YAKAMA NATION", dla$Station.Name, ignore.case=TRUE)] <- "Indian"
dla$Agency_Authority[grepl("indian", dla$Station.Name, ignore.case=TRUE)] <- "Indian"
dla$Agency_Authority[grepl("CITIZENS POTAWATOMI NATION PD", dla$Station.Name, ignore.case=TRUE)] <- "Indian"

dla$Agency_Authority[grepl("secret service", dla$Station.Name, ignore.case=TRUE)] <- "Secret Service"
dla$Agency_Authority[grepl("BUREAU OF INVESTIGATIO", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("EMERGENCY MGMT", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("OF REVENUE", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("livestock", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("DEPT OF HEALTH AND HUMAN", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("DEPT OF MGMT SVC", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"


dla$Agency_Authority[grepl("RAILWAY", dla$Station.Name, ignore.case=TRUE)] <- "Unusual"
dla$Agency_Authority[grepl("LIVE STOCK BOARD", dla$Station.Name, ignore.case=TRUE)] <- "Unusual"
dla$Agency_Authority[grepl("SKI VALLEY", dla$Station.Name, ignore.case=TRUE)] <- "Unusual"
dla$Agency_Authority[grepl("HOSPITAL PD", dla$Station.Name, ignore.case=TRUE)] <- "Unusual"

dla$Agency_Authority[grepl("GAMING AGENCY", dla$Station.Name, ignore.case=TRUE)] <- "Unusual"


dla$Agency_Authority[grepl("harbor", dla$Station.Name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$Agency_Authority[grepl("river", dla$Station.Name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$Agency_Authority[grepl("lake", dla$Station.Name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$Agency_Authority[grepl("resevoir", dla$Station.Name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$Agency_Authority[grepl("port", dla$Station.Name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"
dla$Agency_Authority[grepl("HARBORMASTER", dla$Station.Name, ignore.case=TRUE)] <- "Harbors, Dams, Waterways, and Resevoirs"




#---------- This is a test for NAs -----------------
#agency.authority.test <-
# dla %>%
# select(Station.Name, Agency_Authority, Agency_Jurisdiction)  %>%
# filter(is.na(Agency_Authority))
#View(distinct(agency.authority.test))


# ################################ ADD METADATA: Agency Jurisdiction ###################################################
# Tag "Agency Jurisdiction" with my own categories, based on "Agency Name"

### Import new dataset Agency Jurisdiction/Agency Authority dataset
tbl.agency.authority <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033 Categories - Agency Authority.csv", stringsAsFactors=FALSE)


# JOIN Agency Jurisdiction
dla<- left_join(dla,tbl.agency.authority)


# Correct Agency Jurisdiction Missing values
dla$Agency_Jurisdiction[grepl("DEA", dla$Agency_Authority, ignore.case = FALSE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("FBI", dla$Agency_Authority, ignore.case = FALSE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("DOT", dla$Agency_Authority, ignore.case = FALSE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("CIA", dla$Agency_Authority, ignore.case = FALSE)] <- "Federal"


dla$Agency_Jurisdiction[grepl("corrections", dla$Agency_Authority, ignore.case = TRUE)] <- "Other"

dla$Agency_Jurisdiction[grepl("Fish", dla$Agency_Authority, ignore.case = TRUE)] <- "State"
dla$Agency_Jurisdiction[grepl("Park", dla$Agency_Authority, ignore.case = TRUE)] <- "State"
dla$Agency_Jurisdiction[grepl("Coroner", dla$Agency_Authority, ignore.case = TRUE)] <- "County"
dla$Agency_Jurisdiction[grepl("County", dla$Agency_Authority, ignore.case = TRUE)] <- "County"
dla$Agency_Jurisdiction[grepl("Unknown", dla$Agency_Authority, ignore.case = TRUE)] <- "Unknown"
dla$Agency_Jurisdiction[grepl("Interior", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("Forest", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("State", dla$Agency_Authority, ignore.case = TRUE)] <- "State"
dla$Agency_Jurisdiction[grepl("District", dla$Agency_Authority, ignore.case = TRUE)] <- "County"
dla$Agency_Jurisdiction[grepl("Highway Patrol", dla$Agency_Authority, ignore.case = TRUE)] <- "State"
dla$Agency_Jurisdiction[grepl("Fire Dept", dla$Agency_Authority, ignore.case = TRUE)] <- "Municipal"
dla$Agency_Jurisdiction[grepl("TWP", dla$Agency_Authority, ignore.case = TRUE)] <- "Municipal"


dla$Agency_Jurisdiction[grepl("forest", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("indian", dla$Agency_Authority, ignore.case = TRUE)] <- "Tribal"
dla$Agency_Jurisdiction[grepl("Secret Service", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("DHS", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("Public Safety", dla$Agency_Authority, ignore.case = TRUE)] <- "State"



#  TEST FOR MISSING VALUYES IN Jurisdiction Show NAs #####################
#agency.jurisdiction.test <-
 # dla %>%
  #select(Station.Name, Agency_Authority, Agency_Jurisdiction)  %>%
  #filter(is.na(Agency_Jurisdiction))
#View(distinct(agency.jurisdiction.test))

# ################################ ADD METADATA: State Regions ###################################################

# Import State Regions and subregions
state.regions <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033 Categories - State Regions.csv", stringsAsFactors=FALSE)
colnames(state.regions)


#Join 
dla <- left_join(dla, state.regions)

#### End Here #######

