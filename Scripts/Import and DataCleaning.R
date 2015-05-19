
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)
library(ggvis)


################################################################
#IMPORT and Data Wrangling
################################################################

# Import DLA dataset
dla.orig <- read.csv("~/Documents/R Programming/data-1033-program/DataSets/1033_DLA_data_as_of_march_2015.csv", stringsAsFactors=FALSE)

# Rename and turn into dataframe
dla <- tbl_df(dla.orig)

# Add UPDATED column that shows shows date of download
dla$Updated_On <- 03-15-2015

dla


# Add COST column that calculates total cost of each transaction (Quantity * Acquisition Value)

#- Convert Acquisition.Value to numeric
dla$Acquisition.Value <- as.numeric(dla$Acquisition.Value )
# - Add column
dla<- mutate(dla, Calculated.Cost = Acquisition.Value * Quantity)

# Split Address Street Address Number from the Rest
#   "str_split_fidlaed" does the splitting, but we need to put it into a dataframe
#    split.address <- as.data.frame(str_split_fidlaed(dla$Station.Address, " ", 2))

# Rename the dataframe columns
# colnames(split.address) <- c("Station.Address_Numeric.Only", "Station.Address_Street.Only")

# Convert to dframe for convenience
# split.address <- tbl_df(split.address)

# Add the new columns to the main dataset, dla
# dla$Station.Address_Numeric.Only <- split.address$Station.Address_Numeric.Only
# dla$Station.Address_Street.Only <- split.address$Station.Address_Street.Only

# -----------------------------------------------------------
# Change City Names from Factor to Character
# -----------------------------------------------------------

dla$Station.City <- as.character(dla$Station.City)

# -----------------------------------------------------------
# Problem: Dates and currency are factors
# -----------------------------------------------------------

# The following cols is currency, entered as characters
# select(dla, Acquisition.Value)


# Convert dollars to numeric
# Remove Commas and dollar signs from Cost Acquistion
dla$Acquisition.Value <- gsub('\\$|,', '', dla$Acquisition.Value)
# View(select(dla, Acquisition.Value))
# class(dla$Acquisition.Value)


# Convert dollars to numbers
#Acquisition Value
dla$Acquisition.Value <- as.numeric(dla$Acquisition.Value)
class(dla$Acquisition.Value)


# Problem: time of day is included....it means nothing
# - remove time of day...but how? Not all are same time...
# otherwise this would work: dla$Ship.Date <- gsub(" 12:00:00 AM", "", dla$Ship.Date)

# How about removing rightmost 12 digits with str_sub and negative numbers?

t <- dla$Ship.Date
str_sub(t, -12, -1) <- ""


# Then spell out each month

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

# Then convert the dates
t <- mdy(t)

# reassign 
dla$Ship.Date <- t 



################################################################
IMPORT and Data Wrangling
################################################################
# Rename some Columns

colnames(dla)[4]<- "Station.Name"
colnames(dla)[13]<- "NSN"

# ------------ State Names --------------------- #
# The original dataset doesn't list full state names. Import and add state names table.

# Import State Names
y<- read.csv("~/Documents/R Programming/1033/us_states abbreviations.csv", header=FALSE, stringsAsFactors=FALSE)
y

# DATAWRANGLE STATE NAMES #


# In new dataset, convert to dataframe
y<- tbl_df(y)  

# In new dataset, assign  Column Names, remove edlatra column

colnames(y)[2]<- "state.proper"
colnames(y)[3]<- "State"
y <- y[2:3]


# JOIN: Add State names to main dataset
# left join 
dla<- left_join(dla,y)


# Remove upper case State names

# rename a column
colnames(dla)[25]<- "State.Name"
grep("state.proper",colnames(dla), ignore.case=T)
dla[3]<-NULL
#dla[25]<-NULL


# ADD NEW DATA: FSG and FSGC Categories ######################################

# New Metadata is associated with FSG and FSGC codes, so they must 
# be edlatracted from NSN in main dataset

# Create new column FSG: Edlatract FSG codes to new column and convert to numeric
dla["FSG"]<-substr(dla$NSN, 1,2)
dla$FSG<- as.integer(dla$FSG)
class(dla$FSG)

# In main dataset Edlatract FSGC codes to new column and convert to numeric
dla["FSGC"]<-substr(dla$NSN,1,4)
dla$FSGC<- as.integer(dla$FSGC)
class(dla$FSGC)

#-------Import Metadata based on FSGC Codes...WATCH THIS NEdlaT PART ------#

#Import new data and convert to dataframe
FSGC.codes <- read.csv("~/Documents/R Programming/1033/1033 Categories - FSGC Codes.csv")
FSGC.codes <- tbl_df(FSGC.codes)
y <- FSGC.codes 

# DATAWRANGLING: convert FSGC in MAIN dataset to factors
class(dla$FSGC)
dla$FSGC <- as.numeric(dla$FSGC)
class(dla$FSGC)

#Join the the tables
dla<- left_join(dla,y)

#Import based on FSG categories I came up with 
#Import dataset
FSG.categories <- read.csv("~/Documents/R Programming/1033/1033 FSG Categories_CTM Version.csv")

# Clean the data
str(FSG.categories)

# This has some edlatra blank columns. Delete them
FSG.categories <- select(FSG.categories, c(FSG, FSG.title, FSG.categorized))
View(FSG.categories)

# Convert to dframe
FSG.categories <- tbl_df(FSG.categories)
class(FSG.categories$FSG)

# Convert FSG to integer before importing new dataset
dla$FSG<- as.integer(dla$FSG)

# Merge FSG categories with dla 
dla<- left_join(dla, FSG.categories)


# ADD METADATA ###################################################
# Metadata includes Agency_Authority (type); Agency_Jurisdiction (government);
# (cont.)...Geographic Regions and SubRegions


# Create Agency Authority Column
dla$Agency_Authority <- NA
View(dla)
names(dla)

#Assign Metadata

# Tag "Agency Authority" with my own categories, based on "Agency Name"
dla$Agency_Authority[grepl("police", dla$Station.Name, ignore.case=TRUE)] <- "Police" 


# Useful view in lieu of filter
# View(dla[grepl("irs", dla$Station.Name, ignore.case=TRUE),])
View(dla[grepl("alcohol", dla$Station.Name, ignore.case=TRUE)])

View(flights.location[grepl("Tedlaas", flights.location$name),])

dla$Agency_Authority[grepl(" pd", dla$Station.Name, ignore.case=TRUE)]


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


dla$Agency_Authority[grepl("marshal", dla$Station.Name, ignore.case=TRUE)] <- "Marshals" 
dla$Agency_Authority[grepl("safety", dla$Station.Name, ignore.case=TRUE)] <- "Public Safety"
dla$Agency_Authority[grepl("DEPT OF PUB SAF", dla$Station.Name, ignore.case=TRUE)] <- "Public Safety"
dla$Agency_Authority[grepl("DEPT OF PUBLIC SAFETY", dla$Station.Name, ignore.case=TRUE)] <- "Public Safety"
View(dla)

dla$Agency_Authority[grepl("pardons", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("tobacco", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"


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



dla$Agency_Authority[grepl("constable", dla$Station.Name, ignore.case=TRUE)] <- "Constable"

dla$Agency_Authority[grepl("COUNTY DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
#dla$Agency_Authority[grepl("DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("ATTORNEY", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("CO LAW ENF", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("ATTY GEN", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("PROSECUTOR", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("DA'S OFFICE", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("LOS ANGELES DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"
dla$Agency_Authority[grepl("TN 9TH JUDICIAL DISTRICT DA", dla$Station.Name, ignore.case=TRUE)] <- "District Attorney"


dla$Agency_Authority[grepl("Arson", dla$Station.Name, ignore.case=TRUE)] <- "Fire Dept"
dla$Agency_Authority[grepl("Fire", dla$Station.Name, ignore.case=TRUE)] <- "Fire Dept"
dla$Agency_Authority[grepl("airport", dla$Station.Name, ignore.case=TRUE)] <- "Airports"


dla$Agency_Authority[grepl("coroner", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
dla$Agency_Authority[grepl("animal", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
dla$Agency_Authority[grepl("agriculture", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("Sergeant", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("RIVER AND BAY", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("MOTOR VEHICLE", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"
dla$Agency_Authority[grepl("IL DEPT OF CENTRAL MANAGEMENT", dla$Station.Name, ignore.case=TRUE)] <- "Other State Agencies (Non Security)"



dla$Agency_Authority[grepl("PORT DIST HAR", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"
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



dla$Agency_Authority[grepl("highway", dla$Station.Name, ignore.case=TRUE)] <- "Highway Patrol"
dla$Agency_Authority[grepl("CHIP", dla$Station.Name, ignore.case=FALSE)] <- "Highway Patrol"
dla$Agency_Authority[grepl("STATE PATROL", dla$Station.Name, ignore.case=FALSE)] <- "Highway Patrol"


dla$Agency_Authority[grepl("college", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$Agency_Authority[grepl("university", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$Agency_Authority[grepl("UNIV", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"
dla$Agency_Authority[grepl("SCH DIST PD", dla$Station.Name, ignore.case=TRUE)] <- "Colleges, Universities"



dla$Agency_Authority[grepl("academy", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"
dla$Agency_Authority[grepl("TNG CTR", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"
dla$Agency_Authority[grepl("TNG CENTER", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"
dla$Agency_Authority[grepl("TRNG CTR", dla$Station.Name, ignore.case=TRUE)] <- "Training Academies"


dla$Agency_Authority[grepl("county", dla$Station.Name, ignore.case=TRUE)] <- "County"

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

# Search Edlaperiments
other <-  c("BUREAU OF INVESTIGATIO","livestock","DEPT OF MGMT SVC")
dla[grep(paste(other, collapse='|'), dla$Station.Name, ignore.case=TRUE), ]


dla$Agency_Authority[grepl("METRO", dla$Station.Name, ignore.case=TRUE)] <- "Local Services"
dla$Agency_Authority[grepl("YOUTH AFFAIRS", dla$Station.Name, ignore.case=TRUE)] <- "Local Services"


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




#---------------------------
#agency.authority.test <-
# dla %>%
# select(Station.Name, Agency_Authority, Agency_Jurisdiction)  %>%
# filter(is.na(Agency_Authority))
#View(distinct(agency.authority.test))


# ################################ ADD METADATA ###################################################
# Tag "Agency Jurisdiction" with my own categories, based on "Agency Name"


### Import new dataset Agency Jurisdiction/Agency Authority dataset

# Read
tbl.agency.authority <- read.csv("~/Documents/R Programming/1033/1033 Categories - Agency Authority.csv")

# Make sure join columns are both factors
dla$Agency_Authority
dla$Agency_Authority<- as.factor(dla$Agency_Authority)
class(dla$Agency_Authority)

# JOIN Agency Jurisdiction
dla<- left_join(dla,tbl.agency.authority)
View(dla)

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
dla$Agency_Jurisdiction[grepl("indian", dla$Agency_Authority, ignore.case = TRUE)] <- "Indian"
dla$Agency_Jurisdiction[grepl("Secret Service", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("DHS", dla$Agency_Authority, ignore.case = TRUE)] <- "Federal"
dla$Agency_Jurisdiction[grepl("Public Safety", dla$Agency_Authority, ignore.case = TRUE)] <- "State"


#Whoops...forogt to add a factor, "Indian"
# Convert column to character, add "Indian," then convert back to factor
dla$Agency_Jurisdiction <- as.character(dla$Agency_Jurisdiction)
dla$Agency_Jurisdiction[grepl("indian", dla$Agency_Authority, ignore.case = TRUE)]<-"Indian"
dla$Agency_Jurisdiction <- as.factor(dla$Agency_Jurisdiction)
class(dla$Agency_Jurisdiction)


# Create TEST FOR MISSING VALUYES IN Jurisdiction Show NAs #####################
agency.jurisdiction.test <-
  dla %>%
  select(Station.Name, Agency_Authority, Agency_Jurisdiction)  %>%
  filter(is.na(Agency_Jurisdiction))
View(distinct(agency.jurisdiction.test))


# Import State Regions and subregions
state.regions <- read.csv("~/Documents/R Programming/1033/1033 Categories - State Regions.csv")


#rename State_Abreviation by subsetting
colnames(state.regions)[1]<- "State"
names(state.regions)[1]

#Join 
dla <- left_join(dla, state.regions)


###########################################################################
# Add Congressional Districts
###########################################################################

# Import Data
`1033_Program_Congressional_District` <- read.csv("~/Documents/R Programming/1033/1033_Program_Congressional_District.csv", stringsAsFactors=FALSE)

# Rename
congress <- `1033_Program_Congressional_District`

# alter colnames, select relevant columns
colnames(congress)[7] <- "Station.Name"

# Use only unique values
congress <- distinct(select(congress, Station.Name, CDFP))

# Merge
dla <- left_join(dla, congress)


#EdlaPLORE SAN DIEGO
# Pivot:  San Diego
dla %>%
  filter(Station.City == "SAN DIEGO", !grepl("Federal|Indian", Agency_Jurisdiction)) %>%
  group_by(Station.Zip.Code, CDFP, Agency_Jurisdiction) %>%
  summarise_each(funs(sum), Number.Of.Officers, Calculated.Cost, Quantity) 




dla %>%
  group_by(Station.Name, Station.Zip.Code, CDFP) %>%
  summarise(sum(Number.Of.Officers)) %>%
  filter(Station.Zip.Code == 92101)

View(distinct(select(dla,Agency_Jurisdiction)))



#agency.authority.test <-
dla %>%
  select(Station.City, Station.Name, Agency_Authority, Agency_Jurisdiction)  %>%
  filter(Station.City == "SAN DIEGO", is.na(Agency_Jurisdiction))



###########################################################################
Analysis:Pivots
###########################################################################



# Use Count show total transactions for each state
count(dla, State)


# Show top 5 states with most transactions
dla %>% 
  count(State, sort=TRUE) %>%
  top_n(5)

#Group by State, summarize by total edlapenditures (Acquisition Value * Quantity)
dla %>%
  group_by(State) %>%
  summarise(total.edlapenditures=sum(Acquisition.Value*Quantity)) %>%
  top_n(5) %>%
  arrange(desc(total.edlapenditures))

dla$Agency_Authority[grepl("coroner", dla$Station.Name, ignore.case=TRUE)] <- "Other County Agencies (Non Security)"


#Show Baltimore Guns
t <- dla %>%
  group_by(State, Station.City, Station.Name, FSGC_Title) %>%
  tally() %>%
  filter(Station.City == "BALTIMORE")
View(t)

class(dla$Station.City)


dla %>%
  group_by(State) %>%
  summarise(sum(Calculated.Cost)) %>%
  top_n(5)

# Show 10 States with highest edlapenditures
dla %>%
  group_by(State )%>%
  summarise(total.edlapenditures=sum(Acquisition.Value*Quantity)) %>%
  top_n(10) %>%
  arrange(desc(total.edlapenditures))


# Show top spender in each state, by among agency authority 

dla %>%
  group_by(State, Agency_Authority )%>%
  summarise(total.edlapenditures=sum(Acquisition.Value*Quantity)) %>%
  top_n(1) %>%
  arrange(desc(total.edlapenditures))


# Show 10 states that spent the most on guns
# Here are the factor for FSGC_Title, High Caliber weapons 
#[261] "Guns 75mm through 125mm"                         
#[262] "Guns over 125mm through 150mm"                   
#[263] "Guns over 150mm through 200mm"                   
#[264] "Guns over 200mm through 300mm"                   
#[265] "Guns over 300mm"                                 
#[266] "Guns over 30mm up to 75mm"                       
#[267] "Guns through 30mm"   

#Filter by FSGC_Title factor levels. Slick!
View( dla %>%
        group_by( State, Agency.Name,FSGC_Title ) %>%
        summarise(agency.edlapenditures =sum(Acquisition.Value * Quantity), n= n() )%>%
        filter(FSGC_Title %in% levels(dla$FSGC_Title)[261:267]) %>%
        arrange(desc(agency.edlapenditures))
)

#Show Trussville PD
View(dla %>%
       filter(Agency.Name=="TRUSSVILLE POLICE DEPT") %>%
       filter(FSGC_Title %in% levels(dla$FSGC_Title)[261:267]))


# Show agencies that have high caliber weapons in CA
dla %>%
  group_by(Agency.Name, FSGC_Title) %>%
  filter(State=="CA", FSGC_Title %in% levels(dla$FSGC_Title)[261:265] )
filter(dla, FSGC_Title %in% levels(dla$FSGC_Title)[262])

# ATF By State
dla %>%
  group_by(State, Agency_Authority) %>%
  summarise(sum.agency.authority=sum(Cost))%>%
  filter(grepl("ATF", Agency_Authority), State =="AK")

# Public safety in all states(show first 100 rows)
dla %>%
  group_by(State,Agency.Name) %>%
  summarise(agency.edlapenditures = sum(Cost))%>%
  print(n=100)

# High Caliber weapons going to Alaska agencies (FSG.categorized)
dla %>%
  filter(State=="AK") %>%
  group_by(Agency.Name, FSG.categorized) %>%
  summarise(FSG.categorized.cost=sum(Cost))

# How weapons by caliber
class(dla$FSGC_Title)

levels(dla$FSGC_Title)[261:267]


# Here are the factor for FSG_Title, High Caliber weapons 
[261] "Guns 75mm through 125mm"                         
[262] "Guns over 125mm through 150mm"                   
[263] "Guns over 150mm through 200mm"                   
[264] "Guns over 200mm through 300mm"                   
[265] "Guns over 300mm"                                 
[266] "Guns over 30mm up to 75mm"                       
[267] "Guns through 30mm"   

#dla$FSGC_Title[261:267]
#sub <- d[d$dla1 %in% dla1 & d$dla2 %in% dla2,]

# here are the factor indedla numbers
# high.caliber <- c(261:267)


#Show Guns of caliber 75-125mm
dla %>%
  group_by(State, FSGC_Title) %>%
  summarise(agency.edlapenditures =sum(Cost)) %>%
  filter(FSGC_Title %in% levels(dla$FSGC_Title)[266])

#Show toal guns of all calibers, across all states
dla %>%
  group_by(State, FSGC_Title) %>%
  summarise(agency.edlapenditures =sum(Cost), n= n() )%>%
  filter(FSGC_Title %in% levels(dla$FSGC_Title)[261:267])


#Show total number of guns, by state and caliber
dla %>%
  group_by(State, FSGC_Title) %>%
  summarise(n=n()) %>%
  filter(FSGC_Title %in% levels(dla$FSGC_Title)[261:267])

# Show all guns, by state
t<- filter(dla,State=="AL", FSGC_Title %in% levels(dla$FSGC_Title)[261:267])
View(t)


#Show gun calibers by Agency Jurisdiction
dla %>%
  group_by(State, Agency_Authority, FSGC_Title) %>%
  summarise(n=n()) %>%
  filter(FSGC_Title %in% levels(dla$FSGC_Title)[261:267])
