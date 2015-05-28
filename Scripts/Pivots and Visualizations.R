library(dplyr)
library(dplyrExtras)
library(ggplot2)
library(ggvis)
library(stringr)
library(lubridate)

file <- "1033_DLA_data_as_of_march_2015.csv"

# Data is imported as a data frame
dla <- read.csv(file, stringsAsFactors=FALSE)

# Here are the factor for FSGC_Title, High Caliber weapons 
#[261] "Guns 75mm through 125mm"                         
#[262] "Guns over 125mm through 150mm"                   
#[263] "Guns over 150mm through 200mm"                   
#[264] "Guns over 200mm through 300mm"                   
#[265] "Guns over 300mm"                                 
#[266] "Guns over 30mm up to 75mm"                       
#[267] "Guns through 30mm"   

# ----------------------------------------------------
# Summarise program elements
# ----------------------------------------------------

# Q: How many agencies are participating in 1033 program? (Count unique names)
dla %>% summarise(unique.agencies = n_distinct(Station.Name))

# Q Show participating agencies (Show unique names)
# Good way 
  dla %>% group_by(Station.Name) %>% summarise()

  # Bad way (can't sort/arrange)
    distinct(select(dla, Station.Name))

# What are total expenditures, by Agency_Jurisdiction?
dla %>% group_by(Agency_Jurisdiction) %>% summarise(Cost = sum(Calculated.Cost)) %>% arrange(desc((Cost))) 
  
  # Above, In top 3 states with most expenditures
  dla %>%
    group_by(Agency_Jurisdiction, State)%>%
    summarise(Cost = sum(Calculated.Cost)) %>%
    top_n(3) %>%
    arrange(desc(Cost)) %>% 
    


# ----------------------------------------------------
# Q: What are the states with most transactions (top 5)?
# Method 1: Each row is a transaction
# Assign top five transactions
  
top.5.transactions <- dla %>% 
    count(State, sort=TRUE) %>%
    top_n(5)


# ----------------------------------------------------
# Q: What years saw the most transactions? What states were responsible?



=======
# Create a year column using the lubridate package
dla$real_ship_date <- mdy_hms(dla$Ship.Date)
dla$ship_year <- year(dla$real_ship_date)

top_10_year_transactions <- dla %>%
  count(ship_year, sort = TRUE) %>%
  top_n(10)

View(top_10_year_transactions)

# This code needs work. Shows the largest count of transactions by State and Year.
group_by_state_and_year <- dla %>%
  group_by(State, ship_year)

count_transactions <- summarise(group_by_state_and_year, 
                 count = n())

# I can't figure out why this doesn't sort by count descending.
# Also need to figure out how to limit this to the top 10 states instead of 
# using the filter(count > 2500)
count_transactions %>%
  filter(count > 2500) %>%
  arrange(desc(count)) %>%
  View()
>>>>>>> upstream/master

# ----------------------------------------------------
# Q: Categories of Equipment: Show one category of equipment (guns) compared to all others


# ----------------------------------------------------
# Q: Investigate Alaska public safety (Aaron said multiple addresses)
names(dla)
head(dla)

dla %>%
   group_by(Item.Name) %>%
  filter(Station.Name=="ALASKA DEPT OF PUBLIC SAFETY", Item.Name=="TARGET HOLDING SET,TRAINING") %>%
  summarise(n = n(), Cost = sum(Calculated.Cost))

# Show number of acquisitions by year for Alaska dept of Public Safety
aps<- dla %>% 
  group_by(Year= year(Ship.Date)) %>%
  filter(Station.Name == "ALASKA DEPT OF PUBLIC SAFETY") %>%
  summarise(Transactions =n(), Cost = sum(Calculated.Cost))

  ggvis(aps, ~Year, ~Cost) %>% layer_bars()


