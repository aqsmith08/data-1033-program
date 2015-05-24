library(dplyr)
library(dplyrExtras)
library(ggplot2)
library(ggvis)
library(stringr)

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
## Method 1: Each row is a transaction
  top.5.transactions <- dla %>% 
    count(State, sort=TRUE) %>%
    top_n(5)

# Note: ggvis doesn't like being piped from calculations! Use new, assigned variable.
ggvis(top.5.transactions, ~State, ~n)
# HELP: Format numbers on y axis? Other formatting....

# Show 10 States with highest expenditures
dla %>%
  group_by(State )%>%
  summarise(Cost = sum(Calculated.Cost)) %>%
  top_n(10) %>%
  arrange(desc(Cost))

# ----------------------------------------------------
# Q: What years saw the most transactions? What states were responsible?



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

aps
