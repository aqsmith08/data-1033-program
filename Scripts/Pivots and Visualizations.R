library(dplyr)
library(dplyrExtras)
library(ggplot2)
library(ggvis)
library(stringr)
library(lubridate)

file <- "1033_DLA_data_as_of_march_2015.csv"

# Data is imported as a data frame
dla <- read.csv(file, stringsAsFactors=FALSE)

# ----------------------------------------------------
# Q: What are the states with most transactions (top 5)?
## Method 1: Each row is a transaction
  top.5.transactions <- dla %>% 
    count(State, sort=TRUE) %>%
    top_n(5) %>% View()

# Note: ggvis doesn't like being piped from calculations! Use new, assigned variable.

# HOW'D YOU CREATE THE top_5 VARIABLE? WE SHOULD ADD THAT BECAUSE I GET AN ERROR WHEN I
# RUN THIS CODE

ggvis(top_5, ~State, ~n)
# HELP: Format numbers on y axis? Other formatting....

# Show 10 States with highest expenditures

# HOW'D YOU CREATE THE Calculated.Cost VARIABLE? WE SHOULD ADD THAT BECAUSE I GET AN ERROR WHEN I
# RUN THIS CODE

dla %>%
  group_by(State )%>%
  summarise(Cost = sum(Calculated.Cost)) %>%
  top_n(10) %>%
  arrange(desc(Cost))

# ----------------------------------------------------
# Q: What years saw the most transactions? What states were responsible?

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

# ----------------------------------------------------
# Q: Categories of Equipment: Show one category of equipment (guns) compared to all others




