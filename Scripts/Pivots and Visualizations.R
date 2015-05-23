library(dplyr)
library(dplyrExtras)
library(ggplot2)
library(ggvis)
library(stringr)

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
dla %>%
  group_by(State )%>%
  summarise(Cost = sum(Calculated.Cost)) %>%
  top_n(10) %>%
  arrange(desc(Cost))

# ----------------------------------------------------
# Q: What years saw the most transactions? What states were responsible?

# ----------------------------------------------------
# Q: Categories of Equipment: Show one category of equipment (guns) compared to all others




