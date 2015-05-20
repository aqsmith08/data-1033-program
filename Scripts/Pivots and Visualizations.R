library(dplyr)
library(dplyrExtras)
library(ggplot2)
library(ggvis)
library(stringr)

# ----------------------------------------------------
# Q: What are the states with most transactions (top 5)?
## Method 1: Each row is a transaction
  top.5.transactions <- dla %>% 
    count(State, sort=TRUE) %>%
    top_n(5) %>% View()

# Note: ggvis doesn't like being piped from calculations! Use new, assigned variable.
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


