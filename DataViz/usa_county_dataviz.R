library(dplyr)
library(ggvis)
library(ggplot2)
library(rgdal)

# setwd("~/GitHub/data-1033-program/DataSets")
data <- read.csv("MAIN_1033_Transaction Dataset, Updated 06-20-2015, 2020.csv", header = TRUE)

# Followed this blog post
# http://www.r-bloggers.com/making-static-interactive-maps-with-ggvis-using-ggvis-maps-wshiny/

usa <- readOGR("ggvis-maps/data/us.geojson", "OGRGeoJSON")

maine <- readOGR("data/maine.geojson", "OGRGeoJSON")

map <- ggplot2::fortify(maine, region="name")

map %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=400, height=600, keep_aspect=TRUE)