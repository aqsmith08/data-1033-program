library(dplyr)
library(ggvis)
library(ggplot2)
library(rgdal)
library(maptools)

# Used this blog post as a guide
# http://www.r-bloggers.com/making-static-interactive-maps-with-ggvis-using-ggvis-maps-wshiny/

# setwd("~/GitHub/data-1033-program/DataViz"
usa <- readOGR("us.geojson", "OGRGeoJSON")
map <- ggplot2::fortify(usa, region="GEOID")

# No idea what any of this means
usa <- usa[!usa$STATEFP %in% c("02", "15", "72"),]
us_aea <- spTransform(usa, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

# Once I tried this, I received an error
map <- ggplot2::fortify(us_aea, region="GEOID")

# Error: isTRUE(gpclibPermitStatus()) is not TRUE
# After doing a bunch of researching and trying to install the gpclib package, I gave up.
# Le sigh.

# setwd("~/GitHub/data-1033-program/DataSets")
data <- read.csv("MAIN_1033_Transaction Dataset, Updated 06-20-2015, 2020.csv", header = TRUE)