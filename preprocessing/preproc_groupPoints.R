# File: preproc_groupPoints.r
# Purpose: If we are working with observation data (not EO data), at least in part
#  then the observations need to be grouped spatially. This script should help do that.
#  

library(here)
library(sf)

library(stringr)
#library(nhSDM)

## set paths ----
# set this path to the folder where point data reside
pathTodata <- here("_data","inputs")

fileName <- "ma_test_points.shp"
fileNameShort <- str_split(fileName, "[.]")[[1]][1]

pts <- st_read(paste(pathTodata, fileName, sep = "/"))

pts$key <- as.character(pts$key)
pts$year <- as.character(pts$year)

# convert to albers equal area
pts.aea <- st_transform(pts, 102003)

### Use buffering ----
# define the buffer distance
# since this is the radius, the separation distance is double this value
d = 5000
# buffer the points
pt.buff <- st_buffer(pts.aea, dist = d)
# plot(pt.buff["clust"])
# merge up the polys, then explode to single-part
pt.un <- st_union(pt.buff, by_feature = FALSE)
pt.un.sing <- st_cast(pt.un, "POLYGON")
# assign an integer ID to the original points layer
pts.aea$group <- as.integer(st_intersects(pts.aea, pt.un.sing, sparse = TRUE))

#plot(pts.aea["group"])

st_write(pts.aea, paste(pathTodata,"/", fileNameShort, "_grouped.shp", sep = ""))


## clean up ----
# remove all objects before using another script
rm(list=ls())