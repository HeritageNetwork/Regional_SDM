# File: preproc_groupPoints.r
# Purpose: If we are working with observation data (not EO data), at least in part
#  then the observations need to be grouped spatially. This script should help do that.
#  

library(here)
library(sf)

## set paths ----
# set this path to the folder where point data reside
pathTodata <- here("_data","inputs")

pts <- read_sf(paste(pathTodata,"bombus_test.shp", sep = "/"))

pts$key <- as.character(pts$key)
pts$year <- as.character(pts$year)

# ### version one. using clustering ----
# # point to point distances for all points
# ptdist <- st_distance(x = pts, y = pts, by_element = FALSE)
# hc <- hclust(as.dist(ptdist), method = "complete")
# #define the distance threshold
# d = 10000
# pts$clust <- cutree(hc, h = d)
# # pts$clust <- cutree(hc, k = 5) #this works well (defining number of groups instead of threshold)
# # this figure indicates the points don't "chain" so there are 
# # still groups that separate but are within the threshold
# plot(pts["clust"])


### version two. use buffering ----

# convert to albers equal area (need meters as distance unit)
pts.aea <- st_transform(pts, 102003)

#define the buffer distance
# since this is radius, the separation distance is double this value
d = 10000
# buffer the points
pt.buff <- st_buffer(pts.aea, dist = d)
#plot(pt.buff["clust"])  
# merge up the polys, then explode to single-part
pt.un <- st_union(pt.buff, by_feature = FALSE)
pt.un.sing <- st_cast(pt.un, "POLYGON")

# assign an integer ID to the original points layer
pts.aea$group <- as.integer(st_intersects(pts.aea, pt.un.sing, sparse = TRUE))

#plot(pts.aea["group"])

st_write(pts.aea, paste(pathTodata,"bombus_test_grouped.shp", sep = "/"))


## clean up ----
# remove all objects before using another script
rm(list=ls())