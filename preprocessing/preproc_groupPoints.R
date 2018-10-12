# File: preproc_groupPoints.r
# Purpose: If we are working with observation data (not EO data), at least in part
#  then the observations need to be grouped spatially. This script should help do that.
#  

library(here)
library(sf)

library(nhSDM)

## set paths ----
# set this path to the folder where point data reside
pathTodata <- here("_data","inputs")

pts <- st_read(paste(pathTodata,"ma_test_points.shp", sep = "/"))

pts <- st_read(paste(pathTodata,"MoBI_SpeciesDataForTesting.shp", sep = "/"))

pts$key <- as.character(pts$key)
pts$year <- as.character(pts$year)

# convert to albers equal area
pts.aea <- st_transform(pts, 102003)

# ### version one. using clustering ----
# # point to point distances for all points
tic("cluster method")
ptdist <- st_distance(x = pts.aea, y = pts.aea, by_element = FALSE)
hc <- hclust(as.dist(ptdist), method = "complete")
#define the distance threshold
d = 10000
pts.aea$clust <- cutree(hc, h = d)
toc()
# # pts$clust <- cutree(hc, k = 5) #this works well (defining number of groups instead of threshold)
# # this figure indicates the points don't "chain" so there are 
# # still groups that separate but are within the threshold
# plot(pts["clust"])


### version two. use buffering ----
# define the buffer distance
# since this is radius, the separation distance is double this value
tic("buffer method")
d = 5000
# buffer the points
pt.buff <- st_buffer(pts.aea, dist = d)
#plot(pt.buff["clust"])
# merge up the polys, then explode to single-part
pt.un <- st_union(pt.buff, by_feature = FALSE)
pt.un.sing <- st_cast(pt.un, "POLYGON")
# assign an integer ID to the original points layer
pts.aea$group <- as.integer(st_intersects(pts.aea, pt.un.sing, sparse = TRUE))
toc()
#plot(pts.aea["group"])

### version three. use nhSDM ----
#define the distance
tic("nhSDM method")
d = 10000
ptout <- nh_group(pts.aea, sep.dist = d, union = FALSE )
toc()

plot(ptout["group"])

st_write(ptout, paste(pathTodata,"ma_test_grouped.shp", sep = "/"))


## clean up ----
# remove all objects before using another script
rm(list=ls())