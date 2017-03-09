# File: 1c_
# Purpose: remove random background points that are within polygons (and a 30 m buffer)
# of known locations (EO polygons)

library(rgdal)
library(sp)
library(rgeos)

# set up paths ----
### This is the location and shapefile that has your species polygon data. 
polydir <- "G:/SDM_test/ElementData"
polyFileName <- "glypmuhl_expl.shp"
setwd(polydir)

### This is the background random points shapefile info
ranptsFolder <- "G:/SDM_test/background"
ranptsShp <- "testArea_RanPts_clean"

# load data ----
# get the poly shapefile
shpName <- strsplit(polyFileName,"\\.")[[1]][[1]]
polyShapef <- readOGR(dsn=polydir, layer = shpName) #Z-dimension discarded msg is OK

# get the background shapefile
backgShapef <- readOGR(dsn=ranptsFolder, layer=ranptsShp)

#get projection info for later
projInfo <- backgShapef@proj4string

# random subset ----
# how many background points do we want?
desiredBG <- 1000

# these points are already spatially-balanced, per GRTS
# so to maintain this, we actually draw *in order*, based 
# on the siteID column
# Data are already sorted but to be absolutely sure, sort 'em
backgShapef <- backgShapef[order(backgShapef$siteID),]

# get the subset
BG.subset <- backgShapef[1:desiredBG,]

# projection info doesn't stick, apply from what we grabbed earlier
BG.subset@proj4string <- projInfo

# write it out ---
outFileName <- paste(ranptsShp, "_subset", sep="")
writeOGR(BG.subset, dsn = ranptsFolder, layer = outFileName, 
         driver="ESRI Shapefile", overwrite_layer=TRUE)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())



