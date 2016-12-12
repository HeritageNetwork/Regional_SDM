# File: 1b_removeCoincidentBackgroundPts.r
# Purpose: remove random background points that are within polygons (and a 30 m buffer)
# of known locations (EO polygons)

library(rgdal)
library(sp)
library(rgeos)

# set up paths ----
### This is the location and shapefile that has your species polygon data. 
polydir <- "K:/Reg5Modeling_Project/inputs/species/glypmuhl/polygon_data"
polyFileName <- "glypmuhl_expl.shp"
setwd(polydir)

### This is the background random points shapefile info
ranptsFolder <- "K:/Reg5Modeling_Project/inputs/background"
ranptsShp <- "sdmclpbnd_20160831_buffNeg1000_att_Reg5"

# load data ----
# get the poly shapefile
shpName <- strsplit(polyFileName,"\\.")[[1]][[1]]
polyShapef <- readOGR(dsn=polydir, layer = shpName) #Z-dimension discarded msg is OK

# get the background shapefile
backgShapef <- readOGR(dsn=ranptsFolder, layer=ranptsShp)

#get projection info for later
projInfo <- backgShapef@proj4string

# find coincident points ----
#buffer the poly shapefile 30 m
polybuff <- gBuffer(polyShapef, width = 30)

# find points that fall within the buffered polygons, subset the sp object
coincidentPts <- gContains(polybuff, backgShapef, byid = TRUE)
colnames(coincidentPts) <- "insideBuff"
backgShapef@data <- cbind(backgShapef@data, coincidentPts)
backgSubset <- backgShapef[backgShapef@data$insideBuff == FALSE,]

# projection info doesn't stick, apply from what we grabbed earlier
backgSubset@proj4string <- projInfo

# write it out ---
outFileName <- paste(ranptsShp, "_clean", sep="")
writeOGR(backgSubset, dsn = ranptsFolder, layer = outFileName, 
         driver="ESRI Shapefile", overwrite_layer=TRUE)

## clean up ----
# remove all objects before moving on to the next script
rm(list=ls())
