# File: preproc_makeBackgroundPoints.r
# Purpose: GRTS sampling of study area polygon to generate background random points

library(spsurvey)
library(rgdal)

## set paths ----
# This is the directory that has your study area polygon.
setwd("E:/Reg5Modeling_Project/other_spatial")


# the name of the study area polygon
StudyAreaPoly <- "sdmclpbnd_20160831_buffNeg1000.shp"

# read in the shapefile, get the attribute data
layer <- strsplit(StudyAreaPoly,"\\.")[[1]][[1]]
shapef <- readOGR(StudyAreaPoly, layer = layer)
att.pt <- shapef@data

#get projection info for later
projInfo <- shapef@proj4string

# name of random points output shapefile
nm.RanPtFile <- paste(layer, "_RanPts", sep = "")

## set up and run GRTS ----
# Enter the number of random points you want to generate 
# total area of entire study area is about 1,064,000 km^2
# if our target is about 1 pt / 20 km^2, that comes out to about 53,000 points. 
numpts <- 53000

# Create the design list
dsgn <- list(None=list(panel=c(Panel=numpts), seltype="Equal"))

# Call the grts function
grtsResult <- grts(design=dsgn,
			src.frame="shapefile",
			in.shape=layer,
			att.frame=att.pt,
			type.frame="area",
			DesignID="bkgrndRanPts",
			prjfilename=layer,
			out.shape=nm.RanPtFile)
			
# remove extranneous fields, write it out ----
ranPts <- as(grtsResult, "SpatialPointsDataFrame")
colsToKeep <- c("stratum")
ranPts <- ranPts[,colsToKeep]

# apply projection info
ranPts@proj4string <- projInfo
writeOGR(ranPts, dsn = ".", layer = nm.RanPtFile, driver="ESRI Shapefile", overwrite_layer=TRUE)

## clean up ----
# remove all objects before using another script
rm(list=ls())
