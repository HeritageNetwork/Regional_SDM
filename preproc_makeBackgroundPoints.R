# File: preproc_makeBackgroundPoints.r
# Purpose: GRTS sampling of study area polygon to generate background random points

library(spsurvey)
library(rgdal)

# This is the directory that has your study area polygon.
setwd("D:/RegionalSDM/inputs/background")

# the name of the study area polygon
StudyAreaPoly <- "clpBnd_SDM.shp"

# read in the shapefile, get the attribute data
layer <- strsplit(StudyAreaPoly,"\\.")[[1]][[1]]
shapef <- readOGR(StudyAreaPoly, layer = layer)
att.pt <- shapef@data

#here are some changes to the master
# and here are some more

#get projection info for later
projInfo <- shapef@proj4string

# name of random points output shapefile
nm.RanPtFile <- paste(layer, "_RanPts", sep = "")

# Enter the number of random points you want to generate 
numpts <- 500000

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
			
# remove extranneous fields, write it out
ranPts <- as(grtsResult, "SpatialPointsDataFrame")
colsToKeep <- c("stratum")
ranPts <- ranPts[,colsToKeep]

# apply projection info
ranPts@proj4string <- projInfo

writeOGR(ranPts, dsn = ".", layer = nm.RanPtFile, driver="ESRI Shapefile", overwrite_layer=TRUE)
