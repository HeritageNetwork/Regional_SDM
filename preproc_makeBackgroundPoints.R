# File: preproc_makeBackgroundPoints.r
# Purpose: GRTS sampling of study area polygon to generate background random points

library(spsurvey)
#library(sp)
library(rgdal)

# This is the directory that has your study area polygon.
setwd("G:/SDM_test/")

# the name of the study area polygon
fileName <- "testArea.shp"

# read in the shapefile, get the attribute data
layer <- strsplit(fileName,"\\.")[[1]][[1]]
shapef <- readOGR(fileName, layer = layer)
att.pt <- shapef@data

# name of random points output shapefile
nm.RanPtFile <- paste(layer, "_RanPts", sep = "")

# Enter the number of random points you want to generate 
numpts <- 1000

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
#fullName <- paste(nm.RanPtFile,".shp",sep="")
colsToKeep <- c("stratum")
ranPts <- ranPts[,colsToKeep]
writeOGR(ranPts, dsn = ".", layer = nm.RanPtFile, driver="ESRI Shapefile", overwrite_layer=TRUE)
