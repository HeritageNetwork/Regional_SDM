# File: preproc_attributeBackgroundPts.r
# Purpose: attribute environmental data to random background points

## start with a fresh workspace with no objects loaded
library(raster)
library(rgdal)

## set paths ----
pathToRas <- "D:/RegionalSDM/env_vars/geotiffs"
pathToPts <- "D:/RegionalSDM/inputs/background"

## create a stack ----
setwd(pathToRas)
raslist <- list.files(pattern = ".tif$")
gridlist <- as.list(paste(pathToRas,raslist,sep = "/"))
nm <- substr(raslist,1,nchar(raslist) - 4)
names(gridlist) <- nm
envStack <- stack(gridlist)

## Get random points file ----
setwd(pathToPts)
ranPtsFile <- "clpBnd_SDM_RanPts_clean.shp"
ranPtsFileNoExt <- sub(".shp","",ranPtsFile)
# Read these files into a list of SpatialPoints dataframes
shpf <- readOGR(".", layer = ranPtsFileNoExt)
  
# Get a list of the codes (this assumes all the input files had '_RanPts.shp' that shall be stripped)
code_name <- substr(ranPtsFile,1,(nchar(ranPtsFile)-11))

# do it, write it ----
x <- extract(envStack, shpf, method="simple", sp=TRUE)
filename <- paste(code_name, "_att", sep="")
writeOGR(x, pathToPts, layer=paste(filename), driver="ESRI Shapefile", overwrite_layer=TRUE)

## clean up ----
# remove all objects before using another script
rm(list=ls())
