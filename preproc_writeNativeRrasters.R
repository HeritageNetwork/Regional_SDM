# File: preproc_writeNativeRrasters.r
# Purpose: This script will take a folder of geoTiffs and 
# write them out in native R raster format

library(raster)
library(rgdal)

# set this path to the folder where the environmental rasters reside
# it can have other files, but only *.tifs that you want to write to the R format

pathToTifs <- "K:/Reg5Modeling_Project/inputs/env_vars/geotiffs"
setwd(pathToTifs)

# set the output path
outPath <- "K:/Reg5Modeling_Project/inputs/env_vars/nativeR"

# get a list of the grids
tiflist <- list.files(pattern = ".tif$")

# already got some written to R format? Check, then subset
# skip these rows if you want to do a full overwrite of all
doneraslist <- list.files(path = outPath, pattern = ".grd$")
# lists without file extension
tiflistNoExt <- gsub(".tif$","",tiflist)
raslistNoExt <- gsub(".grd$","",doneraslist)
finalTifList <- tiflist[!tiflistNoExt %in% raslistNoExt]
tiflist <- finalTifList

# loop through each tiff in the list, write it out in native R format
for(tif in tiflist){
  print(paste("working on", tif, sep = " "))
	x <- raster(tif)
	nm <- substr(tif,1,nchar(tif) - 4)
	writeRaster(x, filename = paste(outPath,"/",nm,sep=""), format = "raster", overwrite=TRUE)
	}

## clean up ----
# remove all objects before using another script
rm(list=ls())